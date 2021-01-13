module Field = {
  type t = {
    name: string,
    firstRangeStart: int,
    firstRangeEnd: int,
    secondRangeStart: int,
    secondRangeEnd: int,
  };

  exception InvalidField;

  let make = inputStr => {
    let re =
      Js.Re.fromString("([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)");
    let result = re->Js.Re.exec_(inputStr);
    switch (result) {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)->Belt.Array.keepMap(Js.Nullable.toOption);
      {
        name: captures[1],
        firstRangeStart: captures[2]->Belt.Int.fromString->Belt.Option.getExn,
        firstRangeEnd: captures[3]->Belt.Int.fromString->Belt.Option.getExn,
        secondRangeStart: captures[4]->Belt.Int.fromString->Belt.Option.getExn,
        secondRangeEnd: captures[5]->Belt.Int.fromString->Belt.Option.getExn,
      };
    | None => raise(InvalidField)
    };
  };

  let isNumberInRange = (number, start, end_) => {
    number >= start && number <= end_;
  };

  let isTicketInFieldRange = (field, ticket) => {
    switch (
      ticket->isNumberInRange(field.firstRangeStart, field.firstRangeEnd),
      ticket->isNumberInRange(field.secondRangeStart, field.secondRangeEnd),
    ) {
    | (true, _)
    | (_, true) => true
    | _ => false
    };
  };
};

module Fields = {
  type t = array(Field.t);
};

module Tickets = {
  type t = array(int);

  let make = inputStrs => {
    inputStrs
    ->Belt.Array.map(inputStr => {
        inputStr
        ->Js.String2.split(",")
        ->Belt.Array.keepMap(Belt.Int.fromString)
      })
    ->Belt.Array.concatMany;
  };
};

module InvalidTicketScanner = {
  let rec isValidTicket = (ticket, fields) => {
    let result = fields[0]->Field.isTicketInFieldRange(ticket);

    result || fields->Belt.Array.size == 1
      ? result : ticket->isValidTicket(fields->Belt.Array.sliceToEnd(1));
  };

  let find = (fields, tickets) => {
    tickets->Belt.Array.keep(ticket => !ticket->isValidTicket(fields));
  };
};

let data = Node.Fs.readFileSync("input/2020/2020.16.input", `utf8);
let inputStrs = data->Js.String2.split("\n\n");
let fields =
  inputStrs[0]->Js.String2.split("\n")->Belt.Array.map(Field.make);

let myTickets =
  inputStrs[1]
  ->Js.String2.split("\n")
  ->Belt.Array.sliceToEnd(1)
  ->Tickets.make;

let nearByTickets =
  inputStrs[2]
  ->Js.String2.split("\n")
  ->Belt.Array.sliceToEnd(1)
  ->Tickets.make;

// part1
let invalidTickets = fields->InvalidTicketScanner.find(nearByTickets);
Js.log(invalidTickets->Belt.Array.reduce(0, (+)));


// part2
module TicketsGroup = {
  type t = array(int);

  let make = inputStrs => {
    let rows =
      inputStrs->Belt.Array.map(inputStr => {
        inputStr
        ->Js.String2.split(",")
        ->Belt.Array.keepMap(Belt.Int.fromString)
      });
    let groupSize = rows[0]->Belt.Array.size;

    Belt.Array.range(0, groupSize - 1)
    ->Belt.Array.map(index => {rows->Belt.Array.map(row => row[index])});
  };
};

module PositionFinder = {
  type t = {
    remainPositions: array(array(string)),
    mappedPositions: Belt.Map.String.t(int),
  };

  let ablePositions = (ticketGroup, fields: Fields.t) => {
    let ticketSize = ticketGroup->Belt.Array.size;

    fields->Belt.Array.keepMap(field => {
      let validRangeCount =
        ticketGroup
        ->Belt.Array.map(ticket => field->Field.isTicketInFieldRange(ticket))
        ->Belt.Array.keep(x => x)
        ->Belt.Array.size;

      validRangeCount == ticketSize ? Some(field.name) : None;
    });
  };

  let removeExistsPositions = (positions, existPositions) => {
    positions->Belt.Array.keep(position =>
      !existPositions->Belt.Array.some(existPosition => existPosition == position)
    );
  };

  let update = (ablePositions, positions) => {
    let updatedPositions =
      ablePositions->Belt.Array.reduceWithIndex(
        positions, (positions, ablePosition, index) =>
        ablePosition->Belt.Array.size == 1
          ? positions->Belt.Map.String.set(ablePosition[0], index) : positions
      );
    let existPositions = updatedPositions->Belt.Map.String.keysToArray;

    let remainPositions =
      ablePositions->Belt.Array.map(ablePosition =>
        ablePosition->removeExistsPositions(existPositions)
      );

    {remainPositions, mappedPositions: updatedPositions};
  };

  let isEmpty = remainPositions => {
    let size =
      remainPositions
      ->Belt.Array.keep(remainPosition => remainPosition->Belt.Array.size != 0)
      ->Belt.Array.size;

    size == 0;
  };

  let rec set = result => {
    let updated = result.remainPositions->update(result.mappedPositions);

    updated.remainPositions->isEmpty ? updated : updated->set;
  };

  let find = (fields, ticketsGroup) => {
    let ablePositions =
      ticketsGroup->Belt.Array.map(ticketGroup =>
        ticketGroup->ablePositions(fields)
      );
    let initial = {
      remainPositions: ablePositions,
      mappedPositions: Belt.Map.String.fromArray([||]),
    };
    // Js.log(ablePositions);

    initial->set;
  };

  let mappedPositionsToMyTicketMap = (mappedPositions, myTicket) => {
    let initialMyTickets = Belt.Map.String.fromArray([||]);
    mappedPositions->Belt.Map.String.reduce(
      initialMyTickets, (mappedMyTicket, mappedPosition, index) => {
      mappedMyTicket->Belt.Map.String.set(mappedPosition, myTicket[index])
    });
  };
};

let data2 = Node.Fs.readFileSync("input/2020/2020.16.2.sample", `utf8);
let data2 = Node.Fs.readFileSync("input/2020/2020.16.input", `utf8);
let inputStrs2 = data2->Js.String2.split("\n\n");
let fields2 =
  inputStrs2[0]->Js.String2.split("\n")->Belt.Array.map(Field.make);

let myTickets2 =
  inputStrs2[1]
  ->Js.String2.split("\n")
  ->Belt.Array.sliceToEnd(1)
  ->Tickets.make;

let nearByTickets2 =
  inputStrs2[2]
  ->Js.String2.split("\n")
  ->Belt.Array.sliceToEnd(1)
  ->TicketsGroup.make;

let positions = fields2->PositionFinder.find(nearByTickets2);
let myTicketsMap =
  positions.mappedPositions
  ->PositionFinder.mappedPositionsToMyTicketMap(myTickets2);

Js.log(
  myTicketsMap->Belt.Map.String.reduce(1, (multiply, position, ticket) =>
    position->Js.String2.startsWith("departure")
      ? multiply * ticket : multiply
  ),
);
