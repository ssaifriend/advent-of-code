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
  let isNumberInRange = (number, start, end_) => {
    number >= start && number <= end_;
  };

  let isTicketInFieldRange = (ticket, field: Field.t) => {
    switch (
      ticket->isNumberInRange(field.firstRangeStart, field.firstRangeEnd),
      ticket->isNumberInRange(field.secondRangeStart, field.secondRangeEnd),
    ) {
    | (true, _)
    | (_, true) => true
    | (_, _) => false
    };
  };

  let rec isValidTicket = (ticket, fields) => {
    let result = ticket->isTicketInFieldRange(fields[0]);

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
Js.log(invalidTickets->Belt.Array.reduce(0, (sum, ticket) => sum + ticket));


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
  let isNumberInRange = (number, start, end_) => {
    number >= start && number <= end_;
  };

  let isTicketInFieldRange = (ticket, field: Field.t) => {
    switch (
      ticket->isNumberInRange(field.firstRangeStart, field.firstRangeEnd),
      ticket->isNumberInRange(field.secondRangeStart, field.secondRangeEnd),
    ) {
    | (true, _)
    | (_, true) => true
    | (_, _) => false
    };
  };

  let ablePositions = (ticketGroup, fields: Fields.t) => {
    let ticketSize = ticketGroup->Belt.Array.size;

    fields->Belt.Array.keepMap(field => {
      let validRangeCount =
        ticketGroup
        ->Belt.Array.map(ticket => ticket->isTicketInFieldRange(field))
        ->Belt.Array.keep(x => x)
        ->Belt.Array.size;

      validRangeCount == ticketSize ? Some(field.name) : None;
    });
  };

  let removeUpdatedPositions = (positions, removePositions) => {
    positions->Belt.Array.keep(position =>
      !removePositions->Belt.Array.some(removePos => removePos == position)
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
        ablePosition->removeUpdatedPositions(existPositions)
      );

    {remainPositions, mappedPositions: updatedPositions};
  };

  let rec set = result => {
    let updated = result.remainPositions->update(result.mappedPositions);

    updated.remainPositions->Belt.Array.size == 0 ? updated : updated->set;
  };

  let find = (fields, ticketsGroup) => {
    let ablePositions =
      ticketsGroup->Belt.Array.map(ticketGroup =>
        ticketGroup->ablePositions(fields)
      );
    let initial = {remainPositions: ablePositions, mappedPositions: Belt.Map.String.fromArray([||])}

    initial->set
  };
};

let data2 = Node.Fs.readFileSync("input/2020/2020.16.2.sample", `utf8);
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

Js.log(fields2->PositionFinder.find(nearByTickets2).mappedPositions)
