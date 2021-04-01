let data = Node.Fs.readFileAsUtf8Sync("input/2020/2020.11.input")
let rows = data->Js.String2.split("\n")

module Seat = {
  type t =
    | Empty
    | Occupied
    | Floor

  exception InvalidState

  let make = inputStr =>
    switch inputStr {
    | "L" => Empty
    | "." => Floor
    | _ => raise(InvalidState)
    }
}

module Seats = {
  type t = array<Seat.t>
}

module Coord = {
  type t = {
    row: int,
    col: int,
  }

  let makeAroundCoords = coord => list{
    {row: coord.row - 1, col: coord.col - 1},
    {row: coord.row - 1, col: coord.col},
    {row: coord.row - 1, col: coord.col + 1},
    {row: coord.row, col: coord.col - 1},
    // {row: coord.row, col: coord.col}, // 내 위치
    {row: coord.row, col: coord.col + 1},
    {row: coord.row + 1, col: coord.col - 1},
    {row: coord.row + 1, col: coord.col},
    {row: coord.row + 1, col: coord.col + 1},
  }

  let makeDirections = () => list{
    {row: -1, col: -1},
    {row: -1, col: 0},
    {row: -1, col: 1},
    {row: 0, col: -1},
    // {row: 0, col: 0}, // 내 위치
    {row: 0, col: 1},
    {row: 1, col: -1},
    {row: 1, col: 0},
    {row: 1, col: 1},
  }
}

let parse = r => r->Js.String2.split("")
let seatsMap = rows->Belt.Array.map(row => row->parse->Belt.Array.map(Seat.make))

// part1
module type MakeNextState = {
  let next: int => Seat.t
}

module NextState = (Item: MakeNextState) => {
  let occupiedCount = (seatsMap, positions: list<Coord.t>) =>
    positions
    ->Belt.List.keep(position =>
      switch seatsMap->Belt.Array.get(position.row) {
      | None => false
      | Some(seats) =>
        switch seats->Belt.Array.get(position.col) {
        | Some(seat) => seat == Seat.Occupied
        | _ => false
        }
      }
    )
    ->Belt.List.size

  let nextWithIndex = (seatsMap, coord: Coord.t) => {
    let positions = Coord.makeAroundCoords(coord)

    seatsMap->occupiedCount(positions)->Item.next
  }
}

module MakeEmptyNextState = {
  open Seat
  let next = occupiedCount => occupiedCount == 0 ? Occupied : Empty
}
module EmptyState = NextState(MakeEmptyNextState)

module MakeOccupiedNextState = {
  open Seat
  let next = occupiedCount => occupiedCount >= 4 ? Empty : Occupied
}
module OccupiedState = NextState(MakeOccupiedNextState)

module SeatsCompare = {
  type t = {
    seat1: Seats.t,
    seat2: Seats.t,
  }

  let isEqualSeatWithIndex = (seats, index) => seats.seat1[index] == seats.seat2[index]

  let rec isEqualSeats = (seats, index): bool => {
    let result = seats->isEqualSeatWithIndex(index)

    if !result || seats.seat1->Belt.Array.size == index + 1 {
      result
    } else {
      seats->isEqualSeats(index + 1)
    }
  }

  let make = (seat1, seat2) => {
    seat1: seat1,
    seat2: seat2,
  }
}

module SeatsMap = {
  type t = {
    seat1: array<Seats.t>,
    seat2: array<Seats.t>,
  }

  let rec isEqualRow = (seatsMap, index): bool => {
    let result =
      SeatsCompare.make(seatsMap.seat1[index], seatsMap.seat2[index])->SeatsCompare.isEqualSeats(0)

    if !result || seatsMap.seat1->Belt.Array.size == index + 1 {
      result
    } else {
      seatsMap->isEqualRow(index + 1)
    }
  }

  let isEqual = seatsMap => seatsMap->isEqualRow(0)
}

let printMap = seatsMap => {
  open Seat
  seatsMap->Belt.Array.forEach(seats =>
    seats
    ->Belt.Array.map(seat =>
      switch seat {
      | Occupied => "#"
      | Empty => "L"
      | Floor => "."
      }
    )
    ->Js.Array2.joinWith("")
    ->Js.log
  )
}

module SeatChanger = {
  let makeNextState = (seatsMap, emptyStateChanger, occupiedStatechanger) =>
    seatsMap->Belt.Array.mapWithIndex((rIndex, seats) =>
      seats->Belt.Array.mapWithIndex((cIndex, seat) => {
        let coord: Coord.t = {row: rIndex, col: cIndex}
        switch seat {
        | Seat.Floor => Seat.Floor
        | Empty => seatsMap->emptyStateChanger(coord)
        | Occupied => seatsMap->occupiedStatechanger(coord)
        }
      })
    )

  let rec findNoChangeState = (seatsMap, emptyStateChanger, occupiedStatechanger) => {
    let seatsMap: SeatsMap.t = {
      // 확실한 네이밍 필요?..
      seat1: seatsMap,
      seat2: seatsMap->makeNextState(emptyStateChanger, occupiedStatechanger),
    }

    // seatsMap.seat2->printMap;

    seatsMap->SeatsMap.isEqual
      ? seatsMap.seat2
      : seatsMap.seat2->findNoChangeState(emptyStateChanger, occupiedStatechanger)
  }
}

let occupiedCounter = seatsMap =>
  seatsMap->Belt.Array.reduce(0, (sum, row) =>
    sum + row->Belt.Array.keep(seat => seat == Seat.Occupied)->Belt.Array.size
  )

seatsMap
->SeatChanger.findNoChangeState(EmptyState.nextWithIndex, OccupiedState.nextWithIndex)
->occupiedCounter
->Js.log

// part2
module NextState2 = (Item: MakeNextState) => {
  type t =
    | IndexOverflow
    | IsOccupied
    | GoToNextSeat
    | NotOccupied

  let rec isOccupied = (seatsMap, direction: Coord.t, coord: Coord.t) => {
    let result = switch seatsMap->Belt.Array.get(coord.row) {
    | None => IndexOverflow
    | Some(seats) =>
      switch seats->Belt.Array.get(coord.col) {
      | None => IndexOverflow
      | Some(Seat.Floor) => GoToNextSeat
      | Some(Occupied) => IsOccupied
      | _ => NotOccupied
      }
    }

    switch result {
    | IsOccupied => true
    | GoToNextSeat =>
      seatsMap->isOccupied(
        direction,
        {row: coord.row + direction.row, col: coord.col + direction.col},
      )
    | IndexOverflow | NotOccupied => false
    }
  }

  let occupiedCount = (seatsMap, directions: list<Coord.t>, coord: Coord.t) =>
    directions
    ->Belt.List.keep(direction =>
      seatsMap->isOccupied(
        direction,
        {row: coord.row + direction.row, col: coord.col + direction.col},
      )
    )
    ->Belt.List.size

  let nextWithIndex = (seatsMap, coord: Coord.t) => {
    let directions = Coord.makeDirections()

    seatsMap->occupiedCount(directions, coord)->Item.next
  }
}

module EmptyState2 = NextState2(MakeEmptyNextState)
module MakeOccupiedNextState2 = {
  open Seat
  let next = occupiedCount => occupiedCount >= 5 ? Empty : Occupied
}
module OccupiedState2 = NextState2(MakeOccupiedNextState2)

seatsMap
->SeatChanger.findNoChangeState(EmptyState2.nextWithIndex, OccupiedState2.nextWithIndex)
->occupiedCounter
->Js.log
