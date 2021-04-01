type direction_t =
  | East
  | West
  | North
  | South

exception InvalidDirection

let parse = inputStr =>
  inputStr
  ->Js.String2.split("")
  ->Belt.Array.map(str =>
    switch str {
    | ">" => East
    | "<" => West
    | "^" => North
    | "v" => South
    | _ => raise(InvalidDirection)
    }
  )
  ->Belt.List.fromArray

let rec visit = (directions, (x, y), visits) => {
  let next = switch directions->Belt.List.head {
  | Some(East) => Some((x + 1, y))
  | Some(West) => Some((x - 1, y))
  | Some(North) => Some((x, y + 1))
  | Some(South) => Some((x, y - 1))
  | None => None
  }

  switch (next, directions->Belt.List.tail) {
  | (None, _)
  | (Some(_), None) => visits
  | (Some(coord), Some(tail)) => tail->visit(coord, list{coord, ...visits})
  }
}

module CoordComp = Belt.Id.MakeComparable({
  type t = (int, int)
  let cmp = ((x1, y1), (x2, y2)) =>
    switch Pervasives.compare(x1, x2) {
    | 0 => Pervasives.compare(y1, y2)
    | c => c
    }
})

let unique = visits =>
  visits->Belt.List.toArray->Belt.Set.fromArray(~id=module(CoordComp))->Belt.Set.toArray

list{">", "^>v<", "^v^v^v^v^v"}
->Belt.List.map(l => l->parse->visit((0, 0), list{(0, 0)}))
->Belt.List.forEach(visits => visits->unique->Belt.Array.size->Js.log)

Node.Fs.readFileAsUtf8Sync("input/2015/2015.3.input")
->parse
->visit((0, 0), list{(0, 0)})
->unique
->Belt.Array.size
->Js.log

// part2
let splitRobo = directions =>
  Belt.Array.range(0, 1)->Belt.Array.map(index =>
    directions->Belt.List.keepWithIndex((_, dIndex) => mod(dIndex, 2) == index)
  )

let visitsEachSanta = directionsWithRobo =>
  directionsWithRobo
  ->Belt.Array.map(directions => directions->visit((0, 0), list{(0, 0)})->Belt.List.toArray)
  ->Belt.Array.concatMany
  ->Belt.List.fromArray

list{"^v", "^>v<", "^v^v^v^v^v"}
->Belt.List.map(l => l->parse->splitRobo->visitsEachSanta)
->Belt.List.forEach(visits => visits->unique->Belt.Array.size->Js.log)

Node.Fs.readFileAsUtf8Sync("input/2015/2015.3.input")
->parse
->splitRobo
->visitsEachSanta
->unique
->Belt.Array.size
->Js.log
