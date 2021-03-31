let raw = Node.Fs.readFileAsUtf8Sync("input/2020/2020.3.input")

module Ground = {
  type t = bool

  let make = (inputStr): t =>
    switch inputStr {
    | "#" => true
    | _ => false
    }
}

module Grounds = {
  type t = array<Ground.t>

  let make = inputStrs =>
    inputStrs
    ->Js.String2.split("\n")
    ->Belt.Array.map(row => row->Js.String2.split("")->Belt.Array.map(Ground.make))
}

module Coord = {
  type t = {
    x: int,
    y: int,
  }
}

// part1
module TreeCounter = {
  let walk = (pattern: Coord.t, grounds) => {
    let groundColLength = grounds[0]->Belt.Array.length
    let groundRowLength = grounds->Belt.Array.length

    let rec fn = (gs, ~c: Coord.t, ~p: Coord.t, v) => {
      let v = v + (gs[c.y][c.x] ? 1 : 0)
      let nx = mod(c.x + p.x, groundColLength)
      let ny = c.y + p.y

      ny >= groundRowLength ? v : fn(gs, ~c={x: nx, y: ny}, ~p, v)
    }

    fn(grounds, ~c={x: 0, y: 0}, ~p=pattern, 0)->Belt.Float.fromInt
  }
}

let grounds = raw->Grounds.make

// part2
// let groundColLength = grounds[0]->Belt.Array.length; // part1 에서 이미 정의함
let movePatterns: array<Coord.t> = [
  {x: 1, y: 1},
  {x: 3, y: 1},
  {x: 5, y: 1},
  {x: 7, y: 1},
  {x: 1, y: 2},
]
let eachTreeCount = movePatterns->Belt.Array.map(pattern => pattern->TreeCounter.walk(grounds))

eachTreeCount->Js.log
eachTreeCount->Belt.Array.reduce(1.0, \"*.")->Js.log
