let data = Node.Fs.readFileSync("input/2020/2020.3.input", `utf8);

module Ground = {
  type t = {hasTree: bool};

  let make = inputStr => {
    switch (inputStr) {
    | "#" => {hasTree: true}
    | _ => {hasTree: false}
    };
  };
};

module Grounds = {
  type t = array(Ground.t);

  let make = inputStrs => {
    inputStrs
    ->Js.String2.split("\n")
    ->Belt.Array.map(row => {
        Js.String.split("", row)->Belt.Array.map(Ground.make)
      });
  };
};

let grounds = data->Grounds.make;

// part1
module TreeCounter = {
  type coord_t = {
    x: int,
    y: int,
  };

  type t = {
    coord: coord_t,
    list: list(Ground.t),
  };

  let walk = (grounds, pattern) => {
    let initial = {
      coord: {
        x: 0,
        y: 0,
      },
      list: [],
    };

    let groundColLength = grounds[0]->Belt.Array.length;
    let groundRowLength = grounds->Belt.Array.length;
    let walked =
      Belt.Array.rangeBy(0, groundRowLength - 1, ~step=pattern.y)
      ->Belt.Array.reduce(initial, (walk, row) => {
          {
            coord: {
              x: (walk.coord.x + pattern.x) mod groundColLength,
              y: row,
            },
            list: [grounds[row][walk.coord.x], ...walk.list],
          }
        });

    walked.list
    ->Belt.List.keep(ground => ground.hasTree)
    ->Belt.List.size
    ->Belt.Float.fromInt;
  };
};

// part2
// let groundColLength = grounds[0]->Belt.Array.length; // part1 에서 이미 정의함
let movePatterns: array(TreeCounter.coord_t) = [|
  {x: 1, y: 1},
  {x: 3, y: 1},
  {x: 5, y: 1},
  {x: 7, y: 1},
  {x: 1, y: 2},
|];
let eachTreeCount =
  movePatterns->Belt.Array.map(pattern => grounds->TreeCounter.walk(pattern));

Js.log(eachTreeCount);
Js.log(eachTreeCount->Belt.Array.reduce(1.0, ( *. )));
