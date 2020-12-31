let data = Node.Fs.readFileSync("input/2020/2020.3.input", `utf8);

module Ground = {
  type t = {hasTree: bool};

  let make = (inputStr) => {
    switch (inputStr) {
    | "#" => {hasTree: true}
    | _ => {hasTree: false}
    };
  };
};

let grounds =
  Js.String.split("\n", data)
  ->Belt.Array.map(row => {
      Js.String.split("", row)->Belt.Array.map(Ground.make)
    });

// part1
type cord = {
  x: int,
  y: int,
};
type walkAround = {
  cord,
  list: list(Ground.t),
};

let groundColLength = grounds[0]->Belt.Array.length;
let walked =
  grounds->Belt.Array.reduce(
    {
      cord: {
        x: 1,
        y: 1,
      },
      list: [],
    }, (walk, eachGrounds) => {
    {
      cord: {
        x:
          walk.cord.x + 3 > groundColLength
            ? walk.cord.x + 3 - groundColLength : walk.cord.x + 3,
        y: walk.cord.y + 1,
      },
      list: [eachGrounds[walk.cord.x - 1], ...walk.list],
    }
  });
let treeCount =
  walked.list->Belt.List.keep(ground => ground.hasTree)->Belt.List.length;

Js.log(treeCount);

// part2
// let groundColLength = grounds[0]->Belt.Array.length; // part1 에서 이미 정의함
let groundRowLength = grounds->Belt.Array.length;
let movePatterns = [
  {x: 1, y: 1},
  {x: 3, y: 1},
  {x: 5, y: 1},
  {x: 7, y: 1},
  {x: 1, y: 2},
];
let eachTreeCount =
  movePatterns->Belt.List.map(movePattern => {
    let walked =
      Belt.Array.rangeBy(1, groundRowLength, ~step=movePattern.y)
      ->Belt.Array.reduce(
          {
            cord: {
              x: 1,
              y: 1,
            },
            list: [],
          }, (walk, currentRow) => {
          {
            cord: {
              x:
                walk.cord.x + movePattern.x > groundColLength
                  ? walk.cord.x + movePattern.x - groundColLength
                  : walk.cord.x + movePattern.x,
              y: currentRow,
            },
            list: [grounds[currentRow - 1][walk.cord.x - 1], ...walk.list],
          }
        });

    walked.list
    ->Belt.List.keep(ground => ground.hasTree)
    ->Belt.List.length
    ->Belt.Float.fromInt;
  });

let treeCountMultiply =
  eachTreeCount->Belt.List.reduce(1.0, (multiply, treeCount) =>
    multiply *. treeCount
  );

Js.log(treeCountMultiply);
