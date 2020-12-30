let data = Node.Fs.readFileSync("input/2020/2020.3.input", `utf8);

type ground = {
    hasTree: bool,
};

let grounds = Js.String.split("\n", data)
-> Belt.Array.map(
    (row) => {
        Js.String.split("", row)
        -> Belt.Array.map((x) => {hasTree: x == "#"})
    }
);

// part1
let queue = Belt.MutableQueue.make();
let groundColLength = grounds[0]->Belt.Array.length;
let lastCol = grounds
-> Belt.Array.reduce(
    1,
    (col, eachGrounds) => {
        queue->Belt.MutableQueue.add(eachGrounds[col - 1]);
        col + 3 > groundColLength ? col + 3 - groundColLength : col + 3
    }
);
let treeCount = queue
-> Belt.MutableQueue.reduce(
    0,
    (count, ground) => ground.hasTree ? count + 1 : count
);

Js.log(treeCount);

// part2
type cord = {
    x: int,
    y: int,
}

// let groundColLength = grounds[0]->Belt.Array.length; // part1 에서 이미 정의함
let groundRowLength = grounds->Belt.Array.length;
let movePatterns = [
    {x: 1, y: 1},
    {x: 3, y: 1},
    {x: 5, y: 1},
    {x: 7, y: 1},
    {x: 1, y: 2},
];
let eachTreeCount = movePatterns
-> Belt.List.map(
    (movePattern) => {
        let treeQueue = Belt.MutableQueue.make();
        
        let lastCol = Belt.Array.rangeBy(1, groundRowLength, ~step=movePattern.y)
        -> Belt.Array.reduce(
            1,
            (currentCol, currentRow) => {
                treeQueue->Belt.MutableQueue.add(grounds[currentRow - 1][currentCol - 1]);
                currentCol + movePattern.x > groundColLength ? currentCol + movePattern.x - groundColLength : currentCol + movePattern.x
            }
        );

        treeQueue
        -> Belt.MutableQueue.reduce(
            0,
            (count, ground) => ground.hasTree ? count + 1 : count
        );
    }
);

let treeCountMultiply = eachTreeCount
-> Belt.List.reduce(
    1.0,
    (multiply, treeCount) => multiply *. treeCount->Belt.Float.fromInt
);

Js.log(treeCountMultiply);
