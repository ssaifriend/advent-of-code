let data = Node.Fs.readFileSync("input/2020/2020.2.input", `utf8);

type passwordSet = {
    min: int,
    max: int,
    first: int,
    second: int,
    char: string,
    password: string,
};

let passwordRe = Js.Re.fromString("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)");
let passwordSets = Js.String.split("\n", data)
-> Belt.Array.map(
    (row) => {
        let result = passwordRe->Js.Re.exec_(row)
        switch (result) {
        | Some(r) =>
            let captures = Js.Re.captures(r)->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(fun (x) => x);
            let first = captures[1]->Belt.Int.fromString->Belt.Option.getExn;
            let second = captures[2]->Belt.Int.fromString->Belt.Option.getExn;
            {
                min: first,
                max: second,
                first,
                second,
                char: captures[3],
                password: captures[4],
            };
        | None => raise(Not_found);
        }
    }
);

// part1
let validPasswordSetCount = passwordSets
-> Belt.Array.keep((set) => {
    let charCount = Js.String.split("", set.password)
    -> Belt.Array.keep(fun (x) => set.char == x)
    -> Belt.Array.length;

    set.min <= charCount && charCount <= set.max;
})
-> Belt.Array.length;

Js.log(validPasswordSetCount);


// part2
let validPasswordSetCount2 = passwordSets
-> Belt.Array.keep((set) => {
    set.password.[set.first - 1] == set.char.[0] && set.password.[set.second - 1] != set.char.[0]
    || set.password.[set.first - 1] != set.char.[0] && set.password.[set.second - 1] == set.char.[0]
})
-> Belt.Array.length;

Js.log(validPasswordSetCount2);
