
let data = Node.Fs.readFileSync("input/2020/2020.1.input", `utf8);
let years = Js.String.split("\n", data)
->Belt.Array.map(Belt.Int.fromString)
->Belt.Array.keepMap(fun (x) => x);

// part1
let currentYear = 2020;
years->Belt.Array.forEachWithIndex(
    (index, year) => 
        years->Belt.Array.forEachWithIndex(
            (index2, year2) =>
                if (index != index2 && year + year2 == currentYear) {
                    Js.log(year * year2);
                }
        )
)

// part2
years->Belt.Array.forEachWithIndex(
    (index, year) => 
        years->Belt.Array.forEachWithIndex(
            (index2, year2) =>
                if (index != index2) {
                    years->Belt.Array.forEachWithIndex(
                        (index3, year3) =>
                            if (index != index3 && index2 != index3 && year + year2 + year3 == currentYear) {
                                Js.log(year * year2 * year3);
                            }
                    )
                }
        )
)