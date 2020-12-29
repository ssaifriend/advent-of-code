let sample = "))(((((";

let finalFloor = Array.fold_left(
    (result, str) => 
        switch (str) {
        | "(" => result + 1
        | ")" => result - 1
        },
    0,
    Js.String.split("", sample)
)

// part 1
Js.Console.log(finalFloor);


// part 2 - WIP...
