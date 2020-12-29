let sample = "))(((((";
let floor = ref(0);
let printBasementFirstPosition = ref(false);

for (i in 0 to String.length(sample) - 1) {
    switch (sample.[i]) {
        | '(' => floor := floor^ + 1
        | ')' => floor := floor^ - 1
    }

    // part 2
    if (!printBasementFirstPosition^ && floor^ < 0) {
        Js.Console.log("First poisition in basement");
        Js.Console.log(i + 1);
        printBasementFirstPosition := true;
    }
}

// part 1
Js.Console.log(floor^);

/* - type missmatch
let sampleArray = Js.String.split("", sample);
Array.map(a => a == "(" ? 1 : -1, sampleArray);
*/
