let data = Node.Fs.readFileSync("input/2020/2020.8.input", `utf8);

module Op = {
  type t =
    | Acc(int)
    | Jmp(int)
    | Nop(int);

  let make = inputStr => {
    let re = Js.Re.fromString("(acc|jmp|nop) (\+|\-)([0-9]+)");
    let result = re->Js.Re.exec_(inputStr);
    switch (result) {
    | Some(r) =>
      let captures =
        Js.Re.captures(r)
        ->Belt.Array.map(Js.Nullable.toOption)
        ->Belt.Array.keepMap(x => x);

      let value =
        (captures[2] == "-" ? (-1) : 1)
        * captures[3]->Belt.Int.fromString->Belt.Option.getExn;

      switch (captures[1]) {
      | "acc" => Acc(value)
      | "jmp" => Jmp(value)
      | "nop" => Nop(value)
      | _ => raise(Not_found)
      };
    | None => raise(Not_found)
    };
  };
};

let codes = data->Js.String2.split("\n")->Belt.Array.map(Op.make);

// part1
type summary = {
  sum: int,
  currentIndex: int,
  indexes: list(int),
  isSuccess: bool,
};

let initialState = {sum: 0, currentIndex: 0, indexes: [], isSuccess: false};
let rec walk = (codes: array(Op.t), s): summary => {
  switch (codes->Belt.Array.get(s.currentIndex)) {
  | None => {...s, isSuccess: true}
  | _ when s.indexes->Belt.List.some(index => index == s.currentIndex) => s
  
  | Some(Nop(_)) =>
    codes->walk({
      ...s,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Some(Acc(value)) => 
    codes->walk({
      ...s,
      sum: s.sum + value,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Some(Jmp(value)) => 
    codes->walk({
      ...s,
      currentIndex: s.currentIndex + value,
      indexes: [s.currentIndex, ...s.indexes],
    })
  };
};

Js.log(codes->walk(initialState).sum);

// part2

let changeCode = (codes, changeIndex) => {
  open Op;

  codes
  ->Belt.Array.mapWithIndex(
    (index, code) => {
      let isChangeIndex = index == changeIndex;
      switch (isChangeIndex, code) {
        | (true, Jmp(value)) => Nop(value)
        | (true, Nop(value)) => Jmp(value)
        | (_, value) => value
      }
    }
  );
}

let rec findNotInfSummary = (codes, index) => {
  let changedCodes = codes->changeCode(index);
  let s = changedCodes->walk(initialState);
  
  s.isSuccess ? s.sum : codes->findNotInfSummary(index - 1);
}

Js.log(codes->findNotInfSummary(codes->Belt.Array.size - 1));