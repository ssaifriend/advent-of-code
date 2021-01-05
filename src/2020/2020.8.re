let data = Node.Fs.readFileSync("input/2020/2020.8.sample", `utf8);

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
};

let rec walk = (codes: array(Op.t), s): summary => {
  switch (codes[s.currentIndex]) {
  | _ when s.indexes->Belt.List.some(index => index == s.currentIndex) => s
  
  | Nop(_) =>
    codes->walk({
      ...s,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Acc(value) => 
    codes->walk({
      sum: s.sum + value,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Jmp(value) => 
    codes->walk({
      ...s,
      currentIndex: s.currentIndex + value,
      indexes: [s.currentIndex, ...s.indexes],
    })
  };
};

Js.log(codes->walk({sum: 0, currentIndex: 0, indexes: []}).sum);

// part2 - 문제 재확인 필요;
exception InvalidCode;
exception InvalidIndex;

let rec fixWalk = (codes: array(Op.t), s): summary => {
  switch (codes->Belt.Array.get(s.currentIndex)) {
  | None => s

  | _ when s.indexes->Belt.List.some(index => index == s.currentIndex) => {
    let prevIndex = s.indexes->Belt.List.head->Belt.Option.getExn;
    let changedCode = 
      switch (codes[prevIndex]) {
        | Acc(value) => Op.Jmp(value)
        | Jmp(value) => Acc(value)
        | _ => raise(InvalidCode)
      }
    let revertSum = 
      switch (codes[prevIndex]) {
        | Acc(value) => s.sum - value
        | _ => s.sum
      }

    let _ = codes->Belt.Array.set(prevIndex, changedCode);

    codes->fixWalk({
      sum: revertSum,
      currentIndex: prevIndex,
      indexes: switch (s.indexes->Belt.List.drop(1)) {
        | Some(i) => i
        | None => raise(InvalidIndex)
      },
    })
  }
  
  | Some(Nop(_)) =>
    codes->fixWalk({
      ...s,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Some(Acc(value)) =>
    codes->fixWalk({
      sum: s.sum + value,
      currentIndex: s.currentIndex + 1,
      indexes: [s.currentIndex, ...s.indexes],
    })
  | Some(Jmp(value)) => 
    codes->fixWalk({
      ...s,
      currentIndex: s.currentIndex + value,
      indexes: [s.currentIndex, ...s.indexes],
    })
  };
};

Js.log(codes->fixWalk({sum: 0, currentIndex: 0, indexes: []}).sum);
