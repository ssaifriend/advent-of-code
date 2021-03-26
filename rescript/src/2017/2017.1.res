module Captcha = {
  type t = list<int>

  let make = str =>
    str->Js.String2.split("")->Belt.Array.keepMap(Belt.Int.fromString)->Belt.List.fromArray
}

module Solution = {
  type t = list<int>

  let updateEqualNumber = (numberList, number1, number2) =>
    number1 == number2 ? list{number1, ...numberList} : numberList

  let rec makeMatchesNumbers = (captcha, ~result, ~fullCaptcha) =>
    switch (captcha->Belt.List.head, captcha->Belt.List.tail) {
    | (Some(_), None)
    | (None, _) => result // 실제로 없는 케이스

    | (Some(number), Some(tail)) when tail->Belt.List.size == 0 =>
      result->updateEqualNumber(number, fullCaptcha->Belt.List.headExn)
    | (Some(number), Some(tail)) =>
      tail->makeMatchesNumbers(
        ~result=result->updateEqualNumber(number, tail->Belt.List.headExn),
        ~fullCaptcha,
      )
    }

  let make = captcha =>
    captcha->makeMatchesNumbers(~fullCaptcha=captcha, ~result=list{})->Belt.List.reduce(0, \"+")
}

// switch ([1]->Belt.List.tail) {
// | Some(tail) => Js.log(tail); // <- 여기가 trigger 됨
// | None => Js.log("None");
// }

Node.Fs.readFileSync("input/2017/2017.1.sample", #utf8)
->Js.String2.split("\n")
->Belt.Array.map(Captcha.make)
->Belt.Array.map(Solution.make)
->Js.log

Node.Fs.readFileSync("input/2017/2017.1.input", #utf8)
->Js.String2.split("\n")
->Belt.Array.map(Captcha.make)
->Belt.Array.map(Solution.make)
->Js.log

// part2
module Door = {
  type t = {
    h: list<int>,
    t: list<int>,
    halfSize: int,
  }

  let print = door => {
    door.h->Belt.List.toArray->Js.log
    door.t->Belt.List.toArray->Js.log
    door.halfSize->Js.log

    door
  }

  let make = str => {
    let halfSize = str->Js.String2.length / 2
    let makeIntList = str =>
      str->Js.String2.split("")->Belt.Array.keepMap(Belt.Int.fromString)->Belt.List.fromArray

    {
      h: str->Js.String2.slice(~from=0, ~to_=halfSize)->makeIntList,
      t: str->Js.String2.sliceToEnd(~from=halfSize)->makeIntList,
      halfSize: halfSize,
    }
  }
}

module Solution2 = {
  let updateEqualNumber = (numberList, number1, number2) =>
    number1 == number2 ? list{number1 * 2, ...numberList} : numberList

  let rec makeMatchesNumber = (door: Door.t, ~result) =>
    switch (door.h->Belt.List.head, door.t->Belt.List.head, door.h->Belt.List.tail) {
    | (Some(_), Some(_), None)
    | (Some(_), None, _)
    | (None, _, _) => result // 실제로 없는 케이스

    | (Some(h), Some(t), Some(tail)) when tail->Belt.List.size == 0 =>
      result->updateEqualNumber(h, t)
    | (Some(h), Some(t), Some(_)) =>
      let next = {
        ...door,
        h: door.h->Belt.List.tailExn,
        t: door.t->Belt.List.tailExn,
      }
      next->makeMatchesNumber(~result=result->updateEqualNumber(h, t))
    }

  let make = door => door->makeMatchesNumber(~result=list{})->Belt.List.reduce(0, \"+")
}

Node.Fs.readFileSync("input/2017/2017.1.2.sample", #utf8)
->Js.String2.split("\n")
->Belt.Array.map(Door.make)
// ->Belt.Array.map(Door.print)
->Belt.Array.map(Solution2.make)
->Js.log

Node.Fs.readFileSync("input/2017/2017.1.input", #utf8)
->Js.String2.split("\n")
->Belt.Array.map(Door.make)
->Belt.Array.map(Solution2.make)
->Js.log
