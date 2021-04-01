let data = Node.Fs.readFileAsUtf8Sync("input/2020/2020.4.input")
let raw = data->Js.String2.split("\n\n")

module type Passport = {
  type unvalidate
  type validate
  type t<'a> = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<int>,
  }

  let validators: array<t<unvalidate> => result<unit, unit>>
}

module Validator = (Passport: Passport) => {
  open Passport

  let rec extract = (str, re, os) =>
    switch re->Js.Re.exec_(str) {
    | Some(r) =>
      let captures = r->Js.Re.captures->Belt.Array.keepMap(Js.Nullable.toOption)

      str->extract(re, list{captures, ...os})
    | None => os
    }

  let re = Js.Re.fromStringWithFlags(
    "(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)",
    ~flags="g",
  )

  let make = (inputStr): option<t<unvalidate>> => {
    let os =
      inputStr
      ->extract(re, list{})
      ->Belt.List.toArray
      ->Belt.Array.map(o => (o[1], o[2]))
      ->Belt.Map.String.fromArray

    let getIntExn = (m, k) => m->Belt.Map.String.getExn(k)->Belt.Int.fromString->Belt.Option.getExn

    try {
      Some({
        byr: os->getIntExn("byr"),
        iyr: os->getIntExn("iyr"),
        eyr: os->getIntExn("eyr"),
        hgt: os->Belt.Map.String.getExn("hgt"),
        hcl: os->Belt.Map.String.getExn("hcl"),
        ecl: os->Belt.Map.String.getExn("ecl"),
        pid: os->Belt.Map.String.getExn("pid"),
        cid: switch os->Belt.Map.String.get("cid") {
        | Some(v) => v->Belt.Int.fromString
        | None => None
        },
      })
    } catch {
    | _ => None
    }
  }

  let validate = (p: t<unvalidate>): option<t<validate>> => {
    validators->Belt.Array.map(f => p->f)->Belt.Array.every(Belt.Result.isOk)
      ? Some({
          byr: p.byr,
          iyr: p.iyr,
          eyr: p.eyr,
          hgt: p.hgt,
          hcl: p.hcl,
          ecl: p.ecl,
          pid: p.pid,
          cid: p.cid,
        })
      : None
  }
}

// part1
module Passport1: Passport = {
  type unvalidate
  type validate
  type t<'a> = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<int>,
  }

  let isNotZeroLen = (s) => s->Js.String.length > 0

  let validators = [
    p => p.byr > 0 ? Ok() : Error(),
    p => p.iyr > 0 ? Ok() : Error(),
    p => p.eyr > 0 ? Ok() : Error(),
    p => p.hgt->isNotZeroLen ? Ok() : Error(),
    p => p.hcl->isNotZeroLen ? Ok() : Error(),
    p => p.ecl->isNotZeroLen ? Ok() : Error(),
    p => p.pid->isNotZeroLen ? Ok() : Error(),
    // cid는 없어도 됨
  ]
}

module Validator1 = Validator(Passport1)
raw
->Belt.Array.keepMap(Validator1.make)
->Belt.Array.keepMap(Validator1.validate)
->Belt.Array.size
->Js.log

// part2
module Passport2: Passport = {
  type unvalidate
  type validate
  type t<'a> = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<int>,
  }

  let isMinLen = (s, l) => s->Js.String.length > l
  let isEqLen = (s, l) => s->Js.String.length == l

  let isValidByr = p => p.byr >= 1920 && p.byr <= 2002 ? Ok() : Error()
  let isValidIyr = p => p.iyr >= 2010 && p.iyr <= 2020 ? Ok() : Error()
  let isValidEyr = p => p.eyr >= 2020 && p.eyr <= 2030 ? Ok() : Error()
  let isValidHgt = p => {
    let isValidValue = height => {
      let heightLength = p.hgt->Js.String.length
      let heightUnit = height->Js.String2.substring(~from=heightLength - 2, ~to_=heightLength)
      let convertInt = v => v->Belt.Int.fromString->Belt.Option.getExn
      let between = (v, min, max) => v >= min && v <= max

      switch heightUnit {
      | "cm" => height->convertInt->between(150, 193)
      | "in" => height->convertInt->between(59, 76)
      | _ => false
      }
    }

    p.hgt->isMinLen(1) && p.hgt->isValidValue ? Ok() : Error()
  }

  let isValidHcl = p => {
    let isStartAtSharp = hcl => String.get(hcl, 0) == '#'
    let re = Js.Re.fromString("[0-9a-f]{6}")
    let isValidColor = colorCode => colorCode->isEqLen(6) && re->Js.Re.test_(colorCode)

    p.hcl->isMinLen(0) && p.hcl->isStartAtSharp && p.hcl->Js.String.substr(~from=1)->isValidColor
      ? Ok()
      : Error()
  }

  let validEcl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  let isValidEcl = p => validEcl->Belt.Array.some(ecl => ecl == p.ecl) ? Ok() : Error()

  let re = Js.Re.fromString("[0-9]{9}")
  let isValidPid = p => p.pid->isEqLen(9) && re->Js.Re.test_(p.pid) ? Ok() : Error()

  let validators = [
    isValidByr,
    isValidIyr,
    isValidEyr,
    isValidHgt,
    isValidHcl,
    isValidEcl,
    isValidPid,
  ]
}

module Validator2 = Validator(Passport2)
raw
->Belt.Array.keepMap(Validator2.make)
->Belt.Array.keepMap(Validator2.validate)
->Belt.Array.size
->Js.log
