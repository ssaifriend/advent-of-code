let data = Node.Fs.readFileSync("input/2020/2020.4.input", `utf8);

module Passport = {
  type t = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option(int),
  };

  exception InvalidInfo;

  let rec extract = (str, re, options) => {
    switch (re->Js.Re.exec_(str)) {
    | Some(r) =>
      let captures =
        r
        ->Js.Re.captures
        ->Belt.Array.map(Js.Nullable.toOption)
        ->Belt.Array.keepMap(x => x);

      extract(str, re, [captures, ...options]);
    | None => options
    };
  };

  let re =
    Js.Re.fromStringWithFlags(
      "(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)",
      ~flags="g",
    );

  let make = inputStr => {
    let p = {
      byr: 0,
      iyr: 0,
      eyr: 0,
      hgt: "",
      hcl: "",
      ecl: "",
      pid: "",
      cid: None,
    };

    extract(inputStr, re, [])
    ->Belt.List.reduce(p, (p, option) => {
        switch (option[1]) {
        | "byr" => {
            ...p,
            byr: option[2]->Belt.Int.fromString->Belt.Option.getExn,
          }
        | "iyr" => {
            ...p,
            iyr: option[2]->Belt.Int.fromString->Belt.Option.getExn,
          }
        | "eyr" => {
            ...p,
            eyr: option[2]->Belt.Int.fromString->Belt.Option.getExn,
          }
        | "hgt" => {...p, hgt: option[2]}
        | "hcl" => {...p, hcl: option[2]}
        | "ecl" => {...p, ecl: option[2]}
        | "pid" => {...p, pid: option[2]}
        | "cid" => {...p, cid: option[2]->Belt.Int.fromString}
        | _ => raise(InvalidInfo)
        }
      });
  };
};

module PassportValidator = {
  let isValid = (passport: Passport.t, validators) => {
    validators->Belt.List.reduce(true, (isValid, func) =>
      isValid && func(passport)->Belt.Result.isOk
    )
  };
}

let passports = Js.String.split("\n\n", data)->Belt.Array.map(Passport.make);

// part1

let validFunctions = [
  (passport: Passport.t) => passport.byr > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) => passport.iyr > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) => passport.eyr > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) =>
    passport.hgt->Js.String.length > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) =>
    passport.hcl->Js.String.length > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) =>
    passport.ecl->Js.String.length > 0 ? Ok(true) : Error(false),
  (passport: Passport.t) =>
    passport.pid->Js.String.length > 0 ? Ok(true) : Error(false),
  // cid는 없어도 됨
];

let validPassportCount =
  passports
  ->Belt.Array.map(passport => {
      passport->PassportValidator.isValid(validFunctions)
    })
  ->Belt.Array.keep(isValid => isValid)
  ->Belt.Array.length;

Js.log(validPassportCount);

// part2
let isValidByr = (passport: Passport.t) =>
  passport.byr >= 1920 && passport.byr <= 2002 ? Ok(true) : Error(false);
let isValidIyr = (passport: Passport.t) =>
  passport.iyr >= 2010 && passport.iyr <= 2020 ? Ok(true) : Error(false);
let isValidEyr = (passport: Passport.t) =>
  passport.eyr >= 2020 && passport.eyr <= 2030 ? Ok(true) : Error(false);
let isValidHgt = (passport: Passport.t) => {
  let isValidValue = height => {
    let heightLength = passport.hgt->Js.String.length;
    let heightUnit =
      String.make(1, height.[heightLength - 2])
      ++ String.make(1, height.[heightLength - 1]);

    switch (heightUnit) {
    | "cm" =>
      let h = height->Belt.Int.fromString->Belt.Option.getExn;
      h >= 150 && h <= 193;
    | "in" =>
      let h = height->Belt.Int.fromString->Belt.Option.getExn;
      h >= 59 && h <= 76;
    | _ => false
    };
  };

  passport.hgt->Js.String.length >= 2 && passport.hgt->isValidValue
    ? Ok(true) : Error(false);
};
let isValidHcl = (passport: Passport.t) => {
  let isStartAtSharp = hcl => hcl.[0] == '#';
  let isValidColor = colorCode => {
    let re = Js.Re.fromString("[0-9a-f]{6}");

    colorCode->Js.String.length == 6 && re->Js.Re.test_(colorCode);
  };

  passport.hcl->Js.String.length > 0
  && passport.hcl->isStartAtSharp
  && passport.hcl->Js.String.substr(~from=1)->isValidColor
    ? Ok(true) : Error(false);
};
let isValidEcl = (passport: Passport.t) => {
  let validEclSet =
    Belt.Set.String.fromArray([|
      "amb",
      "blu",
      "brn",
      "gry",
      "grn",
      "hzl",
      "oth",
    |]);

  validEclSet->Belt.Set.String.has(passport.ecl) ? Ok(true) : Error(false);
};
let isValidPid = (passport: Passport.t) => {
  let re = Js.Re.fromString("[0-9]{9}");

  passport.pid->Js.String.length == 9 && Js.Re.test_(re, passport.pid)
    ? Ok(true) : Error(false);
};

let validFunctions2 = [
  isValidByr,
  isValidIyr,
  isValidEyr,
  isValidHgt,
  isValidHcl,
  isValidEcl,
  isValidPid,
];

let validPassportCount2 =
  passports
  ->Belt.Array.map(passport => {
      passport->PassportValidator.isValid(validFunctions2)
    })
  ->Belt.Array.keep(isValid => isValid)
  ->Belt.Array.length;

Js.log(validPassportCount2);
