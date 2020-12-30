let data = Node.Fs.readFileSync("input/2020/2020.4.input", `utf8);

type passport = {
    byr: option(int),
    iyr: option(int),
    eyr: option(int),
    hgt: option(string),
    hcl: option(string),
    ecl: option(string),
    pid: option(string),
    cid: option(int),
};

exception InvalidInfo;

let passports = Js.String.split("\n\n", data)
-> Belt.Array.map(
    (row) => {
        let re = Js.Re.fromStringWithFlags("(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)", ~flags="g")
        let options = Belt.MutableQueue.make();
        let break = ref(false);
        while (!break.contents) {
            switch (re->Js.Re.exec_(row)) {
            | Some(r) => 
                let captures = r->Js.Re.captures->Belt.Array.map(Js.Nullable.toOption)->Belt.Array.keepMap(fun (x) => x);
                options->Belt.MutableQueue.add(captures)
            | None => break := true
            }
        }
        
        let p: passport = {byr: None, iyr: None, eyr: None, hgt: None, hcl: None, ecl: None, pid: None, cid: None};

        options
        -> Belt.MutableQueue.reduce(
            p,
            (p, option) => {
                switch (option[1]) {
                    | "byr" => {...p, byr: option[2]->Belt.Int.fromString}
                    | "iyr" => {...p, iyr: option[2]->Belt.Int.fromString}
                    | "eyr" => {...p, eyr: option[2]->Belt.Int.fromString}
                    | "hgt" => {...p, hgt: Some(option[2])}
                    | "hcl" => {...p, hcl: Some(option[2])}
                    | "ecl" => {...p, ecl: Some(option[2])}
                    | "pid" => {...p, pid: Some(option[2])}
                    | "cid" => {...p, cid: option[2]->Belt.Int.fromString}
                    | _ => raise(InvalidInfo)
                }
            }
        )
    }
)


// part1
let validFunctions = [
    (passport) => passport.byr->Belt.Option.isSome,
    (passport) => passport.iyr->Belt.Option.isSome,
    (passport) => passport.eyr->Belt.Option.isSome,
    (passport) => passport.hgt->Belt.Option.isSome,
    (passport) => passport.hcl->Belt.Option.isSome,
    (passport) => passport.ecl->Belt.Option.isSome,
    (passport) => passport.pid->Belt.Option.isSome,
    // cid는 없어도 됨
]

let validPassportCount = passports
-> Belt.Array.map(
    (passport) => {
        validFunctions
        -> Belt.List.reduce(
            true,
            (isValid, func) => isValid && func(passport)
        )
    }
)
-> Belt.Array.keep((isValid) => isValid)
-> Belt.Array.length

Js.log(validPassportCount);


// part2
let isValidByr = (passport) => {
    passport.byr->Belt.Option.isSome
    && passport.byr->Belt.Option.getExn >= 1920
    && passport.byr->Belt.Option.getExn <= 2002
}
let isValidIyr = (passport) => {
    passport.iyr->Belt.Option.isSome
    && passport.iyr->Belt.Option.getExn >= 2010
    && passport.iyr->Belt.Option.getExn <= 2020
}
let isValidEyr = (passport) => {
    passport.eyr->Belt.Option.isSome
    && passport.eyr->Belt.Option.getExn >= 2020
    && passport.eyr->Belt.Option.getExn <= 2030
}
let isValidHgt = (passport) => {
    let isValidValue = (height) => {
        let heightLength = passport.hgt->Belt.Option.getExn->Js.String.length;
        let heightUnit = String.make(1, height.[heightLength - 2]) ++ String.make(1, height.[heightLength - 1])
        switch (heightUnit) {
            | "cm" => 
                let h = height->Belt.Int.fromString
                h->Belt.Option.isSome && h->Belt.Option.getExn >= 150 && h->Belt.Option.getExn <= 193
            | "in" => 
                let h = height->Belt.Int.fromString
                h->Belt.Option.isSome && h->Belt.Option.getExn >= 59 && h->Belt.Option.getExn <= 76
            | _ => false
        }
    }

    passport.hgt->Belt.Option.isSome
    && passport.hgt->Belt.Option.getExn->Js.String.length >= 2
    && passport.hgt->Belt.Option.getExn->isValidValue
}
let isValidHcl = (passport) => {
    let isStartAtSharp = (hcl) => hcl.[0] == '#'
    let isValidColor = (colorCode) => {
        let re = Js.Re.fromString("[0-9a-f]{6}");

        colorCode->Js.String.length == 6 && re->Js.Re.test_(colorCode)
    }

    passport.hcl->Belt.Option.isSome
    && passport.hcl->Belt.Option.getExn->isStartAtSharp
    && passport.hcl->Belt.Option.getExn->Js.String.substr(~from=1)->isValidColor
}
let isValidEcl = (passport) => {
    let validEclSet = Belt.Set.String.fromArray([|"amb", "blu", "brn", "gry", "grn", "hzl", "oth"|])

    passport.ecl->Belt.Option.isSome
    && validEclSet->Belt.Set.String.has(passport.ecl->Belt.Option.getExn)
}
let isValidPid = (passport) => {
    let re = Js.Re.fromString("[0-9]{9}");

    passport.pid->Belt.Option.isSome
    && passport.pid->Belt.Option.getExn->Js.String.length == 9
    && Js.Re.test_(re, passport.pid->Belt.Option.getExn)
}

let validFunctions2 = [
    isValidByr,
    isValidIyr,
    isValidEyr,
    isValidHgt,
    isValidHcl,
    isValidEcl,
    isValidPid,
]

let validPassportCount2 = passports
-> Belt.Array.map(
    (passport) => {
        validFunctions2
        -> Belt.List.reduce(
            true,
            (isValid, func) => isValid && func(passport)
        )
    }
)
-> Belt.Array.keep((isValid) => isValid)
-> Belt.Array.length

Js.log(validPassportCount2);

