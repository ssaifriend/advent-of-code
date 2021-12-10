import * as fs from 'fs'
import { Option, some, none } from 'fp-ts/Option'
import * as O from 'fp-ts/Option'
import * as String from 'fp-ts/string'
import { pipe } from 'fp-ts/function'
import * as ReadOnlyArray from 'fp-ts/ReadonlyArray'
import * as A from 'fp-ts/Array'
import { identity } from 'fp-ts/function'

type Passport_T = {
    byr: number,
    iyr: number,
    eyr: number,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: Option<number>,
}
type UnvalidatePassport = Passport_T & { readonly _brand: "unvalidate" }
type ValidatePassport = Passport_T & { readonly _brand: "validate" }
// type Passport = UnvalidatePassport | ValidatePassport

interface Ok<A> {
    readonly _tag: 'Ok'
    readonly value: A
}
const Ok: <A=never>(a?: A) => Ok<A> = (a) => ({_tag: 'Ok', value: a})
interface Error<A> {
    readonly _tag: 'Error'
    readonly value: A
}
const Error: <A=never>(a?: A) => Error<A> = (a) => ({_tag: 'Error', value: a})
type Result<A=never, B=never> = Ok<A> | Error<B>

let isOk: <T, U>(r: Result<T, U>) => boolean
    = r => r._tag === 'Ok'


let keepMap: <T, U>(f: (a: T) => Option<U>) => (arr: Array<T>) => Array<U>
    = f => arr => pipe(arr.map(f), A.filter(O.isSome), A.map(optionGetExn))
let optionGetExn: <T>(o: Option<T>) => T
    = o => pipe(o, O.getOrElse(() => { throw new TypeError() }))
let mapGet: <K, V>(m: Map<K, V>, k: K) => Option<V>
    = (m, k) => pipe(m.get(k), O.fromNullable)
let mapGetExn: <K, V>(k: K) => (m: Map<K, V>) => V
    = k => m => pipe(mapGet(m, k), optionGetExn)
let mapIntGetExn: <K>(k: K) => (m: Map<K, string>) => number
    = k => m => pipe(m, mapGetExn(k), Number.parseInt)


let toInt = (s: string) => O.fromNullable(Number.parseInt(s))
let arraySome: <T, U>(f: (a: T) => boolean) => (arr:Array<T>) => boolean
    = f => arr => arr.filter(f).length > 0

let read = (s: string) => pipe(s, String.split("\n\n"))

interface Validator {
    (p: UnvalidatePassport): Result<never, never>;
}

interface PassportParser {
    parser: (m: Map<string, string>) => UnvalidatePassport;
    toValidate: (p: UnvalidatePassport) => ValidatePassport;
    validators: () => Validator[];
}

class Validator {
    static re = /(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)/g
    static parse = (raw: string) => {
        let variables = Array.from(raw.matchAll(Validator.re))
            .map((v): [string, string] => [v[1], v[2]])

        return new Map<string, string>(variables)
    }

    static toPassport = (m: Map<string, string>, parser: PassportParser): Option<UnvalidatePassport> => {
        try {
            return some(parser.parser(m))
        } catch {
            return none
        }
    }

    static toValidate = (p: UnvalidatePassport, parser: PassportParser): Option<ValidatePassport> => {
        if (pipe(
            parser.validators(),
            A.map(v => v(p)),
            A.every(isOk),
        )) {
            return some(parser.toValidate(p))
        } else {
            return none
        }
    }
}

class PassportParser1 implements PassportParser {
    parser = (m: Map<string, string>): UnvalidatePassport => {
        return {
            byr: pipe(m, mapIntGetExn("byr")),
            iyr: pipe(m, mapIntGetExn("iyr")),
            eyr: pipe(m, mapIntGetExn("eyr")),
            hgt: pipe(m, mapGetExn("hgt")),
            hcl: pipe(m, mapGetExn("hcl")),
            ecl: pipe(m, mapGetExn("ecl")),
            pid: pipe(m, mapGetExn("pid")),
            cid: pipe(mapGet(m, "cid"), O.map(Number.parseInt), O.getOrElse(() => null)),
        } as UnvalidatePassport
    }
    toValidate = (p: UnvalidatePassport): ValidatePassport => {
        return {
            byr: p.byr,
            iyr: p.iyr,
            eyr: p.eyr,
            hgt: p.hgt,
            hcl: p.hcl,
            ecl: p.ecl,
            pid: p.pid,
            cid: p.cid,
        } as ValidatePassport
    }
    validators = (): Validator[] => {
        return [
            p => p.byr > 0 ? Ok() : Error(),
            p => p.iyr > 0 ? Ok() : Error(),
            p => p.eyr > 0 ? Ok() : Error(),
            p => p.hgt.length > 0 ? Ok() : Error(),
            p => p.hcl.length > 0 ? Ok() : Error(),
            p => p.ecl.length > 0 ? Ok() : Error(),
            p => p.pid.length > 0 ? Ok() : Error(),
        ]
    };
}

class PassportParser2 implements PassportParser {
    parser = (m: Map<string, string>): UnvalidatePassport => {
        return {
            byr: pipe(m, mapIntGetExn("byr")),
            iyr: pipe(m, mapIntGetExn("iyr")),
            eyr: pipe(m, mapIntGetExn("eyr")),
            hgt: pipe(m, mapGetExn("hgt")),
            hcl: pipe(m, mapGetExn("hcl")),
            ecl: pipe(m, mapGetExn("ecl")),
            pid: pipe(m, mapGetExn("pid")),
            cid: pipe(mapGet(m, "cid"), O.map(Number.parseInt), O.getOrElse(() => null)),
        } as UnvalidatePassport
    }
    toValidate = (p: UnvalidatePassport): ValidatePassport => {
        return {
            byr: p.byr,
            iyr: p.iyr,
            eyr: p.eyr,
            hgt: p.hgt,
            hcl: p.hcl,
            ecl: p.ecl,
            pid: p.pid,
            cid: p.cid,
        } as ValidatePassport
    }

    isMinLen = (s:string, l: number) => s.length > l
    isEqLen = (s:string, l: number) => s.length == l
  
    isValidByr = (p: UnvalidatePassport) => p.byr >= 1920 && p.byr <= 2002 ? Ok() : Error()
    isValidIyr = (p: UnvalidatePassport) => p.iyr >= 2010 && p.iyr <= 2020 ? Ok() : Error()
    isValidEyr = (p: UnvalidatePassport) => p.eyr >= 2020 && p.eyr <= 2030 ? Ok() : Error()
    isValidHgt = (p: UnvalidatePassport) => {
      let isValidValue = (height: string) => {
        let heightLength = p.hgt.length
        let heightUnit = height.substring(heightLength - 2, heightLength)
        let convertInt = (v: string) => optionGetExn(toInt(v))
        let between = (v:number, min:number, max:number) => v >= min && v <= max
  
        switch (heightUnit) {
            case "cm":
                return between(convertInt(height), 150, 193)
            case "in":
                return between(convertInt(height), 59, 76)
            default:
                return false
        }
      }
  
      return this.isMinLen(p.hgt, 1) && isValidValue(p.hgt) ? Ok() : Error()
    }
  
    isValidHcl = (p: UnvalidatePassport) => {
      let isStartAtSharp = (hcl: string) => hcl[0] == '#'
      let re = /[0-9a-f]{6}/
      let isValidColor = (colorCode: string) => this.isEqLen(colorCode, 6) && re.test(colorCode)
  
      return this.isMinLen(p.hcl, 0) && isStartAtSharp(p.hcl) && isValidColor(p.hcl.substr(1))
        ? Ok()
        : Error()
    }
  
    validEcl = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    isValidEcl = (p: UnvalidatePassport) => pipe(this.validEcl, arraySome(ecl => ecl == p.ecl)) ? Ok() : Error()
  
    isValidPid = (p: UnvalidatePassport) => {
        let re = /[0-9]{9}/
        return this.isEqLen(p.pid, 9) && re.test(p.pid) ? Ok() : Error()
    }
  
    validators = (): Validator[] => {
        return [
            this.isValidByr,
            this.isValidIyr,
            this.isValidEyr,
            this.isValidHgt,
            this.isValidHcl,
            this.isValidEcl,
            this.isValidPid,
        ]
    };
}

// let data = fs.readFileSync("../input/2020/2020.4.sample", { encoding: "utf-8" });
let data = fs.readFileSync("../input/2020/2020.4.input", { encoding: "utf-8" });

let parser1 = new PassportParser1()
console.log(
    pipe(
        data,
        read,
        ReadOnlyArray.map(Validator.parse),
        keepMap(
            m => Validator.toPassport(m, parser1)
        ),
        keepMap(
            p => Validator.toValidate(p, parser1)
        )
    ).length
)
let parser2 = new PassportParser2()
console.log(
    pipe(
        data,
        read,
        ReadOnlyArray.map(Validator.parse),
        keepMap(
            m => Validator.toPassport(m, parser2)
        ),
        keepMap(
            p => Validator.toValidate(p, parser2)
        )
    ).length
)
