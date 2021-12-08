import * as fs from 'fs'
import { Option, some, none } from 'fp-ts/Option'
import * as O from 'fp-ts/Option'
import * as String from 'fp-ts/string'
import { pipe } from 'fp-ts/function'
import * as ReadOnlyArray from 'fp-ts/ReadonlyArray'
import * as M from 'fp-ts/Map'
import { chainFirst } from 'fp-ts/Chain'

type Passport = {
    byr: number,
    iyr: number,
    eyr: number,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: Option<number>,
}

let optionGetExn: <T>(o: Option<T>) => T
    = o => pipe(o, O.getOrElse(() => { throw new TypeError() }))
let mapGetExn: <K, V>(m: Map<K, V>, k: K) => V
    = (m, k) => pipe(m.get(k), O.fromNullable, optionGetExn)
let mapIntGetExn: <K>(m: Map<K, string>, k: K) => number
    = (m, k) => pipe(mapGetExn(m, k), Number.parseInt)

class Parser {
    static read = (s: string) => pipe(s, String.split("\n\n"))
}

interface PassportParser {
    parser: (m: Map<string, string>) => Passport;
}

class Validator {
    static re = /(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)/g
    static parse = (raw: string) => {
        let variables = Array.from(raw.matchAll(Validator.re))
            .map((v): [string, string] => [v[1], v[2]])

        return new Map<string, string>(variables)
    }

    static toPassport = (m: Map<string, string>, parser: PassportParser): Option<Passport> => {
        try {
            return some(parser.parser(m))
        } catch {
            return none
        }
    }
}

class PassportParser1 implements PassportParser {
    parser = (m: Map<string, string>): Passport => {
        return {
            byr: mapIntGetExn(m, "byr"),
            iyr: mapIntGetExn(m, "iyr"),
            eyr: mapIntGetExn(m, "eyr"),
            hgt: mapGetExn(m, "hgt"),
            hcl: mapGetExn(m, "hcl"),
            ecl: mapGetExn(m, "ecl"),
            pid: mapGetExn(m, "pid"),
            cid: pipe(m.get("cid"), O.fromNullable, O.getOrElse(() => null)),
        }
    }
}

// let data = fs.readFileSync("../../input/2020/2020.4.sample", { encoding: "utf-8" });
let data = fs.readFileSync("../../input/2020/2020.4.input", { encoding: "utf-8" });

console.log(
    pipe(
        data,
        Parser.read,
        ReadOnlyArray.map(Validator.parse),
        ReadOnlyArray.map(
            m => Validator.toPassport(m, new PassportParser1())
        ),
        ReadOnlyArray.filter(O.isSome),
    ).length
)
