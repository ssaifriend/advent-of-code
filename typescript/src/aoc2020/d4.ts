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
let mapGet: <K, V>(m:Map<K, V>, k:K) => Option<V>
    = (m, k) => pipe(m.get(k), O.fromNullable)
let mapGetExn: <K, V>(k: K) => (m: Map<K, V>) => V
    = k => m => pipe(mapGet(m, k), optionGetExn)
let mapIntGetExn: <K>(k: K) => (m: Map<K, string>) => number
    = k => m => pipe(m, mapGetExn(k), Number.parseInt)

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
            byr: pipe(m, mapIntGetExn("byr")),
            iyr: pipe(m, mapIntGetExn("iyr")),
            eyr: pipe(m, mapIntGetExn("eyr")),
            hgt: pipe(m, mapGetExn("hgt")),
            hcl: pipe(m, mapGetExn("hcl")),
            ecl: pipe(m, mapGetExn("ecl")),
            pid: pipe(m, mapGetExn("pid")),
            cid: pipe(mapGet(m, "cid"), O.map(Number.parseInt), O.getOrElse(() => null)),
        }
    }
}

// let data = fs.readFileSync("../input/2020/2020.4.sample", { encoding: "utf-8" });
let data = fs.readFileSync("../input/2020/2020.4.input", { encoding: "utf-8" });

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
