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
const Ok: <A>(a?: A) => Ok<A> = (a) => ({_tag: 'Ok', value: a})
interface Error<A> {
    readonly _tag: 'Error'
    readonly value: A
}
const Error: <A>(a?: A) => Error<A> = (a) => ({_tag: 'Error', value: a})
type Result<A, B> = Ok<A> | Error<B>

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
    // TODO why?...
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
