import * as fs from 'fs'
import { pipe } from 'fp-ts/lib/function'
import * as A from 'fp-ts/Array'
import { Option, some, none } from 'fp-ts/Option'
import * as O from 'fp-ts/Option'

type Operation = {
    op: 'nop' | 'acc' | 'jmp',
    value: number,
}

let parse = (s: string): Operation => {
    let re = /(acc|jmp|nop) (\+|\-)([0-9]+)/
    let match = s.match(re);

    switch (match[1]) {
        case 'acc':
        case 'jmp':
        case 'nop':
            return ({op: match[1], value: (match[2] === '+' ? 1 : -1) * Number.parseInt(match[3])})
        default:
            throw new TypeError()
    }
}


type State = {
  sum: number,
  currentIndex: number,
  indexes: number[],
  loopState: 'loop' | 'inf' | 'complete',
  operations: Operation[],
}

let arrayGet: <T>(index: number) => (arr: T[]) => Option<T>
    = index => arr => O.fromNullable(arr[index])
let optionGetExn: <T>(o: Option<T>) => T
    = o => pipe(o, O.getOrElse(() => { throw new TypeError() }))
let arraySome: <T>(f: (a: T) => boolean) => (arr:Array<T>) => boolean
    = f => arr => arr.filter(f).length > 0

let walk = (state: State): State => {
    let v = pipe(state.operations, arrayGet(state.currentIndex))
    let newState: State;
    if (O.isSome(v)) {
        let o: Operation = optionGetExn(v)
        switch (o.op) {
            case 'nop':
                newState = {
                    ...state,
                    currentIndex: state.currentIndex + 1,
                    indexes: pipe(state.indexes, A.append(state.currentIndex)),
                }
                break;
            case 'acc':
                newState = {
                    ...state,
                    sum: state.sum + o.value,
                    currentIndex: state.currentIndex + 1,
                    indexes: pipe(state.indexes, A.append(state.currentIndex)),
                }
                break;
            case 'jmp':
                newState = {
                    ...state,
                    currentIndex: state.currentIndex + o.value,
                    indexes: pipe(state.indexes, A.append(state.currentIndex)),
                }
                break;
        }
    } else {
        newState = {...state, loopState: 'complete'}
    }

    if (newState.loopState === 'complete') {
        return newState
    } else if (pipe(state.indexes, arraySome(index => index == state.currentIndex))) {
        return {
            ...state,
            loopState: 'inf',
        }
    } else {
        return walk(newState)
    }
}


// let data = fs.readFileSync("../input/2020/2020.8.sample", { encoding: "utf-8" });
let data = fs.readFileSync("../input/2020/2020.8.input", { encoding: "utf-8" });

let operations = pipe(
    data.split("\n"),
    A.map(row => parse(row)),
)
console.log(
    walk({
        sum: 0,
        currentIndex: 0,
        indexes: [],
        loopState: 'loop',
        operations,
    }).sum
);
