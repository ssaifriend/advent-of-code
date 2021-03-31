let pp = data => data->Js.String2.split("\n")->Belt.Array.map(d => d->Js.String2.split(""))

type count = {
  twice: array<string>,
  three: array<string>,
}
let countTwiceThree = arrs => {
  let getBySize = (g, size) =>
    g->Belt.Map.String.keep((_, v) => v->Belt.Array.size == size)->Belt.Map.String.keysToArray

  arrs->Belt.Array.map(arr => {
    let g = arr->Garter.Array.String.groupBy(s => s)
    let twice = g->getBySize(2)
    let three = g->getBySize(3)

    {twice: twice, three: three}
  })
}
let multiply = counts => {
  let count = (cs, f) => cs->Belt.Array.keep(f)->Belt.Array.size
  let twice = counts->count(c => c.twice->Garter.Array.isEmpty != true)
  let three = counts->count(c => c.three->Garter.Array.isEmpty != true)

  twice * three
}

// sample
"abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"->pp->countTwiceThree->multiply->Js.log

let raw = Node.Fs.readFileAsUtf8Sync("input/2018/2018.2.input")
raw->pp->countTwiceThree->multiply->Js.log


let rec differentCount = (cs, arrs) => {
  let len = cs->Belt.Array.size
  let countArrs = arrs->Belt.Array.map(arr => {
    let cs = cs->Belt.Array.keepWithIndex((c, i) => arr[i] == c)
    (cs, len - cs->Belt.Array.size)
  })
  ->Belt.Array.keep(((_, c)) => c == 1)
  
  switch (countArrs->Belt.Array.get(0)) {
  | Some((cs, _)) => cs->Js.Array2.joinWith("")
  | None => differentCount(arrs[0], arrs->Belt.Array.sliceToEnd(1))
  }
}

// sample
let arrs = 
"abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz"->pp
differentCount(arrs[0], arrs->Belt.Array.sliceToEnd(1))->Js.log

let arrs = raw->pp
differentCount(arrs[0], arrs->Belt.Array.sliceToEnd(1))->Js.log