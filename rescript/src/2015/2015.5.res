let hasLeastThreeVowels = str => {
  let vowels = "aeiou"->Js.String2.split("")

  str
  ->Js.String2.split("")
  ->Belt.Array.keep(s => vowels->Belt.Array.some(vowel => vowel == s))
  ->Belt.Array.size >= 3
}

let hasLeastOneLetterTwice = str => {
  let str1 = str->Js.String2.split("")
  let str2 = str1->Belt.Array.sliceToEnd(1)->Belt.Array.concat([""])

  Belt.Array.zip(str1, str2)->Belt.Array.keep(((str1, str2)) => str1 == str2)->Belt.Array.size > 0
}

let hasNotContainString = str => {
  let invalidStrings = ["ab", "cd", "pq", "xy"]

  invalidStrings
  ->Belt.Array.keep(invalidString => str->Js.String2.includes(invalidString))
  ->Belt.Array.size == 0
}

let isNice = str =>
  str->hasLeastThreeVowels && (str->hasLeastOneLetterTwice && str->hasNotContainString)

"ugknbfddgicrmopn"->isNice->Js.log

//T
"aaa"->isNice->Js.log

//T
"jchzalrnumimnmhp"->isNice->Js.log

//F
"haegwjzuvuyypxyu"->isNice->Js.log

//F
"dvszwmarrgswjxmb"->isNice->Js.log

//F
Node.Fs.readFileAsUtf8Sync("input/2015/2015.5.input")
->Js.String2.split("\n")
->Belt.Array.keep(isNice)
// ->Belt.Array.map(Js.log)
->Belt.Array.size
->Js.log

let hasLeastTwoLetterTwice = str =>
  Belt.Array.range(0, str->Js.String2.length - 4)
  ->Belt.Array.map(index => str->Js.String2.slice(~from=index, ~to_=index + 2))
  ->Belt.Array.keepWithIndex((findStr, index) =>
    str->Js.String2.sliceToEnd(~from=index + 2)->Js.String2.includes(findStr)
  )
  ->Belt.Array.size > 0

let rec hasOneLetterRepeat = (str, ~index) =>
  switch (str[index], str->Belt.Array.get(index + 2)) {
  | (_, None) => false
  | (a, Some(b)) if a == b => true
  | _ => str->hasOneLetterRepeat(~index=index + 1)
  }

let isBetterNice = str =>
  str->hasLeastTwoLetterTwice && str->Js.String2.split("")->hasOneLetterRepeat(~index=0)

"qjhvhtzxzqqjkmpb"->isBetterNice->Js.log

//T
"xxyxx"->isBetterNice->Js.log

//T
"uurcxstgmygtbstg"->isBetterNice->Js.log

//F
"ieodomkazucvgmuy"->isBetterNice->Js.log

//F
Node.Fs.readFileAsUtf8Sync("input/2015/2015.5.input")
->Js.String2.split("\n")
->Belt.Array.keep(isBetterNice)

// ->Belt.Array.map(Js.log)
->Belt.Array.size
->Js.log
