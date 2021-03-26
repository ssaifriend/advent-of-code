let rec find = (secret, number, ~zeroCount) => {
  let str = secret ++ number->Belt.Int.toString
  let md5 = str->Digest.string->Digest.to_hex
  let zeroStr = "0"->Js.String2.repeat(zeroCount)

  md5->Js.String2.slice(~from=0, ~to_=zeroCount) == zeroStr
    ? number
    : secret->find(number + 1, ~zeroCount)
}

// part1
"abcdef"->find(0, ~zeroCount=5)->Js.log

// 609043
"pqrstuv"->find(0, ~zeroCount=5)->Js.log

// 1048970
"bgvyzdsv"->find(0, ~zeroCount=5)->Js.log

// part2
"bgvyzdsv"->find(0, ~zeroCount=6)->Js.log
