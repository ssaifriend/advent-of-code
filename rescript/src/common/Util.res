module Int = {
  let fromStringExn = s => s->Belt.Int.fromString->Belt.Option.getExn
  let fromStringsExn = s => s->Belt.Array.keepMap(Belt.Int.fromString)
}

module Float = {
  let fromStringsExn = s => s->Belt.Array.keepMap(Belt.Float.fromString)
}

module Re = {
  let rec extract = (str, re, options) =>
    switch re->Js.Re.exec_(str) {
    | None => options
    | Some(r) =>
      let captures = r->Js.Re.captures->Belt.Array.keepMap(Js.Nullable.toOption)

      str->extract(re, list{captures, ...options})
    }
}

module Array = {
  module Int = {
    let sum = a => a->Belt.Array.reduce(0, \"+")
  }
}