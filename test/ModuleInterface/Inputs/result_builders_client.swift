import ResultBuilders

let name = "dsl"
tuplify(true) {
  17
  3.14159
  "Hello, \(name.map { $0.uppercased() }.joined())"
  do {
    ["nested", "do"]
    1 + 2 + 3
  }
  if $0 {
    2.71828
    ["if", "stmt"]
  }
}


let array: [String] = makeArray {
  "format"
  "--in-place"
}

let dictionary: [String: Int] = makeDictionary {
  ("format", 1)
}

let ints: [Int] = makeGenericArray {
  1
  2
}
