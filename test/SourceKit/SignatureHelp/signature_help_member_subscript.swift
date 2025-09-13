// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=16:8 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct Matrix {
  subscript(row: Int, column: Int) -> Int {
    return 0
  }
  
  subscript(row r: Int) -> [Int] {
    return []
  }
  
  subscript(column c: Int) -> [Int] {
    return []
  }
}

let matrix = Matrix()
matrix[]

//--- expected.result
{
  key.signatures: [
    {
      key.name: "subscript(keyPath: KeyPath<Matrix, Value>) -> Value",
      key.parameters: [
        {
          key.nameoffset: 10,
          key.namelength: 31
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "subscript(row: Int, column: Int) -> Int",
      key.parameters: [
        {
          key.name: "row",
          key.nameoffset: 10,
          key.namelength: 8
        },
        {
          key.name: "column",
          key.nameoffset: 20,
          key.namelength: 11
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "subscript(row: Int) -> [Int]",
      key.parameters: [
        {
          key.name: "r",
          key.nameoffset: 10,
          key.namelength: 8
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "subscript(column: Int) -> [Int]",
      key.parameters: [
        {
          key.name: "c",
          key.nameoffset: 10,
          key.namelength: 11
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
