// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=23:5 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
func add(_ x: Int = 10, to y: Int) -> Int {}

func add(oneTo x: inout Int) {}

func add(_ x: Int, to y: Int? = nil) -> String {}

func add(first: Double!, second: Float = .pi, third: Int) -> Double {}

struct S {
  let a: Bool
}

func add(s: S = S(a: false)) -> Double {}

func add(x: Int, y: Int, with adder: (Int, Int) -> Int = { $0 + $1 }) -> Int {}

let importantValue = 42

func add(x: Int = importantValue) {}

func add(x: Int, line: UInt = #line, file: StaticString = #file) {}

add()

//--- expected.result
{
  key.signatures: [
    {
      key.name: "add(_ x: Int = 10, to: Int) -> Int",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 13
        },
        {
          key.nameoffset: 19,
          key.namelength: 7
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(oneTo: inout Int)",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 16
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ x: Int, to: Int? = nil) -> String",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 8
        },
        {
          key.nameoffset: 14,
          key.namelength: 14
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(first: Double!, second: Float = .pi, third: Int) -> Double",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 14
        },
        {
          key.nameoffset: 20,
          key.namelength: 19
        },
        {
          key.nameoffset: 41,
          key.namelength: 10
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(s: S = S(a: false)) -> Double",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 18
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(x: Int, y: Int, with: (Int, Int) -> Int = { $0 + $1 }) -> Int",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 6
        },
        {
          key.nameoffset: 12,
          key.namelength: 6
        },
        {
          key.nameoffset: 20,
          key.namelength: 37
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(x: Int = importantValue)",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 23
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(x: Int, line: UInt = #line, file: StaticString = #file)",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 6
        },
        {
          key.nameoffset: 12,
          key.namelength: 18
        },
        {
          key.nameoffset: 32,
          key.namelength: 26
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
