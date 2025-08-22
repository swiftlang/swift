// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=33:5 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
func add(_ x: Int, to y: Int) -> Int {
  return x + y
}

func add(oneTo x: inout Int) {
  x += 1
}

func add<T: AdditiveArithmetic>(_ x: T, to y: T) -> T {
  return x + y
}

func add(first: Double!, second: Float, third: Int) -> Double {
  return first + Double(second) + Double(third)
}

func add(arg1 param1: Double, arg2: Float, arg3 param3: Int) -> Double {
  return param1 + Double(arg2) + Double(param3)
}

func add(numbers: Double...) -> Double {
  return numbers.reduce(into: 0) { $0 += $1 }
}

func add(x: Int, y: Int, with adder: (Int, Int) -> Int) -> Int {
  return adder(x, y)
}

func add(x: Int) -> (Int) -> Int {
  return { (y: Int) in x + y }
}

add()

//--- expected.result
{
  key.signatures: [
    {
      key.name: "add(_ x: Int, to: Int) -> Int",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 8
        },
        {
          key.nameoffset: 14,
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
      key.name: "add(_ x: AdditiveArithmetic, to: AdditiveArithmetic) -> AdditiveArithmetic",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 23
        },
        {
          key.nameoffset: 29,
          key.namelength: 22
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(first: Double!, second: Float, third: Int) -> Double",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 14
        },
        {
          key.nameoffset: 20,
          key.namelength: 13
        },
        {
          key.nameoffset: 35,
          key.namelength: 10
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(arg1: Double, arg2: Float, arg3: Int) -> Double",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 12
        },
        {
          key.nameoffset: 18,
          key.namelength: 11
        },
        {
          key.nameoffset: 31,
          key.namelength: 9
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(numbers: Double...) -> Double",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 18
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(x: Int, y: Int, with: (Int, Int) -> Int) -> Int",
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
          key.namelength: 23
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(x: Int) -> (Int) -> Int",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 6
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
