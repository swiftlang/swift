// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=40:25 %t/input.swift -- %t/input.swift > %t/actual_curry_toplevel.result
// RUN: diff -u %t/expected_curry_toplevel.result %t/actual_curry_toplevel.result
// RUN: %sourcekitd-test -req=signaturehelp -pos=44:13 %t/input.swift -- %t/input.swift > %t/actual_curry_member_partial.result
// RUN: diff -u %t/expected_curry_member_partial.result %t/actual_curry_member_partial.result
// RUN: %sourcekitd-test -req=signaturehelp -pos=49:20 %t/input.swift -- %t/input.swift > %t/actual_curry_member_full.result
// RUN: diff -u %t/expected_curry_member_full.result %t/actual_curry_member_full.result

//--- input.swift
struct Adder {
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

  func add(x: Int, y: Int, with adder: (Int, Int) throws -> Int) rethrows -> Int! {
    return adder(x, y)
  }

  func add(x: Int) -> (Int) -> Int {
    return { (y: Int) in x + y }
  }
}

func topLevelCurried(x: Int) -> (Double) -> (String) -> Void {
  fatalError()
}

func testCurryTopLevel() {
  topLevelCurried(x: 1)()
}

func testCurryMemberPartial() {
  Adder.add()
}

func testCurryMemberFull() {
  let adder = Adder()
  Adder.add(adder)()
}

//--- expected_curry_toplevel.result
{
  key.signatures: [
    {
      key.name: "(Double) -> (String) -> Void",
      key.parameters: [
        {
          key.nameoffset: 1,
          key.namelength: 6
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
//--- expected_curry_member_partial.result
{
  key.signatures: [
    {
      key.name: "add(_ self: Adder) -> (Int, Int) -> Int",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (inout Int) -> ()",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (AdditiveArithmetic, AdditiveArithmetic) -> AdditiveArithmetic",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (Double?, Float, Int) -> Double",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (Double, Float, Int) -> Double",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (Double...) -> Double",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (Int, Int, (Int, Int) throws -> Int) throws -> Int?",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(_ self: Adder) -> (Int) -> (Int) -> Int",
      key.parameters: [
        {
          key.name: "self",
          key.nameoffset: 4,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
//--- expected_curry_member_full.result
{
  key.signatures: [
    {
      key.name: "(_ x: Int, to: Int) -> Int",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 1,
          key.namelength: 8
        },
        {
          key.name: "y",
          key.nameoffset: 11,
          key.namelength: 7
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(oneTo: inout Int)",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 1,
          key.namelength: 16
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(_ x: AdditiveArithmetic, to: AdditiveArithmetic) -> AdditiveArithmetic",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 1,
          key.namelength: 23
        },
        {
          key.name: "y",
          key.nameoffset: 26,
          key.namelength: 22
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(first: Double!, second: Float, third: Int) -> Double",
      key.parameters: [
        {
          key.name: "first",
          key.nameoffset: 1,
          key.namelength: 14
        },
        {
          key.name: "second",
          key.nameoffset: 17,
          key.namelength: 13
        },
        {
          key.name: "third",
          key.nameoffset: 32,
          key.namelength: 10
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(arg1: Double, arg2: Float, arg3: Int) -> Double",
      key.parameters: [
        {
          key.name: "param1",
          key.nameoffset: 1,
          key.namelength: 12
        },
        {
          key.name: "arg2",
          key.nameoffset: 15,
          key.namelength: 11
        },
        {
          key.name: "param3",
          key.nameoffset: 28,
          key.namelength: 9
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(numbers: Double...) -> Double",
      key.parameters: [
        {
          key.name: "numbers",
          key.nameoffset: 1,
          key.namelength: 18
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(x: Int, y: Int, with: (Int, Int) throws -> Int) throws -> Int!",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 1,
          key.namelength: 6
        },
        {
          key.name: "y",
          key.nameoffset: 9,
          key.namelength: 6
        },
        {
          key.name: "adder",
          key.nameoffset: 17,
          key.namelength: 30
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "(x: Int) -> (Int) -> Int",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 1,
          key.namelength: 6
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
