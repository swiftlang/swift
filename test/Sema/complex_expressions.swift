// RUN: %target-typecheck-verify-swift

// SR-139:
// Infinite recursion parsing bitwise operators
let x = UInt32(0x1FF)&0xFF << 24 | UInt32(0x1FF)&0xFF << 16 | UInt32(0x1FF)&0xFF << 8 | (UInt32(0x1FF)&0xFF);

// SR-838:
// expression test_seconds() was too complex to be solved in reasonable time
struct Nano : CustomStringConvertible {
  var value: Int64 = 0
  init(_ x: Int64) { self.value = x }
  var description: String { return "\(self.value)" }
}

func *(lhs: Nano, rhs: Int) -> Nano { return Nano(lhs.value * Int64(rhs)); }
func *(lhs: Int, rhs: Nano) -> Nano { return Nano(Int64(lhs) * rhs.value); }
func +(lhs: Nano, rhs: String) -> String { return lhs.description + rhs; }
func +(lhs: String, rhs: Nano) -> String { return lhs + rhs.description; }
func +(lhs: Nano, rhs: Nano) -> Nano { return Nano(lhs.value + rhs.value); }

let u_nano = Nano(1)
let u_second = Nano(1_000_000_00)
let u_minute = u_second * 60

extension Int {
  var ns: Nano { return Nano(Int64(self)) }
  var s: Nano { return self * u_second }
  var i: Nano { return self * u_minute }
}

func test_seconds() {
  print("Testing second operations:\n")
  print(u_minute + u_second + Nano(500) + " = " + 1.i + 1.s + 500.ns)
  print((u_minute + u_second + Nano(500)) + " = " + (1.i + 1.s + 500.ns))
}

// SR-2102:
// DictionaryExpr was too complex to be solved in reasonable time

let M_PI: Double = 3.1415926535897931
let M_E : Double = 2.7182818284590451

func sqrt(degrees: Int) -> Double {
  return 0
}

func sqrt(degrees: Float) -> Double {
  return 0
}

func sqrt(degrees: Double) -> Double {
  return 0
}

enum Operation {
  case constant(Double)
  case unaryOperation((Double) -> Double)
  case binaryOperation((Double, Double) -> Double)
  case equals
}

var operations: Dictionary<String, Operation> = [
  "π": .constant(M_PI),
  "e": .constant(M_E),
  "√": .unaryOperation(sqrt),
  "×": .binaryOperation({ (op1: Double, op2: Double) in
    return op1 * op2
  }),
  "÷": .binaryOperation({ (op1, op2) in
    return op1 / op2
  }),
  "+": .binaryOperation({ (op1: Double, op2: Double) in
    return op1 + op2
  }),
  "−": .binaryOperation({ (op1, op2) in
    return op1 - op2
  }),
  "=": .equals,
]

// SR-1794
struct P {
  let x: Float
  let y: Float
}

func sr1794(pt: P, p0: P, p1: P) -> Bool {
  return (pt.x - p0.x) * (p1.y - p0.y) - (pt.y - p0.y) * (p1.x - p0.x) < 0.0
}

// Tests for partial contextual type application in sub-expressions

let v1 = (1 - 2 / 3 * 6) as UInt
let v2 = (([1 + 2 * 3, 4, 5])) as [UInt]
let v3 = ["hello": 1 + 2, "world": 3 + 4 + 5 * 3] as Dictionary<String, UInt>
let v4 = [1 + 2 + 3, 4] as [UInt32] + [2 * 3] as [UInt32]
let v5 = ([1 + 2 + 3, 4] as [UInt32]) + ([2 * 3] as [UInt32])
let v6 = [1 + 2 + 3, 4] as Set<UInt32>
let v7: [UInt32] = [55 * 8, 0]

// SR-3668
// "Expression was too complex" errors for short dictionary literals
// of simple closure expressions

let sr3668Dict1: Dictionary<Int, (Int, Int) -> Bool> =
  [  0: { $0 == $1 },  1: { $0 == $1 },  2: { $0 == $1 },  3: { $0 == $1 },
     4: { $0 == $1 },  5: { $0 == $1 },  6: { $0 == $1 },  7: { $0 == $1 },
     8: { $0 == $1 },  9: { $0 == $1 }, 10: { $0 == $1 }, 11: { $0 == $1 },
    12: { $0 == $1 }, 13: { $0 == $1 }, 14: { $0 == $1 }, 15: { $0 == $1 },
    16: { $0 == $1 }, 17: { $0 == $1 }, 18: { $0 == $1 }, 19: { $0 == $1 } ]

let sr3668Dict2: [Int: (Int, Int) -> Bool] =
  [  0: { $0 != $1 },  1: { $0 != $1 },  2: { $0 != $1 },  3: { $0 != $1 },
     4: { $0 != $1 },  5: { $0 != $1 },  6: { $0 != $1 },  7: { $0 != $1 },
     8: { $0 != $1 },  9: { $0 != $1 }, 10: { $0 != $1 }, 11: { $0 != $1 },
    12: { $0 != $1 }, 13: { $0 != $1 }, 14: { $0 != $1 }, 15: { $0 != $1 },
    16: { $0 != $1 }, 17: { $0 != $1 }, 18: { $0 != $1 }, 19: { $0 != $1 } ]
