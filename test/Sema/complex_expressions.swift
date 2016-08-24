// RUN: %target-parse-verify-swift

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
// DictionaryExpr has too complex to be solved in resonable time
import Foundation

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
    "cos": .unaryOperation(cos),
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
