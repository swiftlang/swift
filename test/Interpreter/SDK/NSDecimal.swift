// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

enum NSDecimalResult: ExpressibleByStringLiteral, Equatable, CustomStringConvertible {
  case Some(Decimal)
  case Error(Decimal.CalculationError)
  
  init() {
    self = .Some(Decimal())
  }
  
  init(stringLiteral: String) {
    if let value = Decimal(string: stringLiteral) {
      self = .Some(value)
    } else {
      self = .Error(.lossOfPrecision)
    }
  }

  init(unicodeScalarLiteral: String) {
    self.init(stringLiteral: unicodeScalarLiteral)
  }
  init(extendedGraphemeClusterLiteral: String) {
    self.init(stringLiteral: extendedGraphemeClusterLiteral)
  }

  var description: String {
    switch self {
    case .Some(var decimal):
      return NSDecimalString(&decimal, nil)
    case .Error:
      return "NaN"
    }
  }

  func pow10(_ power: Int) -> NSDecimalResult {
    switch self {
    case .Some(var decimal):
      var result = Decimal()
      let error = NSDecimalMultiplyByPowerOf10(&result, &decimal, Int16(power),
                                               .plain)
      if error != .noError {
        return .Error(error)
      } else {
        return .Some(result)
      }
    case .Error:
      return self
    }
  }

}

func ==(x: NSDecimalResult, y: NSDecimalResult) -> Bool {
  switch (x, y) {
  case var (.Some(x1), .Some(x2)):
    return NSDecimalCompare(&x1, &x2) == .orderedSame
  default:
    return false
  }
}

func +(x: NSDecimalResult, y: NSDecimalResult) -> NSDecimalResult {
  switch (x, y) {
  case var (.Some(x1), .Some(y1)):
    var result = Decimal()
    let error = NSDecimalAdd(&result, &x1, &y1, .plain)
    if error != .noError {
      return .Error(error)
    } else {
      return .Some(result)
    }
    
  case let (.Error(error), _):
    return .Error(error)
    
  case let (_, .Error(error)):
    return .Error(error)

  // FIXME rdar://problem/19165412
  default:
    fatalError("impossible")
  }
}

let zero = NSDecimalResult()
print(zero) // CHECK: 0

let two: NSDecimalResult = "1" + "1"
print(two) // CHECK: 2

let point95: NSDecimalResult = "0.8" + "0.1" + "0.05"
print(point95) // CHECK: 0.95

let twoAgain = point95 + "1.05"
print(twoAgain) // CHECK: 2
print(two == twoAgain) // CHECK: true

print(two + "not a number") // CHECK: NaN
print(two + "not a number" == "still not a number") // CHECK: false
print(two + "not a number" == two) // CHECK: false

let one: NSDecimalResult = "1"
print(one.pow10(2)) // CHECK: 100
print(one.pow10(-2)) // CHECK: 0.01

var twenty = Decimal(20)
var ten = Decimal(10)
twenty *= ten
print(twenty) // CHECK: 200

twenty = Decimal(20)
ten = Decimal(10)
twenty /= ten
print(twenty) // CHECK: 2

twenty = NSDecimalNumber(mantissa: 2, exponent: 1, isNegative: false) as Decimal
print(twenty.significand) // CHECK: 2
print(twenty.exponent) // CHECK: 1

print(Decimal(sign: .plus, exponent: -2, significand: 100)) // CHECK: 1
