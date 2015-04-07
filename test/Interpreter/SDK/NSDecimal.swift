// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

extension NSDecimal {
  init?(_ string: String) {
    self.init()
    let scanner = NSScanner(string: string)
    if !scanner.scanDecimal(&self) {
      return nil
    }
  }
}

enum NSDecimalResult: StringLiteralConvertible, Equatable, CustomStringConvertible {
  case Some(NSDecimal)
  case Error(NSCalculationError)
  
  init() {
    self = .Some(NSDecimal())
  }
  
  init(stringLiteral: String) {
    if let value = NSDecimal(stringLiteral) {
      self = .Some(value)
    } else {
      self = .Error(.LossOfPrecision)
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

  func pow10(power: Int) -> NSDecimalResult {
    switch self {
    case .Some(var decimal):
      var result = NSDecimal()
      let error = NSDecimalMultiplyByPowerOf10(&result, &decimal, Int16(power),
                                               .RoundPlain)
      if error != .NoError {
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
    return NSDecimalCompare(&x1, &x2) == .OrderedSame
  default:
    return false
  }
}

func +(x: NSDecimalResult, y: NSDecimalResult) -> NSDecimalResult {
  switch (x, y) {
  case var (.Some(x1), .Some(y1)):
    var result = NSDecimal()
    let error = NSDecimalAdd(&result, &x1, &y1, .RoundPlain)
    if error != .NoError {
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
println(zero) // CHECK: 0

let two: NSDecimalResult = "1" + "1"
println(two) // CHECK: 2

let point95: NSDecimalResult = "0.8" + "0.1" + "0.05"
println(point95) // CHECK: 0.95

let twoAgain = point95 + "1.05"
println(twoAgain) // CHECK: 2
println(two == twoAgain) // CHECK: true

println(two + "not a number") // CHECK: NaN
println(two + "not a number" == "still not a number") // CHECK: false
println(two + "not a number" == two) // CHECK: false

let one: NSDecimalResult = "1"
println(one.pow10(2)) // CHECK: 100
println(one.pow10(-2)) // CHECK: 0.01
