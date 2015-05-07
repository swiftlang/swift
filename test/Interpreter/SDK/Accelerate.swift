// RUN: %target-run-simple-swift
//
// vU1024Divide() is only available on OS X.
// REQUIRES: OS=macosx

import Accelerate

extension vU1024: IntegerLiteralConvertible, CustomStringConvertible, Equatable {
  public init(var integerLiteral: Int) {
    self.init()
    memcpy(&self, &integerLiteral, sizeof(Int.self))
  }

  init(_ int: Int) {
    self.init(integerLiteral: int)
  }

  public var description: String {
    if self == 0 {
      return "0"
    }
    var digits: [Character] = []
    var intermediate = self
    var digit: vU1024 = 0
    repeat {
      (intermediate, digit) = quorem(intermediate, 10)
      digits.append(Character(UnicodeScalar(Int(digit) + 48)))
    } while intermediate != 0
    return String(digits.reverse())
  }
}

extension Int {
  init(var _ u1024: vU1024) {
    // NB: Doesn't overflow check
    self.init()
    memcpy(&self, &u1024, sizeof(Int.self))
  }
}

func *(var x: vU1024, var y: vU1024) -> vU1024 {
  var result = vU1024()
  vU1024HalfMultiply(&x, &y, &result)
  return result
}

func quorem(var x: vU1024, var _ y: vU1024) -> (vU1024, vU1024) {
  var quo = vU1024()
  var rem = vU1024()
  vU1024Divide(&x, &y, &quo, &rem)
  return (quo, rem)
}

public func ==(var x: vU1024, var y: vU1024) -> Bool {
  return memcmp(&x, &y, sizeof(vU1024.self)) == 0
}

func factorial(x: Int) -> vU1024 {
  var result: vU1024 = 1

  for i in 1...x {
    result = result * vU1024(i)
  }

  return result
}

// CHECK: 7257415615307998967396728211129263114716991681296451376543577798900561843401706157852350749242617459511490991237838520776666022565442753025328900773207510902400430280058295603966612599658257104398558294257568966313439612262571094946806711205568880457193340212661452800000000000000000000000000000000000000000
println(factorial(170))

