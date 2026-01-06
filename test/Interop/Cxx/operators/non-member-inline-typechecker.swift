// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop

import NonMemberInline

let lhs = LoadableIntWrapper(value: 42)
let rhs = LoadableIntWrapper(value: 23)

let resultPlus = lhs + rhs
let resultMinus = lhs - rhs
let resultStar = lhs * rhs
let resultSlash = lhs / rhs
let resultPercent = lhs % rhs
let resultCaret = lhs ^ rhs
let resultAmp = lhs & rhs
let resultPipe = lhs | rhs
let resultLessLess = lhs << rhs
let resultGreaterGreater = lhs >> rhs
let resultLess = lhs < rhs
let resultGreater = lhs > rhs
let resultEqualEqual = lhs == rhs
let resultExclaimEqual = lhs != rhs
let resultLessEqual = lhs <= rhs
let resultGreaterEqual = lhs >= rhs

var lhsMutable = LoadableIntWrapper(value: 42)
lhsMutable /= rhs

public func ==(ptr: UnsafePointer<UInt8>, count: Int) -> Bool {
  let lhs = UnsafeBufferPointer<UInt8>(start: ptr, count: count)
  let rhs = UnsafeBufferPointer<UInt8>(start: ptr, count: count)
  return lhs.elementsEqual(rhs, by: ==)
}


var lhsBool = LoadableBoolWrapper(value: true)
var rhsBool = LoadableBoolWrapper(value: false)

let resultAmpAmp = lhsBool && rhsBool
let resultPipePipe = lhsBool && rhsBool

let lhsRValue = RValueArithmetic(value: 123)
let rhsRValue = RValueArithmetic(value: 146)
let resultRValue = lhsRValue + rhsRValue // expected-error {{binary operator '+' cannot be applied to two 'RValueArithmetic' operands}}

let lhsLRValue = LValueAndRValueArithmetic(value: 123)
let rhsLRValue = LValueAndRValueArithmetic(value: 146)
let resultLRValue = lhsLRValue + rhsLRValue
