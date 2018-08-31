// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/Assert_Debug -Onone
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Release -O
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Unchecked -Ounchecked
// RUN: %target-codesign %t/Assert_Debug
// RUN: %target-codesign %t/Assert_Release
// RUN: %target-codesign %t/Assert_Unchecked
//
// RUN: %target-run %t/Assert_Debug
// RUN: %target-run %t/Assert_Release
// RUN: %target-run %t/Assert_Unchecked
// REQUIRES: executable_test

import StdlibUnittest


func returnNil() -> AnyObject? {
  return _opaqueIdentity(nil as AnyObject?)
}

var OptionalTraps = TestSuite("OptionalTraps")

OptionalTraps.test("UnwrapNone")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var a: AnyObject? = returnNil()
  expectCrashLater()
  let unwrapped: AnyObject = a!
  _blackHole(unwrapped)
}

OptionalTraps.test("UnwrapNone/Ounchecked")
  .xfail(.custom(
    { !_isFastAssertConfiguration() },
    reason: "unwrapping nil should trap unless we are in -Ounchecked mode"))
  .code {
  var a: AnyObject? = returnNil()
  expectEqual(0, unsafeBitCast(a!, to: Int.self))
}

OptionalTraps.test("UnwrapNone/Message")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "this trap may not have an error message may not be printed in -O"))
  .code {
  var a: AnyObject? = returnNil()
  expectCrashLater(withMessage: 
      "Unexpectedly found nil while unwrapping an Optional value")
  let unwrapped: AnyObject = a!
  _blackHole(unwrapped)
}

OptionalTraps.test("UnwrapNone/Message/Implicit")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .skip(.custom(
    { !_isDebugAssertConfiguration() },
    reason: "this trap may not have an error message may not be printed in -O"))
  .code {
  var a: AnyObject! = returnNil()
  expectCrashLater(withMessage: 
      "Unexpectedly found nil while implicitly unwrapping an Optional value")
  let unwrapped: AnyObject = a
  _blackHole(unwrapped)
}

runAllTests()

