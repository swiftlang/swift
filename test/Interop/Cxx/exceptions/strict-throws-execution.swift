// RUN: %empty-directory(%t)
// RUN: split-file %S/strict-throws.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -cxx-exception-mode=strict -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import Cxx
import StrictThrows

let result = try checkedValue(42)
precondition(result == 42)
let captured = StaticApi.checked
let capturedResult = try captured(7)
precondition(capturedResult == 7)
let noThrowResult = try annotatedNoexcept(11)
precondition(noThrowResult == 11)
precondition(noThrow(12) == 12)
precondition(dynamicNone(13) == 13)
precondition(StaticApi.safe(14) == 14)
precondition(SafeTemplate.get(20) == 20)
let conditionalResult = try ThrowingTemplate.get(21)
precondition(conditionalResult == 21)
precondition(safeWithDefault(15) == 15)
let explicitArgument = try checkedWithDefault(16)
precondition(explicitArgument == 16)
let callback = safeCallback()!
precondition(callback(17) == 17)
precondition(cFunction(18) == 18)
precondition(cStaticFunction(19) == 19)
cCallback(nil)

var didRunDefer = false
do {
  defer { didRunDefer = true }
  _ = try checkedValue(-1)
  fatalError("expected a native C++ exception")
} catch let error as CxxException {
  precondition(error.message == "negative value")
}
precondition(didRunDefer)
do {
  try unknownException()
  fatalError("expected an unknown native C++ exception")
} catch let error as CxxException {
  precondition(error.message == "Unknown C++ exception")
}
let failedResult = try? mayThrow(-1)
precondition(failedResult == nil)
let conditionalFailure = try? ThrowingTemplate.get(-1)
precondition(conditionalFailure == nil)
