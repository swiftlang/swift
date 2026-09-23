// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrows
import Cxx

let directResult = try checkedDivide(12, 3)
precondition(directResult == 4)
let noexceptResult = try checkedNoexcept(8)
precondition(noexceptResult == 8)
let floatingResult = try Numbers.checked(1.5)
precondition(floatingResult == 1.5)
do {
  _ = try Numbers.checked(-1)
  fatalError("expected a namespace function to throw")
} catch let error as CxxException {
  precondition(error.message == "negative floating point value")
}
try checkedVoid(false)
let staticResult = try StaticFunctions.checked(9)
precondition(staticResult == 9)

var didRunDefer = false
do {
  defer { didRunDefer = true }
  _ = try checkedDivide(12, 0)
  fatalError("expected an exception")
} catch let error as CxxException {
  precondition(error.message == "division by zero")
}
precondition(didRunDefer)

let captured: (CInt, CInt) throws -> CInt = checkedDivide
let capturedResult = try captured(20, 4)
precondition(capturedResult == 5)
do {
  _ = try captured(20, 0)
  fatalError("expected a captured function to throw")
} catch let error as CxxException {
  precondition(error.message == "division by zero")
}

do {
  try checkedVoid(true)
  fatalError("expected a non-std exception")
} catch let error as CxxException {
  precondition(error.message == "Unknown C++ exception")
}

precondition((try? checkedDivide(12, 0)) == nil)
precondition((try? checkedDivide(12, 3)) == 4)

let cleanupCountBefore = getCleanupCount()
do {
  try throwWithCleanup()
  fatalError("expected the cleanup function to throw")
} catch let error as CxxException {
  precondition(error.message == "cleanup")
}
precondition(getCleanupCount() == cleanupCountBefore + 1)
