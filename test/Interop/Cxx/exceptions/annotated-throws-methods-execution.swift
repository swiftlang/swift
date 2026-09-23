// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-methods.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging -Xfrontend -disable-availability-checking
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrowingMethods
import Cxx

func expectException(_ message: String, _ body: () throws -> Void) {
  do {
    try body()
    fatalError("expected exception")
  } catch let error as CxxException {
    precondition(error.message == message)
  } catch {
    fatalError("unexpected error: \(error)")
  }
}

var counter = Counter()
let initialValue = try counter.read(false)
precondition(initialValue == 10)
let unnamedValue = try counter.unnamed(123)
precondition(unnamedValue == 10)
let captured = counter.read
let capturedValue = try captured(false)
precondition(capturedValue == 10)
expectException("read failed") { _ = try captured(true) }
let addedValue = try counter.add(5, false)
precondition(addedValue == 15 && counter.value == 15)
let cleanupBefore = getMethodCleanupCount()
var didRunDefer = false
expectException("add failed") {
  defer { didRunDefer = true }
  _ = try counter.add(7, true)
}
precondition(didRunDefer)
precondition(counter.value == 22)
precondition(getMethodCleanupCount() == cleanupBefore + 1)
let taken = try counter.take(false)
precondition(taken == 22)
expectException("take failed") { _ = try counter.take(true) }
expectException("Unknown C++ exception") { try counter.clear(true) }
precondition(counter.value == 0)
let noexceptValue = try counter.getNoexceptValue()
precondition(noexceptValue == 0)

var inherited = DerivedCounter()
let inheritedResult = try inherited.add(4, false)
precondition(inheritedResult == 14)
expectException("read failed") { _ = try inherited.read(true) }
var twiceInherited = TwiceDerivedCounter()
let twiceInheritedResult = try twiceInherited.add(3, false)
precondition(twiceInheritedResult == 13)
expectException("add failed") { _ = try twiceInherited.add(4, true) }
precondition(twiceInherited.value == 17)

let qualified = QualifiedReceiver()
let qualifiedRead = try qualified.read(false)
precondition(qualifiedRead == 80)
expectException("qualified receiver failed") { _ = try qualified.read(true) }
let qualifiedTaken = try qualified.take(false)
precondition(qualifiedTaken == 80)
expectException("qualified consume failed") { _ = try qualified.take(true) }

let nontrivial = NontrivialReceiver()
let copiesBefore = getReceiverCopyCount()
let nontrivialResult = try nontrivial.checked(false)
precondition(nontrivialResult == 60)
expectException("nontrivial receiver failed") { _ = try nontrivial.checked(true) }
precondition(getReceiverCopyCount() == copiesBefore)
let nontrivialTaken = try nontrivial.take(false)
precondition(nontrivialTaken == 61)
expectException("nontrivial consume failed") { _ = try nontrivial.take(true) }

func checkNoncopyableReceiver() throws {
  var value = NoncopyableReceiver()
  let read = try value.read(false)
  precondition(read == 70)
  let added = try value.add(2)
  precondition(added == 72)
  do {
    _ = try value.read(true)
    fatalError("expected a noncopyable receiver exception")
  } catch let error as CxxException {
    precondition(error.message == "noncopyable receiver failed")
  }
  let taken = try value.take(false)
  precondition(taken == 72)
}
let destructionBefore = getNoncopyableDestructionCount()
try checkNoncopyableReceiver()
precondition(getNoncopyableDestructionCount() == destructionBefore + 1)
func checkConsumingFailure() throws {
  let value = NoncopyableReceiver()
  _ = try value.take(true)
}
expectException("noncopyable consume failed") { try checkConsumingFailure() }
precondition(getNoncopyableDestructionCount() == destructionBefore + 2)

let valueBase = ValueBase()
let baseResult = try valueBase.checked(false)
precondition(baseResult == 20)
let valueDerived = ValueDerived()
let derivedResult = try valueDerived.checked(false)
precondition(derivedResult == 30)
expectException("value derived failed") { _ = try valueDerived.checked(true) }

extension ReferenceDerived {
  func baseChecked(_ fail: Bool) throws -> CInt {
    try super.checked(fail)
  }
  func capturedBaseChecked() -> (Bool) throws -> CInt {
    super.checked
  }
}

let reference = getReferenceBase()
let dynamicResult = try reference.checked(false)
precondition(dynamicResult == 50)
expectException("reference derived failed") { _ = try reference.checked(true) }
let referenceDerived = ReferenceDerived.create()
let superResult = try referenceDerived.baseChecked(false)
precondition(superResult == 40)
expectException("reference base failed") { _ = try referenceDerived.baseChecked(true) }
let capturedSuper = referenceDerived.capturedBaseChecked()
let capturedSuperResult = try capturedSuper(false)
precondition(capturedSuperResult == 40)
expectException("reference base failed") { _ = try capturedSuper(true) }
