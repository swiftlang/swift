// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-constructors.swift %t
// RUN: %target-build-swift %s -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s -I %t -o %t/test-opt -O -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrowingConstructors
@_spi(CxxExceptionBridging) import Cxx

func checkSuccess() throws {
  let value = try Checked(CInt(17))
  precondition(value.value == 17)
  let constructor: (CInt) throws -> Checked = Checked.init
  let captured = try constructor(29)
  precondition(captured.value == 29)
  let inherited = try Inherited(CInt(19))
  precondition(inherited.value == 19)
  let derived = try ExplicitDerived(23)
  precondition(derived.value == 23)
  _ = try PrivateValue(47)
  _ = try EmptyNontrivial(53)
  let emptyMoveOnlyConstructor: (CInt) throws -> EmptyMoveOnly = EmptyMoveOnly.init
  _ = try emptyMoveOnlyConstructor(59)
  let fallback = try CopyFallback(31)
  precondition(fallback.value == 31)
  let aligned = try Aligned(37)
  precondition(aligned.value == 37)
  let moveOnlyValue = try MoveOnly(41).value
  precondition(moveOnlyValue == 41)
  let moveOnlyConstructor: (CInt) throws -> MoveOnly = MoveOnly.init
  let capturedMoveOnlyValue = try moveOnlyConstructor(43).value
  precondition(capturedMoveOnlyValue == 43)
  let zero = try ZeroInitialized()
  precondition(zero.value == 0)
  _ = try NoexceptConstruction(false)
  let box = try IntBox(61)
  precondition(box.value == 61)
}

try checkSuccess()
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

let resultDestructions = getResultDestructions()
let memberDestructions = getMemberDestructions()
var didRunDefer = false
do {
  defer { didRunDefer = true }
  _ = try Checked(CInt(-1))
  fatalError("expected construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
precondition(didRunDefer)
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 1)
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

do {
  _ = try MoveOnly(-1)
  fatalError("expected move-only construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "Unknown C++ exception")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 2)
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

do {
  _ = try Partial(true)
  fatalError("expected member construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "member construction failed")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 3)
precondition(getLiveMembers() == 0)

do {
  _ = try Inherited(CInt(-1))
  fatalError("expected inherited construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 4)
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

do {
  _ = try ExplicitDerived(-1)
  fatalError("expected derived construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 5)
do {
  _ = try PrivateValue(-1)
  fatalError("expected private storage construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "private construction failed")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 6)
do {
  _ = try EmptyNontrivial(-1)
  fatalError("expected empty nontrivial construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "empty nontrivial construction failed")
}
do {
  _ = try EmptyMoveOnly(-1)
  fatalError("expected empty move-only construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "empty move-only construction failed")
}
precondition(getResultDestructions() == resultDestructions)
precondition(getMemberDestructions() == memberDestructions + 6)
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

let emptyConstructor = Empty.init
do {
  _ = try emptyConstructor()
  fatalError("expected empty construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "empty failed")
}

do {
  _ = try Unnamed(1)
  fatalError("expected constructor with unnamed parameter to throw")
} catch let exception as CxxException {
  precondition(exception.message == "Unknown C++ exception")
}

do {
  _ = try IntBox(-1)
  fatalError("expected class template specialization construction to throw")
} catch let exception as CxxException {
  precondition(exception.message == "box construction failed")
}
