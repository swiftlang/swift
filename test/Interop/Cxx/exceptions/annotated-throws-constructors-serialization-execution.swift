// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-constructors.swift %t
// RUN: split-file %S/annotated-throws-constructors-serialization.swift %t
// RUN: %target-build-swift -parse-as-library -c -module-name ConstructorLibrary -emit-module -emit-module-path %t/ConstructorLibrary.swiftmodule %t/library.swift -I %t -o %t/library.o -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-build-swift %s %t/library.o -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %s %t/library.o -I %t -o %t/test-opt -O -Xfrontend -enable-default-cmo -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

import AnnotatedThrowingConstructors
import ConstructorLibrary
@_spi(CxxExceptionBridging) import Cxx

func success() throws {
  let value = try construct(11)
  let captured = try constructorReference()(13)
  let overloaded = try overloadedConstructorReference()(2.5)
  let inherited = try inheritedConstructorReference()(19)
  _ = try emptyNontrivialConstructorReference()(23)
  _ = try explicitDerivedConstructorReference()(29)
  _ = try privateConstructorReference()(31)
  let moveOnlyValue = try moveOnlyConstructorReference()(17).value
  precondition(value.value == 11)
  precondition(captured.value == 13)
  precondition(overloaded.value == 102)
  precondition(inherited.value == 19)
  precondition(moveOnlyValue == 17)
}
try success()
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)

do {
  _ = try constructorReference()(-1)
  fatalError("expected serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
do {
  _ = try emptyNontrivialConstructorReference()(-1)
  fatalError("expected empty nontrivial serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "empty nontrivial construction failed")
}
do {
  _ = try explicitDerivedConstructorReference()(-1)
  fatalError("expected explicit derived serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
do {
  _ = try privateConstructorReference()(-1)
  fatalError("expected private storage serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "private construction failed")
}
do {
  _ = try overloadedConstructorReference()(-1.5)
  fatalError("expected overloaded serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "double construction failed")
}
do {
  _ = try inheritedConstructorReference()(-1)
  fatalError("expected inherited serialized constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "construction failed")
}
do {
  _ = try moveOnlyConstructorReference()(-1)
  fatalError("expected serialized move-only constructor reference to throw")
} catch let exception as CxxException {
  precondition(exception.message == "Unknown C++ exception")
}
precondition(getLiveResults() == 0)
precondition(getLiveMembers() == 0)
