// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-build-swift -parse-as-library -c -O -Xfrontend -enable-default-cmo -module-name ThrowingMembersLibrary -emit-module -emit-module-path %t/ThrowingMembersLibrary.swiftmodule %t/library.swift -I %t -o %t/library.o -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-build-swift %t/main.swift %t/library.o -I %t -o %t/test -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test
// RUN: %target-build-swift %t/main.swift %t/library.o -I %t -o %t/test-opt -O -Xfrontend -enable-default-cmo -cxx-interoperability-mode=default -Xfrontend -enable-experimental-feature -Xfrontend CxxExceptionBridging
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrowingMembers {
  header "members.h"
  requires cplusplus
}

//--- members.h
#include <stdexcept>
#define SWIFT_THROWS __attribute__((swift_attr("import_throws")))

struct Member {
  int value;
  explicit Member(int value) SWIFT_THROWS : value(value) {
    if (value < 0) throw std::runtime_error("construction failed");
  }
  int read(bool fail) const SWIFT_THROWS {
    if (fail) throw std::runtime_error("read failed");
    return value;
  }
  void update(int delta, bool fail) & SWIFT_THROWS {
    value += delta;
    if (fail) throw std::runtime_error("update failed");
  }
};
struct Derived : Member {
  using Member::Member;
};

//--- library.swift
import AnnotatedThrowingMembers

@inlinable
public func constructAndRead(_ value: CInt, fail: Bool) throws -> CInt {
  let member = try Member(value)
  return try member.read(fail)
}
@inlinable
public func constructorReference() -> (CInt) throws -> Derived {
  Derived.init
}
@inlinable
public func read(_ value: Derived, fail: Bool) throws -> CInt {
  try value.read(fail)
}
@inlinable
public func capturedRead(_ value: Derived) -> (Bool) throws -> CInt {
  value.read
}
@inlinable
public func update(_ value: inout Derived, delta: CInt, fail: Bool) throws {
  try value.update(delta, fail)
}

//--- main.swift
import AnnotatedThrowingMembers
import ThrowingMembersLibrary
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

let direct = try constructAndRead(11, fail: false)
precondition(direct == 11)
let constructor = constructorReference()
var derived = try constructor(13)
let initial = try read(derived, fail: false)
precondition(initial == 13)
let captured = capturedRead(derived)
let capturedValue = try captured(false)
precondition(capturedValue == 13)
try update(&derived, delta: 4, fail: false)
let updated = try read(derived, fail: false)
precondition(updated == 17)

expectException("construction failed") {
  _ = try constructAndRead(-1, fail: false)
}
expectException("construction failed") { _ = try constructor(-1) }
expectException("read failed") { _ = try constructAndRead(1, fail: true) }
expectException("read failed") { _ = try read(derived, fail: true) }
expectException("read failed") { _ = try captured(true) }
expectException("update failed") { try update(&derived, delta: 5, fail: true) }
precondition(derived.value == 22)
