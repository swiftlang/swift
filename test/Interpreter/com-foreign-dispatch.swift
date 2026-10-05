// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// REQUIRES: executable_test

import ForeignCOM

@inline(never)
func borrowed(_ interface: borrowing any IValue) -> Int32 {
  interface.value(2)
}

@inline(never)
func consuming(_ interface: consuming any IValue) -> Int32 {
  interface.value(3)
}

@inline(never)
func address(_ interface: inout any IValue) -> Int32 {
  interface.value(4)
}

func exerciseValue() {
  var interface = makeValue(40)
  precondition(borrowed(interface) == 42)
  precondition(consuming(interface) == 43)
  precondition(address(&interface) == 44)
  withExtendedLifetime(interface) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
  precondition(GetForeignCOMMethodCalls() == 3)
}

exerciseValue()
checkDestruction()
print("value dispatch balanced")
// CHECK: value dispatch balanced

func exerciseInherited() {
  let interface = makeExtended(7)
  precondition(interface.value(5) == 12)
  precondition(interface.multiply(6) == 42)
  withExtendedLifetime(interface) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
  precondition(GetForeignCOMMethodCalls() == 2)
}

exerciseInherited()
checkDestruction()
print("inherited dispatch balanced")
// CHECK-NEXT: inherited dispatch balanced

enum Stop: Error { case stop }

@inline(never)
func modifyAndThrow(_ value: inout Int32) throws {
  value = 73
  throw Stop.stop
}

func exerciseProperty() {
  let interface = makeProperty(10)
  precondition(interface.value == 10)
  interface.value = 20
  interface.value += 5
  precondition(interface[3] == 28)
  do {
    try modifyAndThrow(&interface.value)
    preconditionFailure("expected an error")
  } catch Stop.stop {
    precondition(interface.value == 73)
  } catch {
    preconditionFailure("unexpected error")
  }
  // This slot follows the accessors, so counting a synthesized _modify
  // accessor in the foreign vtable would dispatch to the wrong entry.
  interface.reset()
  precondition(interface.value == 0)
}

exerciseProperty()
checkDestruction()
print("property writeback balanced")
// CHECK-NEXT: property writeback balanced

precondition(makeValue(41).value(1) == 42)
checkDestruction()
print("temporary balanced")
// CHECK-NEXT: temporary balanced

@inline(never)
func makeBoundMethod() -> (Int32) -> Int32 {
  makeValue(40).value
}

func exerciseBoundMethod() {
  let method = makeBoundMethod()
  precondition(GetForeignCOMDestructionCount() == 0)
  precondition(method(2) == 42)
  precondition(method(3) == 43)
  withExtendedLifetime(method) {
    precondition(GetForeignCOMReferenceCount() > 0)
  }
}

exerciseBoundMethod()
checkDestruction()
print("bound method balanced")
// CHECK-NEXT: bound method balanced
