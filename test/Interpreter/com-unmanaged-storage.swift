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

final class Holder {
  unowned(unsafe) var value: any IValue
  unowned(unsafe) var optional: (any IValue)?
  unowned(unsafe) var property: (any IProperty)?

  @inline(never)
  init(_ value: borrowing any IValue) {
    self.value = copy value
    self.optional = copy value
  }

  @inline(never)
  func replace(_ value: borrowing any IValue) {
    self.value = copy value
    self.optional = copy value
  }

  @inline(never)
  func load() -> any IValue { value }

  @inline(never)
  func loadOptional() -> (any IValue)? { optional }

  @inline(never)
  func loadProperty() -> (any IProperty)? { property }
}

@inline(never)
func checkLoad(_ holder: Holder) {
  let loaded = holder.load()
  precondition(GetForeignCOMReferenceCount() == 2)
  precondition(loaded.value(0) == 42)
  withExtendedLifetime(loaded) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}

@inline(never)
func checkOptionalLoad(_ holder: Holder) {
  let loaded = holder.loadOptional()
  precondition(GetForeignCOMReferenceCount() == 2)
  precondition(loaded!.value(0) == 42)
  withExtendedLifetime(loaded) {}
}

@inline(never)
func checkPropertyLoad(_ holder: Holder) {
  let loaded = holder.loadProperty()
  precondition(GetForeignCOMReferenceCount() == 3)
  precondition(loaded!.value == 42)
  withExtendedLifetime(loaded) {}
}

@inline(never)
func checkProperty(_ holder: Holder, _ value: borrowing any IValue) {
  let property = value as! any IProperty
  holder.property = property
  precondition(GetForeignCOMReferenceCount() == 2)
  checkPropertyLoad(holder)
  precondition(GetForeignCOMReferenceCount() == 2)
  holder.property = nil
  precondition(GetForeignCOMReferenceCount() == 2)
  withExtendedLifetime(property) {}
}

func exerciseLoads() {
  let value = makeValue(42)
  var holder: Holder? = Holder(value)
  precondition(GetForeignCOMReferenceCount() == 1)
  checkLoad(holder!)
  precondition(GetForeignCOMReferenceCount() == 1)
  checkOptionalLoad(holder!)
  precondition(GetForeignCOMReferenceCount() == 1)
  checkProperty(holder!, value)
  precondition(GetForeignCOMReferenceCount() == 1)

  holder!.optional = nil
  let addRefs = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  precondition(holder!.loadOptional() == nil)
  precondition(holder!.loadProperty() == nil)
  holder = nil
  precondition(GetForeignCOMAddRefCalls() == addRefs)
  precondition(GetForeignCOMReleaseCalls() == releases)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(value) {}
}

exerciseLoads()
checkDestruction()
print("strong loads balanced")
// CHECK: strong loads balanced

@inline(never)
func makeDanglingHolder() -> Holder {
  let value = makeValue(11)
  return Holder(value)
}

@inline(never)
func replaceDanglingValue(_ holder: Holder) {
  let value = makeValue(22)
  holder.replace(value)
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(holder.load().value(0) == 22)
  withExtendedLifetime(value) {}
}

func exerciseReplacement() {
  var holder: Holder? = makeDanglingHolder()
  checkDestruction()
  // Replacement must not release the old, already destroyed referent.
  replaceDanglingValue(holder!)
  checkDestruction()
  let addRefs = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  holder = nil
  precondition(GetForeignCOMAddRefCalls() == addRefs)
  precondition(GetForeignCOMReleaseCalls() == releases)
}

exerciseReplacement()
print("replacement and destruction balanced")
// CHECK-NEXT: replacement and destruction balanced

@inline(never)
func makeCapture(_ value: borrowing any IValue) -> () -> any IValue {
  { [unowned(unsafe) captured = copy value] in captured }
}

@inline(never)
func checkCapture(_ closure: () -> any IValue) {
  let value = closure()
  precondition(GetForeignCOMReferenceCount() == 2)
  precondition(value.value(0) == 33)
  withExtendedLifetime(value) {}
}

func exerciseCapture() {
  let value = makeValue(33)
  let closure = makeCapture(value)
  precondition(GetForeignCOMReferenceCount() == 1)
  checkCapture(closure)
  precondition(GetForeignCOMReferenceCount() == 1)
  withExtendedLifetime(closure) {}
  withExtendedLifetime(value) {}
}

exerciseCapture()
checkDestruction()
print("capture balanced")
// CHECK-NEXT: capture balanced
