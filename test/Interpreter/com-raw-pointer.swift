// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -enable-builtin-module -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -enable-builtin-module -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// REQUIRES: executable_test

import Builtin
import ForeignCOM

@inline(never)
func borrow(_ value: borrowing any IValue) -> UnsafeRawPointer {
  UnsafeRawPointer(Builtin.bridgeToRawPointer(value))
}

@inline(never)
func retain(_ pointer: UnsafeRawPointer) -> any IValue {
  Builtin.bridgeFromRawPointer(pointer._rawValue)
}

@inline(never)
func retainProperty(_ pointer: UnsafeRawPointer) -> any IProperty {
  Builtin.bridgeFromRawPointer(pointer._rawValue)
}

@inline(never)
func exerciseBorrow(_ value: borrowing any IValue) {
  let retains = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  let pointer = borrow(value)
  precondition(GetForeignCOMAddRefCalls() == retains)
  precondition(GetForeignCOMReleaseCalls() == releases)
  precondition(pointer == unsafeBitCast(value, to: UnsafeRawPointer.self))
  let copied = retain(pointer)
  precondition(GetForeignCOMReferenceCount() == 2)
  precondition(copied.value(0) == 42)
  withExtendedLifetime(copied) {}
}

@inline(never)
func exerciseValue() {
  let value = makeValue(42)
  exerciseBorrow(value)
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(value.value(0) == 42)
  withExtendedLifetime(value) {}
}
exerciseValue()
checkDestruction()
print("borrow balanced")
// CHECK: borrow balanced

// The copied secondary interface survives release of the factory reference.
@inline(never)
func copyProperty() -> any IProperty {
  let object = ForeignCOMObject_Create(42)!
  let pointer = ForeignCOMObject_GetPropertyStorage(object)!.load(as: UnsafeRawPointer.self)
  let value = retainProperty(pointer)
  precondition(GetForeignCOMReferenceCount() == 2)
  ForeignCOMObject_Release(object)
  return value
}

@inline(never)
func exerciseProperty() {
  let value = copyProperty()
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(value.value == 42)
  withExtendedLifetime(value) {}
}
exerciseProperty()
checkDestruction()
print("copy balanced")
// CHECK-NEXT: copy balanced

@inline(never)
func take(_ pointer: UnsafeRawPointer) -> any IValue {
  Builtin.takeFromRawPointer(pointer._rawValue)
}

@inline(never)
func takeProperty(_ pointer: UnsafeRawPointer) -> any IProperty {
  Builtin.takeFromRawPointer(pointer._rawValue)
}

@inline(never)
func takeOptional(_ pointer: UnsafeRawPointer?) -> (any IValue)? {
  guard let pointer else { return nil }
  return .some(Builtin.takeFromRawPointer(pointer._rawValue))
}

@inline(never)
func exerciseTake() {
  let object = ForeignCOMObject_Create(42)!
  let pointer = ForeignCOMObject_GetValueStorage(object)!.load(as: UnsafeRawPointer.self)
  let value = take(pointer)
  precondition(GetForeignCOMAddRefCalls() == 0)
  precondition(GetForeignCOMReleaseCalls() == 0)
  precondition(borrow(value) == pointer)
  precondition(value.value(0) == 42)
  withExtendedLifetime(value) {}
}
exerciseTake()
checkDestruction()
print("adoption balanced")
// CHECK-NEXT: adoption balanced

// Ownership follows the secondary interface pointer, without an adjustment
// back to the primary interface or a QueryInterface call.
@inline(never)
func exerciseTakeProperty() {
  let object = ForeignCOMObject_Create(42)!
  let primary = ForeignCOMObject_GetValueStorage(object)!.load(as: UnsafeRawPointer.self)
  let secondary = ForeignCOMObject_GetPropertyStorage(object)!.load(as: UnsafeRawPointer.self)
  precondition(primary != secondary)
  let value = takeProperty(secondary)
  precondition(GetForeignCOMAddRefCalls() == 0)
  precondition(GetForeignCOMReleaseCalls() == 0)
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  precondition(UnsafeRawPointer(Builtin.bridgeToRawPointer(value)) == secondary)
  precondition(value.value == 42)
  withExtendedLifetime(value) {}
}
exerciseTakeProperty()
checkDestruction()
print("secondary adoption balanced")
// CHECK-NEXT: secondary adoption balanced

@inline(never)
func exerciseTakeOptional() {
  let object = ForeignCOMObject_Create(42)!
  let pointer = ForeignCOMObject_GetValueStorage(object)!.load(as: UnsafeRawPointer.self)
  let value = takeOptional(pointer)
  precondition(GetForeignCOMAddRefCalls() == 0)
  precondition(GetForeignCOMReleaseCalls() == 0)
  precondition(value!.value(0) == 42)
  withExtendedLifetime(value) {}
}
exerciseTakeOptional()
checkDestruction()
let retains = GetForeignCOMAddRefCalls()
let releases = GetForeignCOMReleaseCalls()
precondition(takeOptional(nil) == nil)
precondition(GetForeignCOMAddRefCalls() == retains)
precondition(GetForeignCOMReleaseCalls() == releases)
print("optional adoption balanced")
// CHECK-NEXT: optional adoption balanced
