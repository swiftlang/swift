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
func copy<T: IValue>(_ value: borrowing T) -> () -> UnsafeRawPointer {
  let adds = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  let pointer = Builtin.bridgeToRawPointer(value)
  precondition(GetForeignCOMAddRefCalls() == adds)
  precondition(GetForeignCOMReleaseCalls() == releases)
  let copied: T = Builtin.bridgeFromRawPointer(pointer)
  precondition(GetForeignCOMAddRefCalls() == adds + 1)
  return { UnsafeRawPointer(Builtin.bridgeToRawPointer(copied)) }
}

@inline(never)
func copyClass<T: IProperty>(_ value: borrowing T) -> () -> UnsafeRawPointer {
  let adds = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  let pointer = Builtin.bridgeToRawPointer(value)
  precondition(GetForeignCOMAddRefCalls() == adds)
  precondition(GetForeignCOMReleaseCalls() == releases)
  let copied: T = Builtin.bridgeFromRawPointer(pointer)
  precondition(GetForeignCOMAddRefCalls() == adds + 1)
  return { UnsafeRawPointer(Builtin.bridgeToRawPointer(copied)) }
}

@inline(never)
func exerciseCopy() {
  let copy = copy(makeExtended(42))
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(GetForeignCOMDestructionCount() == 0)
  let value: any IValue = Builtin.bridgeFromRawPointer(copy()._rawValue)
  precondition(value.value(0) == 42)
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  withExtendedLifetime((copy, value)) {}
}
exerciseCopy()
checkDestruction()
print("generic copy balanced")
// CHECK: generic copy balanced

@inline(never)
func exerciseClassCopy() {
  let copy = copyClass(makeProperty(42))
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(GetForeignCOMDestructionCount() == 0)
  let value: any IProperty = Builtin.bridgeFromRawPointer(copy()._rawValue)
  precondition(value.value == 42)
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  withExtendedLifetime((copy, value)) {}
}
exerciseClassCopy()
checkDestruction()
print("generic class copy balanced")
// CHECK-NEXT: generic class copy balanced

@inline(never)
func adopt<T: IValue>(_ pointer: UnsafeRawPointer,
                      matching value: borrowing T) -> () -> UnsafeRawPointer {
  let adds = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  let owned: T = Builtin.takeFromRawPointer(pointer._rawValue)
  precondition(GetForeignCOMAddRefCalls() == adds)
  precondition(GetForeignCOMReleaseCalls() == releases)
  let borrowed = UnsafeRawPointer(Builtin.bridgeToRawPointer(value))
  precondition(borrowed == pointer)
  return { UnsafeRawPointer(Builtin.bridgeToRawPointer(owned)) }
}

@inline(never)
func adopt<T: IProperty>(_ pointer: UnsafeRawPointer,
                         matching value: borrowing T) -> () -> UnsafeRawPointer {
  let adds = GetForeignCOMAddRefCalls()
  let releases = GetForeignCOMReleaseCalls()
  let owned: T = Builtin.takeFromRawPointer(pointer._rawValue)
  precondition(GetForeignCOMAddRefCalls() == adds)
  precondition(GetForeignCOMReleaseCalls() == releases)
  let borrowed = UnsafeRawPointer(Builtin.bridgeToRawPointer(value))
  precondition(borrowed == pointer)
  return { UnsafeRawPointer(Builtin.bridgeToRawPointer(owned)) }
}

enum InterfaceKind {
  case value
  case property
}

@inline(never)
func captureAdoptedInterface(_ interface: InterfaceKind) -> () -> UnsafeRawPointer {
  // The closure owns the adopted factory reference. The borrowed exemplar
  // owns a separate reference which is released when this function returns.
  let object = ForeignCOMObject_Create(42)!
  switch interface {
  case .value:
    let storage = ForeignCOMObject_GetValueStorage(object)!
    let value = storage.load(as: (any IExtended).self)
    return adopt(storage.load(as: UnsafeRawPointer.self), matching: value)
  case .property:
    let storage = ForeignCOMObject_GetPropertyStorage(object)!
    let value = storage.load(as: (any IProperty).self)
    return adopt(storage.load(as: UnsafeRawPointer.self), matching: value)
  }
}

@inline(never)
func exerciseAdoption(of interface: InterfaceKind) {
  let captured = captureAdoptedInterface(interface)
  precondition(GetForeignCOMReferenceCount() == 1)
  precondition(GetForeignCOMDestructionCount() == 0)
  switch interface {
  case .value:
    let value: any IValue = Builtin.bridgeFromRawPointer(captured()._rawValue)
    precondition(value.value(0) == 42)
  case .property:
    let value: any IProperty = Builtin.bridgeFromRawPointer(captured()._rawValue)
    precondition(value.value == 42)
  }
  precondition(GetForeignCOMQueryInterfaceCalls() == 0)
  withExtendedLifetime(captured) {}
}
exerciseAdoption(of: .value)
checkDestruction()
exerciseAdoption(of: .property)
checkDestruction()
print("generic adoption balanced")
// CHECK-NEXT: generic adoption balanced
