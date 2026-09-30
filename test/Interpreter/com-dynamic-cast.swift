// RUN: %empty-directory(%t)
// RUN: %target-clang -x c %S/Inputs/ForeignCOM/ForeignCOM.c -c -o %t/ForeignCOM.o
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name COMCasts -sil-verify-all -emit-object %S/../Inputs/COMCasts.sil -o %t/COMCasts.o
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o %t/COMCasts.o -L %t -lCOM %target-rpath(%t) -o %t/test
// RUN: %target-codesign %t/test %t/%target-library-name(COM)
// RUN: %target-run %t/test %t/%target-library-name(COM) | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-experimental-com-interop -Xfrontend -sil-verify-all -I %t -I %S/Inputs/ForeignCOM %S/Inputs/ForeignCOM.swift %t/main.swift %t/ForeignCOM.o %t/COMCasts.o -L %t -lCOM %target-rpath(%t) -o %t/test-opt
// RUN: %target-codesign %t/test-opt
// RUN: %target-run %t/test-opt %t/%target-library-name(COM) | %FileCheck %s
// REQUIRES: executable_test

import ForeignCOM

@com(interface: "10000000-0000-0000-0000-000000000004")
protocol IMissing {}

@com(interface: "10000000-0000-0000-0000-000000000005")
protocol IClassValue: IValue, AnyObject {}

@inline(never)
func conditional(_ source: borrowing any IValue) -> (any IProperty)? {
  source as? any IProperty
}

@inline(never)
func forced(_ source: borrowing any IValue) -> any IProperty {
  source as! any IProperty
}

@inline(never)
func consuming(_ source: consuming any IValue) -> (any IProperty)? {
  source as? any IProperty
}

@inline(never)
func erased(_ source: borrowing Any) -> (any IProperty)? {
  source as? any IProperty
}

@inline(never)
func generic<T>(_ source: borrowing T, _: T.Type) -> (any IProperty)? {
  source as? any IProperty
}

@inline(never)
func genericForced<T>(_ source: borrowing T, _: T.Type) -> any IProperty {
  source as! any IProperty
}

@inline(never)
func classBound(_ source: borrowing any IProperty) -> (any IClassValue)? {
  source as? any IClassValue
}

@inline(never)
func optionalSource(_ source: (any IValue)?) -> (any IProperty)? {
  source as? any IProperty
}

@inline(never)
func optionalTarget(_ source: borrowing any IValue) -> (any IProperty)?? {
  source as? (any IProperty)?
}

@inline(never)
func matches(_ source: borrowing any IValue) -> Bool {
  source is any IProperty
}

@inline(never)
func missing(_ source: borrowing any IValue) -> (any IMissing)? {
  source as? any IMissing
}

@inline(never)
func pattern(_ source: borrowing any IValue) -> Int32 {
  switch source {
  case let property as any IProperty where property.value > 0:
    return property.value
  default:
    return source.value(1)
  }
}

// Preserve the closed existential in Any; direct erasure currently opens the
// interface and requires the separate opened-archetype metadata work.
@inline(never)
func box<T>(_ source: T, _: T.Type) -> Any {
  source
}

func exerciseCasts() {
  let source = makeValue(40)
  let property = conditional(source)!
  // The property vtable asserts that it receives the adjusted interface
  // pointer. Returning the original value pointer cannot dispatch this call.
  precondition(property.value == 40)
  property.value = 42
  precondition(source.value(0) == 42)
  precondition((source as! any IExtended).multiply(2) == 84)
  precondition(forced(source).value == 42)
  precondition(generic(source, (any IValue).self)!.value == 42)
  precondition(genericForced(source, (any IValue).self).value == 42)
  let boxed = box(source, (any IValue).self)
  precondition(erased(boxed)!.value == 42)
  precondition(optionalSource(source)!.value == 42)
  precondition(optionalTarget(source)!!.value == 42)

  let before = GetForeignCOMQueryInterfaceCalls()
  precondition(matches(source))
  precondition(GetForeignCOMQueryInterfaceCalls() == before + 1)
  precondition(missing(source) == nil)
  precondition(GetForeignCOMQueryInterfaceCalls() == before + 2)
  precondition(optionalSource(nil) == nil)
  precondition(GetForeignCOMQueryInterfaceCalls() == before + 2)

  precondition(pattern(source) == 42)
  property.value = -2
  precondition(pattern(source) == -1)
  precondition(source.value(0) == -2)
  withExtendedLifetime((source, property, boxed)) {
    precondition(GetForeignCOMDestructionCount() == 0)
  }
}

exerciseCasts()
checkDestruction()
print("interface casts balanced")
// CHECK: interface casts balanced

// Both interfaces are class-bound. A borrowed cast must not consume the
// source reference, and the returned interface must remain independently owned.
func exerciseClassBound() {
  let property = makeProperty(41)
  let value = classBound(property)!
  precondition(value.value(1) == 42)
  precondition(property.value == 41)
  property.value = 43
  precondition(value.value(0) == 43)
}

exerciseClassBound()
checkDestruction()
print("class-bound cast balanced")
// CHECK-NEXT: class-bound cast balanced

func exerciseConsumed() {
  let property = consuming(makeValue(42))!
  precondition(property.value == 42)
}

exerciseConsumed()
checkDestruction()
print("consumed source balanced")
// CHECK-NEXT: consumed source balanced

@inline(never)
func escapedProperty() -> any IProperty {
  let source = makeValue(42)
  return forced(source)
}

func exerciseEscaped() {
  let property = escapedProperty()
  precondition(property.value == 42)
  precondition(GetForeignCOMDestructionCount() == 0)
}

exerciseEscaped()
checkDestruction()
print("escaped result balanced")
// CHECK-NEXT: escaped result balanced

func exerciseIdentity() {
  let property = makeProperty(42)
  let value = classBound(property)!
  let first = property as! any IUnknown
  let second = value as! any IUnknown
  precondition(unsafeBitCast(first, to: UnsafeRawPointer.self) ==
               unsafeBitCast(second, to: UnsafeRawPointer.self))
  precondition(property.value == 42)
  precondition(value.value(0) == 42)
}

exerciseIdentity()
checkDestruction()
print("IUnknown identity balanced")
// CHECK-NEXT: IUnknown identity balanced

// These entry points exercise scalar SIL directly, independently of which
// cast representation Swift source lowering and optimization choose.
@_silgen_name("com_cast_conditional")
func scalarConditional(_ source: consuming any IValue) -> (any IProperty)?
@_silgen_name("com_cast_forced")
func scalarForced(_ source: consuming any IValue) -> any IProperty
@_silgen_name("com_cast_optional")
func scalarOptional(_ source: consuming (any IValue)?) -> (any IProperty)?

func exerciseScalarConditional() {
  let property = scalarConditional(makeValue(42))!
  precondition(property.value == 42)
}
exerciseScalarConditional()
checkDestruction()
print("scalar conditional balanced")
// CHECK-NEXT: scalar conditional balanced

func exerciseScalarForced() {
  let property = scalarForced(makeValue(42))
  precondition(property.value == 42)
}
exerciseScalarForced()
checkDestruction()
print("scalar forced balanced")
// CHECK-NEXT: scalar forced balanced

func exerciseScalarOptional() {
  let property = scalarOptional(makeValue(42))!
  precondition(property.value == 42)
  let before = GetForeignCOMQueryInterfaceCalls()
  precondition(scalarOptional(nil) == nil)
  precondition(GetForeignCOMQueryInterfaceCalls() == before)
}
exerciseScalarOptional()
checkDestruction()
print("scalar optional balanced")
// CHECK-NEXT: scalar optional balanced

@_silgen_name("com_cast_missing")
func scalarMissing(_ source: consuming any IValue) -> (any IMissing)?

precondition(scalarMissing(makeValue(42)) == nil)
precondition(GetForeignCOMQueryInterfaceCalls() == 1)
checkDestruction()
print("scalar failure balanced")
// CHECK-NEXT: scalar failure balanced
