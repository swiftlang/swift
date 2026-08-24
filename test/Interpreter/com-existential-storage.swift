// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -module-name main %s -o %t/test.exe -L %t %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "36000000-0000-0000-0000-000000000001")
protocol IValue {
  func load(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
final class StoredObject: IValue {
  let value: Int32

  init(_ value: Int32) {
    self.value = value
  }

  deinit {
    print("deinit: \(value)")
  }

  func load(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = value
    return 0
  }
}

@inline(never)
func load(_ value: any IValue) -> Int32 {
  var result: Int32 = -1
  _ = value.load(&result)
  return result
}

@inline(never)
func makeCapture(_ value: Int32) -> () -> Int32 {
  let interface: any IValue = StoredObject(value)
  return { load(interface) }
}

@inline(never)
func makeArray(_ value: Int32) -> [any IValue] {
  let interface: any IValue = StoredObject(value)
  return [interface, interface]
}

@inline(never)
func makeDictionary(_ value: Int32) -> [Int: any IValue] {
  let interface: any IValue = StoredObject(value)
  return [1: interface]
}

@inline(never)
func makeOptional(_ value: Int32) -> (any IValue)? {
  let interface: any IValue = StoredObject(value)
  return interface
}

do {
  let closure = makeCapture(11)
  withExtendedLifetime(closure) {
    print("capture: \(closure())")
  }
}

do {
  let values = makeArray(22)
  var copy = values
  copy.removeLast()
  withExtendedLifetime(values) {
    withExtendedLifetime(copy) {
      print("array: \(values.reduce(0) { $0 + load($1) })")
      print("array copy: \(load(copy[0]))")
    }
  }
}

do {
  let valuesByKey = makeDictionary(33)
  withExtendedLifetime(valuesByKey) {
    print("dictionary: \(load(valuesByKey[1]!))")
  }
}

do {
  let optional = makeOptional(44)
  withExtendedLifetime(optional) {
    print("optional: \(load(optional!))")
  }
}
print("done")

// CHECK:      capture: 11
// CHECK-NEXT: deinit: 11
// CHECK-NEXT: array: 44
// CHECK-NEXT: array copy: 22
// CHECK-NEXT: deinit: 22
// CHECK-NEXT: dictionary: 33
// CHECK-NEXT: deinit: 33
// CHECK-NEXT: optional: 44
// CHECK-NEXT: deinit: 44
// CHECK-NEXT: done
