// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -module-name main %s -o %t/test.exe -L %t %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "32000000-0000-0000-0000-000000000001")
protocol IBase {
  func base(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com(interface: "32000000-0000-0000-0000-000000000002")
protocol IDerived: IBase {
  func derived(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com(interface: "32000000-0000-0000-0000-000000000003")
protocol IIndependent {
  func independent(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
final class COMObject: IDerived, IIndependent {
  deinit {
    print("deinit")
  }

  func base(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 1
    return 0
  }

  func derived(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 2
    return 0
  }

  func independent(_ result: UnsafeMutablePointer<Int32>?) -> Int32 {
    result?.pointee = 3
    return 0
  }
}

@inline(never)
func identity<T: IDerived>(_ value: T) -> T {
  value
}

@inline(never)
func nested<T: IDerived>(_ value: T) -> T {
  let values = [identity(value)]
  return identity(values[0])
}

@inline(never)
func base<T: IDerived>(_ value: T) -> any IBase {
  value
}

@inline(never)
func derived<T: IDerived>(_ value: T) -> any IDerived {
  value
}

@inline(never)
func independent<T: IIndependent>(_ value: T) -> any IIndependent {
  value
}

func value(pIBase value: any IBase) -> Int32 {
  var result: Int32 = -1
  _ = value.base(&result)
  return result
}

func value(pIDerived value: any IDerived) -> Int32 {
  var result: Int32 = -1
  _ = value.derived(&result)
  return result
}

func value(pIIndependent value: any IIndependent) -> Int32 {
  var result: Int32 = -1
  _ = value.independent(&result)
  return result
}

@inline(never)
func base<T: IDerived>(_ value: T, _ result: UnsafeMutablePointer<Int32>?)
    -> Int32 {
  value.base(result)
}

@inline(never)
func derived<T: IDerived>(_ value: T, _ result: UnsafeMutablePointer<Int32>?)
    -> Int32 {
  value.derived(result)
}

@inline(never)
func forward(_ value: any IDerived) -> any IDerived {
  derived(value)
}

do {
  let object = COMObject()
  let forwarded: COMObject = nested(object)
  print("identity: \(forwarded === object)")
  print("base: \(value(pIBase: base(forwarded)))")
  print("derived: \(value(pIDerived: derived(forwarded)))")
  print("independent: \(value(pIIndependent: independent(forwarded)))")
  var result: Int32 = -1
  _ = base(forwarded, &result)
  print("generic base: \(result)")
  result = -1
  _ = derived(forwarded, &result)
  print("generic derived: \(result)")
  let existential: any IDerived = forwarded
  print("existential forwarding: \(value(pIDerived: forward(existential)))")
}
print("done")

// CHECK:      identity: true
// CHECK-NEXT: base: 1
// CHECK-NEXT: derived: 2
// CHECK-NEXT: independent: 3
// CHECK-NEXT: generic base: 1
// CHECK-NEXT: generic derived: 2
// CHECK-NEXT: existential forwarding: 2
// CHECK-NEXT: deinit
// CHECK-NEXT: done
