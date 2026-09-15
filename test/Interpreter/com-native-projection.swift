// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -L %t -module-name main %s -o %t/test.exe %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "20000000-0000-0000-0000-000000000001")
protocol IBase {
  func base(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com(interface: "20000000-0000-0000-0000-000000000002")
protocol IDerived: IBase {
  func derived(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com(interface: "20000000-0000-0000-0000-000000000003")
protocol IIndependent {
  func independent(_ result: UnsafeMutablePointer<Int32>?) -> Int32
}

@com
final class CObject: IDerived, IIndependent {
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

func base(_ value: any IBase) -> Int32 {
  var result: Int32 = -1
  _ = value.base(&result)
  return result
}

func derived(_ value: any IDerived) -> Int32 {
  var result: Int32 = -1
  _ = value.derived(&result)
  return result
}

func independent(_ value: any IIndependent) -> Int32 {
  var result: Int32 = -1
  _ = value.independent(&result)
  return result
}

let object = CObject()
let d: any IDerived = object
let i: any IIndependent = object
let b: any IBase = d
let s: any ISwiftObject = object

print("base: \(base(b))")
print("derived: \(derived(d))")
print("independent: \(independent(i))")
let instance = Unmanaged.passUnretained(object).toOpaque()
print("Swift object: \(s.object == instance)")
let metadata = unsafeBitCast(CObject.self, to: UnsafeRawPointer.self)
print("Swift metadata: \(s.metadata == metadata)")

withExtendedLifetime(object) { }

// CHECK: base: 1
// CHECK-NEXT: derived: 2
// CHECK-NEXT: independent: 3
// CHECK-NEXT: Swift object: true
// CHECK-NEXT: Swift metadata: true
