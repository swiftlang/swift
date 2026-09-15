// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -module-name main %s -o %t/test.exe -L %t -lCOM %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "00000000-0000-0000-0000-000000000011")
protocol ISource { }

@com(interface: "00000000-0000-0000-0000-000000000022")
protocol ITarget { }

@com(interface: "00000000-0000-0000-0000-000000000033")
protocol IMissing { }

typealias QueryInterfaceFn =
    @convention(c) (UnsafeMutableRawPointer, UnsafeRawPointer, UnsafeMutablePointer<UnsafeMutableRawPointer?>) -> Int32

typealias RefCountFn =
    @convention(c) (UnsafeMutableRawPointer) -> UInt32

var RetainCount: UInt32 = 1
var QueryInterfaceCalls = 0
var AddRefCalls = 0
var ReleaseCalls = 0

func AddRef(_: UnsafeMutableRawPointer) -> UInt32 {
  AddRefCalls += 1
  RetainCount += 1
  return RetainCount;
}

func Release(_: UnsafeMutableRawPointer) -> UInt32 {
  ReleaseCalls += 1
  RetainCount -= 1
  return RetainCount
}

func QueryInterface(_ value: UnsafeMutableRawPointer, _ iid: UnsafeRawPointer,
                    _ result: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> Int32 {
  QueryInterfaceCalls += 1

  result.pointee = nil

  guard iid.load(fromByteOffset: 15, as: UInt8.self) == 0x22 else {
    return Int32(bitPattern: 0x8000_4002)
  }

  result.pointee = value
  _ = AddRef(value)
  return 0
}

let vtable = UnsafeMutablePointer<UnsafeRawPointer?>.allocate(capacity: 3)
vtable[0] = unsafeBitCast(QueryInterface as QueryInterfaceFn,
                          to: UnsafeRawPointer.self)
vtable[1] = unsafeBitCast(AddRef as RefCountFn, to: UnsafeRawPointer.self)
vtable[2] = unsafeBitCast(Release as RefCountFn, to: UnsafeRawPointer.self)

let interface: UnsafeMutableRawPointer =
    .allocate(byteCount: MemoryLayout<UnsafeRawPointer>.stride,
              alignment: MemoryLayout<UnsafeRawPointer>.alignment)
interface.storeBytes(of: UnsafeRawPointer(vtable), as: UnsafeRawPointer.self)

do {

  let source = unsafeBitCast(interface, to: (any ISource).self)

  do {
    let (q, a, r) = (QueryInterfaceCalls, AddRefCalls, ReleaseCalls)

    let matches = source is any ITarget
    print("is: \(matches)")

    let balanced =
        QueryInterfaceCalls == q + 1 && AddRefCalls == a + 1 && ReleaseCalls == r + 1
    print("is balanced: \(balanced)")
  }

  do {
    let (q, a, r) = (QueryInterfaceCalls, AddRefCalls, ReleaseCalls)

    do {
      let result = source as? any ITarget
      print("as?: \(result != nil)")
      withExtendedLifetime(result) { }
    }

    let balanced =
        QueryInterfaceCalls == q + 1 && AddRefCalls == a + 1 && ReleaseCalls == r + 1
    print("as? balanced: \(balanced)")
  }

  do {
    let (q, a, r) = (QueryInterfaceCalls, AddRefCalls, ReleaseCalls)

    let result = source as? any IMissing
    print("failure: \(result == nil)")

    let untouched =
        QueryInterfaceCalls == q + 1 && AddRefCalls == a && ReleaseCalls == r
    print("failure untouched: \(untouched)")
  }

  do {
    let (q, a, r) = (QueryInterfaceCalls, AddRefCalls, ReleaseCalls)

    do {
      let result = source as! any ITarget
      print("as!: true")
      withExtendedLifetime(result) { }
    }

    let balanced =
        QueryInterfaceCalls == q + 1 && AddRefCalls == a + 1 && ReleaseCalls == r + 1
    print("as! balanced: \(balanced)")
  }

  do {
    let erased: Any = source
    let (q, a, r) = (QueryInterfaceCalls, AddRefCalls, ReleaseCalls)

    do {
      let result = erased as? any ITarget
      print("Any as?: \(result != nil)")
      withExtendedLifetime(result) { }
    }

    let balanced =
        QueryInterfaceCalls == q + 1 && AddRefCalls == a + 1 && ReleaseCalls == r + 1
    print("Any balanced: \(balanced)")

    withExtendedLifetime(erased) { }
  }

  withExtendedLifetime(source) { }

}

vtable.deallocate()
interface.deallocate()

// CHECK:       is: true
// CHECK-NEXT:  is balanced: true
// CHECK-NEXT:  as?: true
// CHECK-NEXT:  as? balanced: true
// CHECK-NEXT:  failure: true
// CHECK-NEXT:  failure untouched: true
// CHECK-NEXT:  as!: true
// CHECK-NEXT:  as! balanced: true
// CHECK-NEXT:  Any as?: true
// CHECK-NEXT:  Any balanced: true
