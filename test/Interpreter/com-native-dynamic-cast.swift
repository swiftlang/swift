// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(COM)) -Xfrontend -enable-experimental-com-interop -module-name COM -module-link-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-build-swift -Xfrontend -enable-experimental-com-interop -I %t -module-name main %s -o %t/test.exe -L %t %target-rpath(%t)
// RUN: %target-codesign %t/test.exe %t/%target-library-name(COM)
// RUN: %target-run %t/test.exe | %FileCheck %s

// REQUIRES: executable_test

@com(interface: "30000000-0000-0000-0000-000000000001")
protocol ISource {
}

protocol P: AnyObject {
}

var deinits = 0

@com
final class CSource: ISource, P {
  deinit { deinits += 1 }
}

final class C {
}

final class D {
}

do {
  let native = CSource()

  do {
    let erased: Any = native
    let projected = erased as? any ISource
    print("Any(native) -> COM: \(projected == nil ? false : true)")
    withExtendedLifetime(projected) { }
  }

  do {
    let dynamic: AnyObject = native
    let projected = dynamic as? any ISource
    print("dynamic native -> COM: \(projected == nil ? false : true)")
    withExtendedLifetime(projected) { }
  }

  let source: any ISource = native

  do {
    let recovered = source as? CSource
    print("COM -> native class: \(recovered == nil ? false : true)")
    withExtendedLifetime(recovered) { }
  }

  do {
    let recovered = source as! CSource
    print("COM -> native class [forced]: \(recovered === native)")
    withExtendedLifetime(recovered) { }
  }

  print("COM is native type: \(source is CSource)")

  let erased: Any = source
  print("COM unrelated cast: \(erased as? C == nil)")

  do {
    let recovered = source as? any P
    print("COM -> Swift protocol: \(recovered == nil ? false : true)")
    withExtendedLifetime(recovered) { }
  }

  let d: Any = D()
  print("non-COM conversion: \(d as? any ISource == nil)")
  withExtendedLifetime(d) { }
}

print("native ownership balanced: \(deinits == 1)")

typealias pfnQueryInterface =
    @convention(c) (UnsafeMutableRawPointer, UnsafeRawPointer, UnsafeMutablePointer<UnsafeMutableRawPointer?>) -> Int32
typealias pfnAddRef =
    @convention(c) (UnsafeMutableRawPointer) -> UInt32
typealias pfnRelease =
    @convention(c) (UnsafeMutableRawPointer) -> UInt32

var counts: (query: Int, acquire: Int, release: Int) = (0, 0, 0)

func QueryInterface(_ pUnk: UnsafeMutableRawPointer, _ riid: UnsafeRawPointer,
                    _ ppvObject: UnsafeMutablePointer<UnsafeMutableRawPointer?>)
    -> Int32 {
  counts.query += 1
  ppvObject.pointee = nil
  return E_NOINTERFACE
}

func AddRef(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  counts.acquire += 1
  return 2
}

func Release(_ pUnk: UnsafeMutableRawPointer) -> UInt32 {
  counts.release += 1
  return 1
}

let lpVtbl = UnsafeMutablePointer<UnsafeRawPointer?>.allocate(capacity: 3)
lpVtbl[0] = unsafeBitCast(QueryInterface as pfnQueryInterface, to: UnsafeRawPointer.self)
lpVtbl[1] = unsafeBitCast(AddRef as pfnAddRef, to: UnsafeRawPointer.self)
lpVtbl[2] = unsafeBitCast(Release as pfnRelease, to: UnsafeRawPointer.self)

let pvObject = UnsafeMutableRawPointer.allocate(byteCount: MemoryLayout<UnsafeRawPointer>.stride,
                                                alignment: MemoryLayout<UnsafeRawPointer>.alignment)
pvObject.storeBytes(of: UnsafeRawPointer(lpVtbl), as: UnsafeRawPointer.self)

do {
  let foreign = unsafeBitCast(pvObject, to: (any ISource).self)
  let queries = counts.query
  print("foreign COM -> native failure: \(foreign as? C == nil)")
  print("foreign queried ISwiftObject: \(counts.query == queries + 1)")
  withExtendedLifetime(foreign) { }
}

print("foreign ownership balanced: \(counts.acquire == counts.release)")
lpVtbl.deallocate()
pvObject.deallocate()

// CHECK:      Any(native) -> COM: true
// CHECK-NEXT: dynamic native -> COM: true
// CHECK-NEXT: COM -> native class: true
// CHECK-NEXT: COM -> native class [forced]: true
// CHECK-NEXT: COM is native type: true
// CHECK-NEXT: COM unrelated cast: true
// CHECK-NEXT: COM -> Swift protocol: true
// CHECK-NEXT: non-COM conversion: true
// CHECK-NEXT: native ownership balanced: true
// CHECK-NEXT: foreign COM -> native failure: true
// CHECK-NEXT: foreign queried ISwiftObject: true
// CHECK-NEXT: foreign ownership balanced: true
