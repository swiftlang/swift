// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %t/Lib.swift -module-name Lib
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift -module-to-print Lib -I %t | %FileCheck %s --check-prefix LIB
// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %t/main.swift -I %t | %FileCheck %s

//--- Lib.swift

public protocol P {}
public protocol Q {}
public protocol R {}

public typealias X = Q & R

public struct S: X & P {}
// LIB:      0:0 | protocol/Swift | Q | s:3Lib1QP | Ref,RelBase | rel: 1
// LIB-NEXT:   RelBase | struct/Swift | S | s:3Lib1SV
// LIB-NEXT: 0:0 | protocol/Swift | R | s:3Lib1RP | Ref,RelBase | rel: 1
// LIB-NEXT:   RelBase | struct/Swift | S | s:3Lib1SV
// LIB-NEXT: 0:0 | protocol/Swift | P | s:3Lib1PP | Ref,RelBase | rel: 1
// LIB-NEXT:   RelBase | struct/Swift | S | s:3Lib1SV

public protocol NonCopyableProto: ~Copyable {}
public struct NonCopyable: NonCopyableProto & ~Copyable {}
// LIB:      0:0 | protocol/Swift | NonCopyableProto | s:3Lib16NonCopyableProtoP | Ref,RelBase | rel: 1
// LIB-NEXT:   RelBase | struct/Swift | NonCopyable | s:3Lib11NonCopyableV

// We don't currently have a relation for Copyable.
// LIB-NOT: s:s8CopyableP

extension [Int] {
  public func foo() {}
}
// LIB:      0:0 | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3fooyyF | Def | rel: 0
// LIB-NEXT: 0:0 | struct/Swift | Array | s:Sa | Ref,RelExt | rel: 1
// LIB-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3fooyyF

extension Array where Element == Int {
  public func bar() {}
}
// LIB:      0:0 | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3baryyF | Def | rel: 0
// LIB-NEXT: 0:0 | struct/Swift | Array | s:Sa | Ref,RelExt | rel: 1
// LIB-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3baryyF

extension Int? {
  public func baz() {}
}
// LIB:      0:0 | extension/ext-enum/Swift | Optional | s:e:s:Sq3LibSiRszlE3bazyyF | Def | rel: 0
// LIB-NEXT: 0:0 | enum/Swift | Optional | s:Sq | Ref,RelExt | rel: 1
// LIB-NEXT:   RelExt | extension/ext-enum/Swift | Optional | s:e:s:Sq3LibSiRszlE3bazyyF

public typealias IntArray = [Int]
public typealias ArrayOf<T> = [T]

extension IntArray {
  public func qux() {}
}
// We don't currently report references to typealiases in imported modules, so
// this is just an extension of Array.
// LIB:      0:0 | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3quxyyF | Def | rel: 0
// LIB-NEXT: 0:0 | struct/Swift | Array | s:Sa | Ref,RelExt | rel: 1
// LIB-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE3quxyyF

extension ArrayOf<Int> {
  public func flim() {}
}
// We don't currently report references to typealiases in imported modules, so
// this is just an extension of Array.
// LIB:      0:0 | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE4flimyyF | Def | rel: 0
// LIB-NEXT: 0:0 | struct/Swift | Array | s:Sa | Ref,RelExt | rel: 1
// LIB-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa3LibSiRszlE4flimyyF

//--- main.swift

import Lib

struct K: P & X {}
// CHECK:      [[@LINE-1]]:11 | protocol/Swift | P | s:3Lib1PP | Ref,RelBase | rel: 1
// CHECK-NEXT:   RelBase | struct/Swift | K | s:14swift_ide_test1KV
// CHECK-NEXT: [[@LINE-3]]:15 | type-alias/Swift | X | s:3Lib1Xa | Ref | rel: 0
// CHECK-NEXT: [[@LINE-4]]:15 | protocol/Swift | Q | s:3Lib1QP | Ref,Impl,RelBase | rel: 1
// CHECK-NEXT:   RelBase | struct/Swift | K | s:14swift_ide_test1KV
// CHECK-NEXT: [[@LINE-6]]:15 | protocol/Swift | R | s:3Lib1RP | Ref,Impl,RelBase | rel: 1
// CHECK-NEXT:   RelBase | struct/Swift | K | s:14swift_ide_test1KV

extension IntArray {
  // CHECK:      [[@LINE-1]]:11 | extension/ext-struct/Swift | Array | s:e:s:Sa14swift_ide_testSiRszlE4flamyyF | Def | rel: 0
  // CHECK-NEXT: [[@LINE-2]]:11 | type-alias/Swift | IntArray | s:3Lib8IntArraya | Ref | rel: 0
  // CHECK-NEXT: [[@LINE-3]]:11 | struct/Swift | Array | s:Sa | Ref,Impl,RelExt | rel: 1
  // CHECK-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa14swift_ide_testSiRszlE4flamyyF
  func flam() {}
}

extension ArrayOf<Int> {
  // CHECK:      [[@LINE-1]]:11 | extension/ext-struct/Swift | Array | s:e:s:Sa14swift_ide_testSiRszlE4bishyyF | Def | rel: 0
  // CHECK-NEXT: [[@LINE-2]]:11 | type-alias/Swift | ArrayOf | s:3Lib7ArrayOfa | Ref | rel: 0
  // CHECK-NEXT: [[@LINE-3]]:11 | struct/Swift | Array | s:Sa | Ref,Impl,RelExt | rel: 1
  // CHECK-NEXT:   RelExt | extension/ext-struct/Swift | Array | s:e:s:Sa14swift_ide_testSiRszlE4bishyyF
  func bish() {}
}
