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
