import Foundation

var x = NSUTF8StringEncoding

var d : AnyIterator<Int>

func foo1(_ a : inout [Int]) {
  a = a.sorted()
  a.append(1)
}

struct S1 {}

func foo2(_ a : inout [S1]) {
  a = a.sorted(by: { (a, b) -> Bool in
    return false
  })
  a.append(S1())
}

import Swift
func foo3(a: Float, b: Bool) {}

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %sourcekitd-test -req=cursor -pos=3:18 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-OVERLAY %s
// CHECK-OVERLAY:      source.lang.swift.ref.var.global
// CHECK-OVERLAY-NEXT: NSUTF8StringEncoding
// CHECK-OVERLAY-NEXT: s:10Foundation20NSUTF8StringEncodingSuv
// CHECK-OVERLAY-NEXT: UInt
// CHECK-OVERLAY-NEXT: $sSuD
// CHECK-OVERLAY-NEXT: Foundation
// CHECK-OVERLAY-NEXT: SYSTEM
// CHECK-OVERLAY-NEXT: <Declaration>let NSUTF8StringEncoding: <Type usr="s:Su">UInt</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-ITERATOR %s
// CHECK-ITERATOR-NOT: _AnyIteratorBase
// CHECK-ITERATOR: <Group>Collection/Type-erased</Group>

// RUN: %sourcekitd-test -req=cursor -pos=8:10 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-REPLACEMENT1 %s
// CHECK-REPLACEMENT1: <Group>Collection/Array</Group>
// CHECK-REPLACEMENT1: <Declaration>{{.*}}func sorted() -&gt; [<Type usr="s:Si">Int</Type>]</Declaration>
// CHECK-REPLACEMENT1: RELATED BEGIN
// CHECK-REPLACEMENT1: sorted(by:)</RelatedName>
// CHECK-REPLACEMENT1: RELATED END

// RUN: %sourcekitd-test -req=cursor -pos=9:8 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-REPLACEMENT2 %s
// CHECK-REPLACEMENT2: <Group>Collection/Array</Group>
// CHECK-REPLACEMENT2: <Declaration>{{.*}}mutating func append(_ newElement: __owned <Type usr="s:Si">Int</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=15:10 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-REPLACEMENT3 %s
// CHECK-REPLACEMENT3: <Group>Collection/Array</Group>
// CHECK-REPLACEMENT3: func sorted(by areInIncreasingOrder: (<Type usr="s:13cursor_stdlib2S1V">S1</Type>
// CHECK-REPLACEMENT3: sorted()</RelatedName>

// RUN: %sourcekitd-test -req=cursor -pos=18:8 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-REPLACEMENT4 %s
// CHECK-REPLACEMENT4: <Group>Collection/Array</Group>
// CHECK-REPLACEMENT4: <Declaration>{{.*}}mutating func append(_ newElement: __owned <Type usr="s:13cursor_stdlib2S1V">S1</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=21:10 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-MODULE-GROUP1 %s
// CHECK-MODULE-GROUP1: MODULE GROUPS BEGIN
// CHECK-MODULE-GROUP1-DAG: Math
// CHECK-MODULE-GROUP1-DAG: Collection
// CHECK-MODULE-GROUP1-DAG: Collection/Array
// CHECK-MODULE-GROUP1: MODULE GROUPS END

// RUN: %sourcekitd-test -req=cursor -pos=22:17 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-FLOAT1 %s
// CHECK-FLOAT1: s:Sf

// RUN: %sourcekitd-test -req=cursor -pos=22:25 %s -- %s %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-BOOL1 %s
// CHECK-BOOL1: s:Sb
