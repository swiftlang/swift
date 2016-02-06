import Foundation

var x = NSUTF8StringEncoding

var d : AnyGenerator<Int>

func foo1(var a : [Int]) {
	a = a.sort()
	a.append(1)
}

struct S1 {}

func foo2(var a : [S1]) {
  a = a.sort({ (a, b) -> Bool in
    return false
  })
  a.append(S1())
}

// RUN: %sourcekitd-test -req=cursor -pos=3:18 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-OVERLAY %s
// CHECK-OVERLAY:      source.lang.swift.ref.var.global
// CHECK-OVERLAY-NEXT: NSUTF8StringEncoding
// CHECK-OVERLAY-NEXT: s:v10Foundation20NSUTF8StringEncodingSu
// CHECK-OVERLAY-NEXT: UInt
// CHECK-OVERLAY-NEXT: <Declaration>public let NSUTF8StringEncoding: <Type usr="s:Su">UInt</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-GENERATOR %s
// CHECK-GENERATOR-NOT: _AnyGeneratorBase
// CHECK-GENERATOR: <Group>ExistentialCollection</Group>

// RUN: %sourcekitd-test -req=cursor -pos=8:10 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-REPLACEMENT1 %s
// CHECK-REPLACEMENT1: <Group>CollectionAlgorithms</Group>
// CHECK-REPLACEMENT1: <Declaration>@warn_unused_result(mutable_variant=&quot;sortInPlace&quot;) func sort() -&gt; [<Type usr="s:Si">Int</Type>]</Declaration>
// CHECK-REPLACEMENT1: RELATED BEGIN
// CHECK-REPLACEMENT1: sort(@noescape _: @noescape (Int, Int) -&gt; Bool) -&gt; [Int]</RelatedName>
// CHECK-REPLACEMENT1: sort() -&gt; [Int]</RelatedName>
// CHECK-REPLACEMENT1: sort(@noescape _: @noescape (Int, Int) -&gt; Bool) -&gt; [Int]</RelatedName>
// CHECK-REPLACEMENT1: RELATED END

// RUN: %sourcekitd-test -req=cursor -pos=9:8 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-REPLACEMENT2 %s
// CHECK-REPLACEMENT2: <Group>Arrays</Group>
// CHECK-REPLACEMENT2: <Declaration>mutating func append(newElement: <Type usr="s:Si">Int</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=15:10 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-REPLACEMENT3 %s
// CHECK-REPLACEMENT3: <Group>CollectionAlgorithms</Group>
// CHECK-REPLACEMENT3: func sort(@noescape isOrderedBefore: @noescape (<Type usr="s:V13cursor_stdlib2S1">S1</Type>
// CHECK-REPLACEMENT3: sort() -&gt; [S1]</RelatedName>
// CHECK-REPLACEMENT3: sort() -&gt; [S1]</RelatedName>
// CHECK-REPLACEMENT3: sort(@noescape _: @noescape (S1, S1) -&gt; Bool) -&gt; [S1]</RelatedName>

// RUN: %sourcekitd-test -req=cursor -pos=18:8 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-REPLACEMENT4 %s
// CHECK-REPLACEMENT4: <Group>Arrays</Group>
// CHECK-REPLACEMENT4: <Declaration>mutating func append(newElement: <Type usr="s:V13cursor_stdlib2S1">S1</Type>)</Declaration>
