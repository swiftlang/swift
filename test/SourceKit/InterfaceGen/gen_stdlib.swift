
var x: Int

// XFAIL: linux
// RUN: %sourcekitd-test -req=interface-gen -module Swift > %t.response
// RUN: %FileCheck -check-prefix=CHECK-STDLIB -input-file %t.response %s
// RUN: %FileCheck -check-prefix=CHECK-MUTATING-ATTR -input-file %t.response %s
// RUN: %FileCheck -check-prefix=CHECK-HIDE-ATTR -input-file %t.response %s

// Just check a small part, mainly to make sure we can print the interface of the stdlib.
// CHECK-STDLIB-NOT: extension _SwiftNSOperatingSystemVersion
// CHECK-STDLIB: struct Int : SignedInteger, Comparable, Equatable {
// CHECK-STDLIB:   static var max: Int { get }
// CHECK-STDLIB:   static var min: Int { get }
// CHECK-STDLIB: }

// Check that extensions of nested decls are showing up.
// CHECK-STDLIB-LABEL: extension String.UTF16View.Index {
// CHECK-STDLIB: func samePosition(in utf8: String.UTF8View) -> String.UTF8View.Index?
// CHECK-STDLIB: func samePosition(in unicodeScalars: String.UnicodeScalarView) -> String.UnicodeScalarIndex?
// CHECK-STDLIB: func samePosition(in characters: String) -> String.Index?
// CHECK-STDLIB-NEXT: }

// CHECK-MUTATING-ATTR: mutating func

// CHECK-HIDE-ATTR-NOT: @effects
// CHECK-HIDE-ATTR-NOT: @semantics
// CHECK-HIDE-ATTR-NOT: @inline

// RUN: %sourcekitd-test -req=interface-gen-open -module Swift \
// RUN:   == -req=cursor -pos=2:8 %s -- %s | %FileCheck -check-prefix=CHECK1 %s

// CHECK1:      source.lang.swift.ref.struct ()
// CHECK1-NEXT: Int
// CHECK1-NEXT: s:Si
// CHECK1-NEXT: Int.Type
// CHECK1-NEXT: _T0
// CHECK1-NEXT: Swift{{$}}
// CHECK1-NEXT: <Group>Math/Integers</Group>
// CHECK1-NEXT: /<interface-gen>{{$}}
// CHECK1-NEXT: SYSTEM
// CHECK1-NEXT: <Declaration>struct Int : <Type usr="s:s13SignedIntegerP">SignedInteger</Type>{{.*}}{{.*}}<Type usr="s:s10ComparableP">Comparable</Type>{{.*}}<Type usr="s:s9EquatableP">Equatable</Type>{{.*}}</Declaration>

// RUN: %sourcekitd-test -req=module-groups -module Swift | %FileCheck -check-prefix=GROUP1 %s
// GROUP1: <GROUPS>
// GROUP1-NOT: <NULL>
// GROUP1: <\GROUPS>

// RUN: %sourcekitd-test -req=interface-gen -module Swift -group-name Bool > %t.Bool.response
// RUN: %FileCheck -check-prefix=CHECK-BOOL -input-file %t.Bool.response %s
// CHECK-BOOL-DAG: extension Bool : ExpressibleByBooleanLiteral {

// These are not in the bool group:
// CHECK-BOOL-NOT: Zip2Iterator
// CHECK-BOOL-NOT: Zip2Sequence
// CHECK-BOOL-NOT: struct Int
// CHECK-BOOL-NOT: struct Float
// CHECK-BOOL-NOT: extension String

// RUN: %sourcekitd-test -req=interface-gen -module Swift -interested-usr s:Sb > %t.Bool.response
// RUN: %FileCheck -check-prefix=CHECK-BOOL -input-file %t.Bool.response %s

// RUN: %sourcekitd-test -req=interface-gen -module Swift -interested-usr s:Si > %t.Int.response
// RUN: %FileCheck -check-prefix=CHECK-INT -input-file %t.Int.response %s

// CHECK-INT: struct Int
// CHECK-INT: extension Int
// CHECK-INT-NOT: Zip2Iterator
// CHECK-INT-NOT: Zip2Sequence
// CHECK-INT-NOT: struct Bool
// CHECK-INT-NOT: struct Float

// RUN: %sourcekitd-test -req=interface-gen -module Swift -interested-usr s:Sf > %t.Float.response
// RUN: %FileCheck -check-prefix=CHECK-FLOAT -input-file %t.Float.response %s

// CHECK-FLOAT: struct Float
// CHECK-FLOAT-NOT: Zip2Iterator
// CHECK-FLOAT-NOT: Zip2Sequence
// CHECK-FLOAT-NOT: struct Bool
// CHECK-FLOAT-NOT: struct Int
