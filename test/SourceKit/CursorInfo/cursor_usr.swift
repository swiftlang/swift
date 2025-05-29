// The RUN lines are at the bottom in case we ever need to rely on line:col info.
import Foo
import FooSwiftModule

struct S1 {}

func foo(x: FooStruct1) -> S1 {}

// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %swiftc_driver -emit-module -o %t/FooSwiftModule.swiftmodule %S/Inputs/FooSwiftModule.swift

// Soundness check that we have identical responses when things work.
// RUN: %sourcekitd-test -req=cursor -pos=5:8 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s > %t.from_offset.txt
// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr2S1V" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s > %t.from_usr.txt
// RUN: %FileCheck %s -check-prefix=CHECK_SANITY1 < %t.from_offset.txt
// RUN: %FileCheck %s -check-prefix=CHECK_SANITY1 < %t.from_usr.txt
// RUN: %diff -u %t.from_usr.txt %t.from_offset.txt
// CHECK_SANITY1: source.lang.swift.decl.struct (5:8-5:10)
// CHECK_SANITY1-NEXT: S1
// CHECK_SANITY1-NEXT: s:10cursor_usr2S1
// CHECK_SANITY1-NEXT: source.lang.swift
// CHECK_SANITY1-NEXT: S1.Type
// CHECK_SANITY1-NEXT: $s10cursor_usr2S1VmD
// CHECK_SANITY1-NEXT: cursor_usr{{$}}
// CHECK_SANITY1-NEXT: <Declaration>struct S1</Declaration>
// CHECK_SANITY1-NEXT: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>S1</decl.name></decl.struct>

// Bogus USR.
// RUN: %sourcekitd-test -req=cursor -usr "s:blahblahblah" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s -check-prefix=RESOLVE
// Missing s: prefix.
// RUN: %sourcekitd-test -req=cursor -usr "10cursor_usr6globalSivp" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s -check-prefix=RESOLVE
// RESOLVE: <empty cursor info; internal diagnostic: "Unable to resolve type from USR.">

// FIXME: no support for clang USRs.
// RUN: %sourcekitd-test -req=cursor -usr "c:@S@FooStruct1" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s -check-prefix=CSUPPORT
// CSUPPORT: <empty cursor info; internal diagnostic: "Lookup for C/C++/ObjC USRs not implemented.">

// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr2S1V" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: source.lang.swift.decl.struct (5:8-5:10)
// CHECK1: s1
// CHECK1: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>S1</decl.name></decl.struct>
