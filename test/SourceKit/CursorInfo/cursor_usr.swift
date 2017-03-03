// The RUN lines are at the bottom in case we ever need to rely on line:col info.
import Foo
import FooSwiftModule

var global: Int

struct S1 {}

func foo(x: FooStruct1) -> S1 {}

// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swiftc_driver -emit-module -o %t/FooSwiftModule.swiftmodule %S/Inputs/FooSwiftModule.swift

// Sanity check that we have identical responses when things work.
// RUN: %sourcekitd-test -req=cursor -pos=5:5 %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s > %t.from_offset.txt
// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr6globalSiv" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s > %t.from_usr.txt
// RUN: %FileCheck %s -check-prefix=CHECK_SANITY1 < %t.from_offset.txt
// RUN: %FileCheck %s -check-prefix=CHECK_SANITY1 < %t.from_usr.txt
// RUN: diff -u %t.from_usr.txt %t.from_offset.txt
// CHECK_SANITY1: source.lang.swift.decl.var.global (5:5-5:11)
// CHECK_SANITY1-NEXT: global
// CHECK_SANITY1-NEXT: s:10cursor_usr6globalSiv
// CHECK_SANITY1-NEXT: Int
// CHECK_SANITY1-NEXT: _TtSi
// CHECK_SANITY1-NEXT: <Declaration>var global: <Type usr="s:Si">Int</Type></Declaration>
// CHECK_SANITY1-NEXT: <decl.var.global><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>global</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.global>

// Bogus USR.
// RUN: %sourcekitd-test -req=cursor -usr "s:blahblahblah" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=EMPTY
// Missing s: prefix.
// RUN: %sourcekitd-test -req=cursor -usr "10cursor_usr6globalSiv" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=EMPTY
// FIXME: no support for clang USRs.
// RUN: %sourcekitd-test -req=cursor -usr "c:@S@FooStruct1" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=EMPTY
// EMPTY: <empty cursor info>

// FIXME: missing symbol shows up as some other part of the USR (the type here).
// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr11global_noneSiv" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=SHOULD_BE_EMPTY
// SHOULD_BE_EMPTY: source.lang.swift.decl.struct ()
// SHOULD_BE_EMPTY: Int

// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr2S1V" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=CHECK1
// CHECK1: source.lang.swift.decl.struct (7:8-7:10)
// CHECK1: S1
// CHECK1: <decl.struct><syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>S1</decl.name></decl.struct>

// RUN: %sourcekitd-test -req=cursor -usr "s:14FooSwiftModule03fooB4FuncSiyF" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=CHECK2
// CHECK2: source.lang.swift.decl.function.free ()
// CHECK2: fooSwiftFunc()
// CHECK2: () -> Int
// CHECK2: FooSwiftModule
// CHECK2: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>fooSwiftFunc</decl.name>() -&gt; <decl.function.returntype><ref.struct usr="s:Si">Int</ref.struct></decl.function.returntype></decl.function.free>

// RUN: %sourcekitd-test -req=cursor -usr "s:10cursor_usr3fooAA2S1VSC10FooStruct1VF" %s -- -I %t -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | %FileCheck %s -check-prefix=CHECK3
// CHECK3: source.lang.swift.decl.function.free (9:6-9:24)
// CHECK3: foo(x:)
// CHECK3: (FooStruct1) -> S1
// CHECK3: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>x</decl.var.parameter.argument_label>: <decl.var.parameter.type><ref.struct usr="c:@S@FooStruct1">FooStruct1</ref.struct></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.struct usr="s:10cursor_usr2S1V">S1</ref.struct></decl.function.returntype></decl.function.free>
