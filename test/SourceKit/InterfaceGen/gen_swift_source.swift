// XFAIL: linux
// RUN: %sourcekitd-test -req=interface-gen %S/Inputs/Foo2.swift -- %S/Inputs/Foo2.swift %mcp_opt %clang-importer-sdk > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=interface-gen-open %S/Inputs/Foo2.swift -- %S/Inputs/Foo2.swift %mcp_opt %clang-importer-sdk \
// RUN: == -req=cursor -pos=18:49 | %FileCheck -check-prefix=CHECK1 %s
// The cursor points to 'FooOverlayClassBase' inside the list of base classes, see 'gen_swift_source.swift.response'

// CHECK1: FooOverlayClassBase
// CHECK1: s:4Foo219FooOverlayClassBaseC
// CHECK1: FooOverlayClassBase.Type

// RUN: %sourcekitd-test -req=interface-gen %S/Inputs/UnresolvedExtension.swift -- %S/Inputs/UnresolvedExtension.swift | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: extension ET

// RUN: %sourcekitd-test -req=interface-gen %S/Inputs/Foo3.swift -- %S/Inputs/Foo3.swift | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: public class C {
