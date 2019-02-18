// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %sourcekitd-test -req=index %s -- %s -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck %s

import Foo

func foo(a: FooClassDerived) {
  a.fooInstanceFunc0()
  fooFunc1(0)
}

// CHECK:      key.kind: source.lang.swift.import.module.clang
// CHECK-NEXT: key.name: "Foo"
// CHECK-NEXT: key.filepath: "{{.*[/\\]}}Foo{{.*}}.pcm"
// CHECK-NOT: key.hash:

// CHECK:      key.kind: source.lang.swift.ref.module
// CHECK-NEXT: key.name: "Foo"
// CHECK-NEXT: key.usr: "c:@M@Foo"

// CHECK:      key.kind: source.lang.swift.ref.class
// CHECK-NEXT: key.name: "FooClassDerived"
// CHECK-NEXT: key.usr: "c:objc(cs)FooClassDerived"

// CHECK:      key.kind: source.lang.swift.ref.function.method.instance
// CHECK-NEXT: key.name: "fooInstanceFunc0()"
// CHECK-NEXT: key.usr: "c:objc(cs)FooClassDerived(im)fooInstanceFunc0"

// CHECK:      key.kind: source.lang.swift.ref.function.free
// CHECK-NEXT: key.name: "fooFunc1(_:)"
// CHECK-NEXT: key.usr: "c:@F@fooFunc1"
