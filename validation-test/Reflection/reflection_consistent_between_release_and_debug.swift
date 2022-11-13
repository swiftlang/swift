// RUN: %empty-directory(%t)

// RUN: %target-build-swift -target %target-cpu-apple-macosx99.99 -g -enable-upcoming-feature OptInReflection %s -o %t/debug.out
// RUN: %target-codesign %t/debug.out
// RUN: %target-run %t/debug.out | %FileCheck %s --check-prefix=CHECK-DEBUG

// RUN: %target-build-swift -target %target-cpu-apple-macosx99.99 -Osize -enable-upcoming-feature OptInReflection %s -o %t/release.out
// RUN: %target-codesign %t/release.out
// RUN: %target-run %t/release.out | %FileCheck %s --check-prefix=CHECK-RELEASE

// REQUIRES: executable_test

// output of stdlib's reflection-consuming APIs should be consistent across debug and release settings

public enum Foo: Reflectable {
	case A(Int)
	case B
}

public struct Bar {
	let a: Int
	let b: String
}


debugPrint(Foo.A(123))
// CHECK-DEBUG-DAG: debug.Foo.A(123)
// CHECK-RELEASE-DAG: release.Foo.A(123)

debugPrint(Bar(a: 999, b: "bar"))
// CHECK-DEBUG-DAG: debug.Bar()
// CHECK-RELEASE-DAG: release.Bar()
