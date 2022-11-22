// RUN: %empty-directory(%t)

// RUN: %target-build-swift -target %target-cpu-apple-macosx99.99 -DOPTIN -g -enable-upcoming-feature OptInReflection %s -o %t/opt_in.out
// RUN: %target-codesign %t/opt_in.out
// RUN: %target-run %t/opt_in.out | %FileCheck %s --check-prefix=CHECK-OPT-IN

// RUN: %target-build-swift -enable-full-reflection-metadata -target %target-cpu-apple-macosx99.99 -DFULL %s -o %t/full.out
// RUN: %target-codesign %t/full.out
// RUN: %target-run %t/full.out | %FileCheck %s --check-prefix=CHECK-FULL

// REQUIRES: executable_test

public enum Foo: Reflectable {
	case A(Int)
	case B
}

public struct Bar {
	let a: Int
	let b: String
}

public func consume<T: Reflectable>(_ t: T) {
	debugPrint(t)
}

public func consumeOptional<T: Reflectable>(_ t: T?) {
	debugPrint(t as Any)
}

public func consumeOptionalOptional<T: Reflectable>(_ t: T??) {
	debugPrint(t as Any)
}

public func castToReflectable<T>(_ t: T) -> Reflectable? {
	return t as? Reflectable
}

debugPrint(castToReflectable(Foo.A(123)))
// CHECK-OPT-IN-DAG: Optional(opt_in.Foo.A(123))
// CHECK-FULL-DAG: Optional(full.Foo.A(123))

debugPrint(Foo.A(123))
// CHECK-OPT-IN-DAG: opt_in.Foo.A(123)
// CHECK-FULL-DAG: full.Foo.A(123)

debugPrint(Bar(a: 999, b: "bar"))
// CHECK-OPT-IN-DAG: opt_in.Bar()
// CHECK-FULL-DAG: full.Bar(a: 999, b: "bar")

consumeOptional(Foo.A(123) as? Reflectable)
// CHECK-OPT-IN-DAG: Optional(opt_in.Foo.A(123))
// CHECK-FULL-DAG: Optional(full.Foo.A(123))

consumeOptionalOptional(Foo.A(123) as? Reflectable?)
// CHECK-OPT-IN-DAG: Optional(Optional(opt_in.Foo.A(123)))
// CHECK-FULL-DAG: Optional(Optional(full.Foo.A(123)))

print(Foo.A(123) is Reflectable)
// CHECK-OPT-IN-DAG: true
// CHECK-FULL-DAG: true

consume(Foo.A(123) as! Reflectable)
// CHECK-OPT-IN-DAG: opt_in.Foo.A(123)
// CHECK-FULL-DAG: full.Foo.A(123)

consumeOptional(Bar(a: 999, b: "bar") as? Reflectable)
// CHECK-OPT-IN-DAG: nil
// CHECK-FULL-DAG: Optional(full.Bar(a: 999, b: "bar"))

print(Bar(a: 999, b: "bar") is Reflectable)
// CHECK-OPT-IN-DAG: false
// CHECK-FULL-DAG: true

#if FULL
consume(Bar(a: 999, b: "bar") as! Reflectable)
// CHECK-FULL-DAG: full.Bar(a: 999, b: "bar")
#endif
