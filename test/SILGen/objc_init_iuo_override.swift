// RUN: %target-swift-emit-silgen -verify -import-objc-header %S/Inputs/objc_init_iuo_override.h %s
// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/51297

class ChildClass: ParentClass {}

class GrandchildClass: ChildClass {
	// The original init is defined in objc as:
        //   - (instancetype)initWithFoo:(id<FooProtocol>)foo;
        // without nullability audits, meaning the signature in Swift is:
        //   init!(foo: FooProtocol)
        // and
	override init(foo: FooProtocol) {
		super.init(foo: foo)
	}
}

