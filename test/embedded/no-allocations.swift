// RUN: %target-swift-emit-ir %s -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -no-allocations -wmo -verify -verify-ignore-unknown

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public class X {} // expected-error {{cannot use allocating type 'X' in -no-allocations mode}}
public func use_a_class() -> X {
	let x = X() // expected-note {{instance of type created here}}
	return x
}

public func use_an_array() -> Int {
	let a = [1, 2, 3] // expected-error {{cannot use allocating type '_ContiguousArrayStorage<Int>' in -no-allocations mode}}
	return a.count
}

public func use_unsafepointer_allocate() -> UnsafeMutablePointer<UInt8> {
	return UnsafeMutablePointer<UInt8>.allocate(capacity: 10) // expected-error {{cannot use allocating operation in -no-allocations mode}}
}
