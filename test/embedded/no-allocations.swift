// RUN: %target-swift-emit-ir %s -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -wmo

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/ArrayShared.swift':48{{cannot use allocating type '_ContiguousArrayStorage<Int>' in -no-allocations mode}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/SwiftNativeNSArray.swift':501{{cannot use allocating type '__SwiftNativeNSArrayWithContiguousStorage' in -no-allocations mode}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/ContiguousArrayBuffer.swift':361{{cannot use allocating type '_ContiguousArrayStorage<Int>' in -no-allocations mode}}" >> %t/main.swift
// RUN: echo "// expected-error@'%swift_src_root/stdlib/public/core/UnsafePointer.swift':831{{cannot use allocating operation in -no-allocations mode}}" >> %t/main.swift
// RUN: %target-swift-emit-ir %t/main.swift -enable-experimental-feature Embedded -no-allocations -wmo -verify -verify-ignore-unknown

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

//--- main.swift

public class X {} // expected-error {{cannot use allocating type 'X' in -no-allocations mode}}
public func use_a_class() -> X {
	let x = X() // expected-note {{instance of type created here}}
	return x
}

public func use_an_array() -> Int {
	let a = [1, 2, 3] // expected-note {{generic specialization called here}}
	return a.count
}

public func use_unsafepointer_allocate() -> UnsafeMutablePointer<UInt8> {
	return UnsafeMutablePointer<UInt8>.allocate(capacity: 10) // expected-note {{generic specialization called here}}
}
