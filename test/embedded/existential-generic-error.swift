// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public protocol MyProtocol: AnyObject {
    func foo<T: BinaryInteger>(ptr: UnsafeMutableRawPointer?, value: T)
}

func test_some(p: some MyProtocol) {
    p.foo(ptr: nil, value: 0) // expected-error {{a protocol type cannot contain a generic method 'foo(ptr:value:)' in embedded Swift}}
}

public func test_any(p: any MyProtocol) {
    test_some(p: p)
}
