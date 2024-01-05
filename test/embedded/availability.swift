// Building with regular Swift should succeed
// RUN: %target-swift-emit-ir %s -parse-stdlib  -wmo

// Building with embedded Swift should produce unavailability errors
// RUN: %target-typecheck-verify-swift -parse-stdlib -enable-experimental-feature Embedded  -wmo

// REQUIRES: swift_in_compiler

@_unavailableInEmbedded
public func embedded() { }
public func regular() {
	embedded() // expected-error {{'embedded()' is unavailable: unavailable in embedded Swift}}
	// expected-note@-3 {{'embedded()' has been explicitly marked unavailable here}}
}

@_unavailableInEmbedded
public func unused() { } // no error

@_unavailableInEmbedded
public func called_from_unavailable() { }
@_unavailableInEmbedded
public func also_embedded() { 
	called_from_unavailable() // no error
}
