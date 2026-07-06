// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -import-objc-header %S/Inputs/protocol-static-reqt-objc-class-impl.h %s -verify
// REQUIRES: objc_interop

protocol P {
	static func foo() -> Self
}

extension C: P {}

