// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -emit-module-path %t/pack_expansion_type.swiftmodule -DLIB -enable-experimental-feature VariadicGenerics
// RUN: %target-swift-frontend -emit-sil %s -I %t -DAPP -module-name main -enable-experimental-feature VariadicGenerics

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

#if LIB

public func calleeWithPack<each T>(_: repeat each T) {}

public func calleeGeneric<T>(_: T) {}

@_transparent public func transparentCaller<each T>(_ t: repeat each T) {
  calleeWithPack(repeat [each t])
  calleeGeneric((repeat [each t]))
}

@_alwaysEmitIntoClient public func serializedCaller<each T>(_ t: repeat each T) {
  calleeWithPack(repeat [each t])
  calleeGeneric((repeat [each t]))
}

public func transparentCaller2() {
  transparentCaller(1, "hi", false)
}

public func calleeWithRequirement<each T: Equatable>(_: repeat each T) {}

@_transparent public func callerWithRequirement(x: Int, y: String) {
  calleeWithRequirement(x, y)
}

#elseif APP

import pack_expansion_type

transparentCaller(1, "hi", false)
serializedCaller(1, "hi", false)
callerWithRequirement(x: 1, y: "hi")

#endif
