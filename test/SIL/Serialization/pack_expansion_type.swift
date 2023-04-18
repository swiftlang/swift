// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -emit-module-path %t/pack_expansion_type.swiftmodule -DLIB -enable-experimental-feature VariadicGenerics
// RUN: %target-swift-frontend -emit-sil %s -I %t -DAPP -module-name main -enable-experimental-feature VariadicGenerics

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

#if LIB

public func callee<each T>(_: repeat each T) {}

@_transparent public func caller<each T>(_ t: repeat each T) {
  callee(repeat [each t])
}

public func calleer() {
  caller(1, "hi", false)
}

#elseif APP

import pack_expansion_type

caller(1, "hi", false)

#endif
