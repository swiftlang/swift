// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/pack_expansion_type_other.swift -emit-module -emit-module-path %t/pack_expansion_type_other.swiftmodule -enable-experimental-feature VariadicGenerics -disable-availability-checking
// RUN: %target-typecheck-verify-swift -I %t -enable-experimental-feature VariadicGenerics -disable-availability-checking

// Experimental features require an asserts compiler
// REQUIRES: asserts

import pack_expansion_type_other

_ = variadicFunction(t: 1, 2, u: "hi", "bye")
_ = VariadicType().variadicMethod(t: 1, "hi", u: "bye", 3)
