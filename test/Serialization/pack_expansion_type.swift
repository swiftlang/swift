// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/pack_expansion_type_other.swift -emit-module -emit-module-path %t/pack_expansion_type_other.swiftmodule -enable-experimental-feature VariadicGenerics

// Experimental features require an asserts compiler
// REQUIRES: asserts

import pack_expansion_type_other

variadicFunction(t: 1, 2, u: "hi", "bye")

VariadicType<Int, String>.variadicMethod(t: 1, 2, u: "hi", "bye")
