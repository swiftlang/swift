// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/pack_expansion_type_other.swift -emit-module -emit-module-path %t/pack_expansion_type_other.swiftmodule -target %target-swift-5.9-abi-triple
// RUN: %target-typecheck-verify-swift -I %t -target %target-swift-5.9-abi-triple

import pack_expansion_type_other

_ = variadicFunction(t: 1, 2, u: "hi", "bye")
_ = VariadicType().variadicMethod(t: 1, "hi", u: "bye", 3)
