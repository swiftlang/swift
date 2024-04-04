// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/protocol_with_self_conforming_existential_other.swift -emit-module-path %t/protocol_with_self_conforming_existential_other.swiftmodule
// RUN: %target-swift-frontend -emit-silgen %s -I %t

import protocol_with_self_conforming_existential_other

// Declare a type conforming to P2, to force deserializing its
// requirement signature.

struct MyP2: P2 {}

