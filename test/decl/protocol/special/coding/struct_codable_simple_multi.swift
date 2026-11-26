// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown %S/Inputs/struct_codable_simple_multi1.swift %S/Inputs/struct_codable_simple_multi2.swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown %S/Inputs/struct_codable_simple_multi2.swift %S/Inputs/struct_codable_simple_multi1.swift
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -verify-ignore-unknown %S/Inputs/struct_codable_simple_multi1.swift %S/Inputs/struct_codable_simple_multi2.swift -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros
