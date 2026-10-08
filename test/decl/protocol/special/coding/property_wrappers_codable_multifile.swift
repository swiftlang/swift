// RUN: %target-swift-frontend -c -primary-file %s %S/Inputs/property_wrappers_codable_multifile_other.swift
// RUN: %target-swift-frontend -c -primary-file %s %S/Inputs/property_wrappers_codable_multifile_other.swift -enable-experimental-feature DeriveConformancesViaMacros -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// REQUIRES: swift_feature_DeriveConformancesViaMacros

func test(_ value: Foo = Foo()) {
  let _: Codable = value
}
