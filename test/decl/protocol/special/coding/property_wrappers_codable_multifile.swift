// RUN: %target-swift-frontend -c -primary-file %s %S/Inputs/property_wrappers_codable_multifile_other.swift

func test(_ value: Foo = Foo()) {
  let _: Codable = value
}
