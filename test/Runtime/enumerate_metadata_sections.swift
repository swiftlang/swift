// RUN: %target-run-simple-swift
// REQUIRES: executable_test

#if os(Linux) || os(Windows)
import Swift
import SwiftShims
import StdlibUnittest

let EnumerateMetadataSections = TestSuite("EnumerateMetadataSections")

@_silgen_name("swift_enumerateAllMetadataSections")
func swift_enumerateAllMetadataSections(
  _ body: @convention(c) (
    _ sections: UnsafePointer<MetadataSections>,
    _ context: UnsafeMutableRawPointer
  ) -> Bool,
  _ context: UnsafeMutableRawPointer
)

public protocol P { }
public struct S: P { }

EnumerateMetadataSections.test("swift_enumerateAllMetadataSections works") {
  var sectionsEnumerated = 0
  swift_enumerateAllMetadataSections({ sections, context in
    let sectionsEnumerated = context.bindMemory(to: Int.self, capacity: 1)

    // Confirm that the base address of the metadata sections was loaded.
    let baseAddress = sections.pointee.baseAddress
    expectNotNil(baseAddress)

    // Confirm that P and S above have been emitted.
    if baseAddress == #dsohandle {
      expectNotNil(sections.pointee.swift5_protocols)
      expectNotNil(sections.pointee.swift5_protocol_conformances)
      expectNotNil(sections.pointee.swift5_type_metadata)
    }

    sectionsEnumerated.pointee += 1
    return true
  }, &sectionsEnumerated)

  // Confirm that at least one section was enumerated.
  expectGT(sectionsEnumerated, 0)
}

runAllTests()

#endif
