// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// On Darwin platforms, dyld is expected to manage loaded sections/images for us
// so there's no requirement to test that sections are correctly uniqued.
#if INTERNAL_CHECKS_ENABLED && !canImport(Darwin)
@_silgen_name("swift_getMetadataSection")
internal func _getMetadataSection(_ index: UInt) -> UnsafeRawPointer?

@_silgen_name("swift_getMetadataSectionCount")
internal func _getMetadataSectionCount() -> UInt

@_silgen_name("swift_getMetadataSectionName")
internal func _getMetadataSectionName(
  _ metadata_section: UnsafeRawPointer
) -> UnsafePointer<CChar>

do {
  let sectionCount = _getMetadataSectionCount()
  expectGT(sectionCount, 0)

  var sections = Set<UnsafeRawPointer>()
  var images = Set<UnsafeRawPointer?>()
  for i in 0 ..< sectionCount {
    guard let section = _getMetadataSection(i) else {
      fatalError("Section \(i) failed to resolve.")
    }
    let name = String(cString: _getMetadataSectionName(section))

    expectFalse(sections.contains(section), "Section \(name) was found twice!")
    sections.insert(section)

    expectFalse(images.contains(expected), "Image \(name) was found twice!")
    images.insert(expected)
  }
}
#endif
