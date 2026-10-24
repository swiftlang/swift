//===--- CustomSections.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if !$Embedded

import SwiftShims

/// The runtime's walk over the custom section index of every registered image
/// (ELF, Wasm, COFF) or over the image headers dyld reported (Mach-O). Absent
/// from Apple OS releases whose runtime predates it, so weak-linked and guarded
/// by the availability check below.
@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_enumerateCustomSections")
internal func _swift_enumerateCustomSections(
  _ sectionName: UnsafePointer<CChar>,
  _ body: @convention(c) (
    _ start: UnsafeRawPointer?,
    _ size: Int,
    _ context: UnsafeMutableRawPointer?
  ) -> Bool,
  _ context: UnsafeMutableRawPointer?
)

/// Returns the contents of the custom section named `name` from every loaded
/// image that the Swift runtime knows about.
///
/// A library that places constant records into a section of its own with the
/// `@section` attribute, for example from a macro expansion, uses this to find
/// every record in the process without a registry and without walking image
/// formats itself.
///
/// - Parameter name: The section name as written in the `@section` attribute
///   for the current object format: `"segment,section"` on Mach-O, a C
///   identifier on ELF and Wasm, and a name with a `$` group suffix on COFF.
/// - Returns: One buffer per image that contains the section, in no particular
///   order, for the images loaded at the time of the call. Each buffer stays
///   valid as long as its image stays loaded; the Swift runtime does not
///   support unloading images that contain Swift code.
///
/// The images searched are those that registered with the Swift runtime when
/// they loaded: every image that contains Swift code, and on Mach-O every
/// image the dynamic loader reports to the runtime, which can include images
/// without Swift code. A section placed by C code in a C-only library is
/// therefore found on Mach-O but not on ELF, Wasm or COFF.
///
/// Records in the buffers have whatever layout the emitting code gave them.
/// Read a record that holds a Swift function value as a struct with the same
/// fields rather than as the tuple type that wrote it, or store such values as
/// `@convention(c)` function pointers. Records are found only if they were
/// kept: a record without `@used` that nothing references can be removed by
/// the optimizer or the linker.
///
/// Section names are shared by every library in an image. Prefix yours with
/// your library's own name; the `swift5_`, `.sw5` and `__swift5_` families
/// belong to the Swift runtime.
@backDeployed(before: SwiftStdlib 6.5)
public func customSectionContents(
  named name: StaticString
) -> [UnsafeRawBufferPointer] {
  var result: [UnsafeRawBufferPointer] = unsafe []
  withUnsafeMutablePointer(to: &result) { resultPointer in
    let context = UnsafeMutableRawPointer(resultPointer)
    unsafe _withCustomSectionName(name) { cName in
      unsafe _enumerateCustomSections(cName, { start, size, context in
        guard let start = unsafe start, let context = unsafe context else {
          return true
        }
        let result = unsafe context.assumingMemoryBound(
          to: [UnsafeRawBufferPointer].self)
        unsafe result.pointee.append(
          unsafe UnsafeRawBufferPointer(start: start, count: size))
        return true
      }, context)
    }
  }
  return unsafe result
}

/// Hands `name` to `body` as a NUL-terminated C string. The pointer form of a
/// static string already is one; a single Unicode scalar is copied.
@_alwaysEmitIntoClient
internal func _withCustomSectionName(
  _ name: StaticString, _ body: (UnsafePointer<CChar>) -> Void
) {
  if name.hasPointerRepresentation {
    unsafe body(
      UnsafeRawPointer(name.utf8Start).assumingMemoryBound(to: CChar.self))
  } else {
    name.withUTF8Buffer { utf8 in
      withUnsafeTemporaryAllocation(
        of: CChar.self, capacity: utf8.count + 1
      ) { cName in
        for unsafe (index, byte) in unsafe utf8.enumerated() {
          unsafe cName[index] = CChar(bitPattern: byte)
        }
        unsafe cName[utf8.count] = 0
        unsafe body(UnsafePointer(cName.baseAddress!))
      }
    }
  }
}

/// Uses the runtime's walk where the runtime has one, and the client-side
/// fallback on Apple OS releases whose runtime predates it.
@_alwaysEmitIntoClient
internal func _enumerateCustomSections(
  _ cName: UnsafePointer<CChar>,
  _ body: @convention(c) (
    UnsafeRawPointer?, Int, UnsafeMutableRawPointer?
  ) -> Bool,
  _ context: UnsafeMutableRawPointer?
) {
  // Literal versions, because this body is emitted into clients, which cannot
  // use the standard library's availability macros. 9999 is the placeholder
  // for a release without an OS version yet; the standard library treats it as
  // always available on a development build.
  if #available(
    macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, visionOS 9999, *
  ) {
    unsafe _swift_enumerateCustomSections(cName, body, context)
  } else {
    unsafe _enumerateCustomSectionsWithoutRuntime(cName, body, context)
  }
}

/// The back-deployment fallback. Mach-O keeps section bounds in every image
/// header, so this walks dyld's image list at query time and looks the name up
/// in each header, as the runtime does, without keeping a registry. Other
/// platforms ship the runtime with the toolchain and never reach this.
@_alwaysEmitIntoClient
internal func _enumerateCustomSectionsWithoutRuntime(
  _ cName: UnsafePointer<CChar>,
  _ body: @convention(c) (
    UnsafeRawPointer?, Int, UnsafeMutableRawPointer?
  ) -> Bool,
  _ context: UnsafeMutableRawPointer?
) {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
  // Two NUL-terminated names of at most 16 bytes each, as a Mach-O header
  // stores them.
  withUnsafeTemporaryAllocation(of: CChar.self, capacity: 34) { names in
    guard unsafe _splitMachOSectionName(cName, into: names) else { return }
    let segment = unsafe UnsafePointer(names.baseAddress!)
    let section = unsafe segment + 17
    let imageCount = _swift_stdlib_customSections_imageCount()
    for index in 0..<imageCount {
      guard let header = unsafe _swift_stdlib_customSections_imageHeader(index)
      else { continue }
      var size = 0
      guard let start = unsafe _swift_stdlib_customSections_sectionData(
        header, segment, section, &size
      ), size > 0 else { continue }
      if unsafe !body(start, size, context) { return }
    }
  }
#endif
}

/// Splits `"segment,section"`, tolerating spaces and trailing attributes as the
/// runtime does, into two NUL-terminated names at `names[0]` and `names[17]`.
/// Returns false when either part is empty or longer than 16 bytes.
@_alwaysEmitIntoClient
internal func _splitMachOSectionName(
  _ name: UnsafePointer<CChar>, into names: UnsafeMutableBufferPointer<CChar>
) -> Bool {
  let comma: CChar = 0x2C
  let space: CChar = 0x20
  var commaIndex = 0
  while unsafe name[commaIndex] != 0 && name[commaIndex] != comma {
    commaIndex += 1
  }
  guard unsafe name[commaIndex] == comma else { return false }

  var segmentStart = 0
  while unsafe name[segmentStart] == space { segmentStart += 1 }
  var segmentEnd = commaIndex
  while unsafe segmentEnd > segmentStart && name[segmentEnd - 1] == space {
    segmentEnd -= 1
  }
  var sectionStart = commaIndex + 1
  while unsafe name[sectionStart] == space { sectionStart += 1 }
  var sectionEnd = sectionStart
  while unsafe name[sectionEnd] != 0 && name[sectionEnd] != comma
    && name[sectionEnd] != space {
    sectionEnd += 1
  }

  let segmentLength = segmentEnd - segmentStart
  let sectionLength = sectionEnd - sectionStart
  guard segmentLength > 0, segmentLength <= 16,
        sectionLength > 0, sectionLength <= 16 else { return false }
  for offset in 0..<segmentLength {
    unsafe names[offset] = unsafe name[segmentStart + offset]
  }
  unsafe names[segmentLength] = 0
  for offset in 0..<sectionLength {
    unsafe names[17 + offset] = unsafe name[sectionStart + offset]
  }
  unsafe names[17 + sectionLength] = 0
  return true
}

#endif // !$Embedded
