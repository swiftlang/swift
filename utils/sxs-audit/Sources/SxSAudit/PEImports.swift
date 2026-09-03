// Copyright © 2026 Saleem Abdulrasool <compnerd@compnerd.org>. All rights reserved.
// SPDX-License-Identifier: BSD-3-Clause

internal import struct FoundationEssentials.URL
private import WinSDK

private func offset(of pointer: UnsafeRawPointer,
                    relativeTo base: UnsafeRawPointer,
                    in image: borrowing RawSpan,
                    count: Int) -> Int? {
  let start = UInt(bitPattern: base)
  let address = UInt(bitPattern: pointer)
  guard address >= start, count >= 0 else { return nil }
  let distance = address - start
  guard distance <= UInt(image.byteCount) else { return nil }
  let offset = Int(distance)
  guard count <= image.byteCount - offset else {
    return nil
  }
  return offset
}

private func load<T>(_ type: T.Type, from bytes: borrowing RawSpan,
                     at offset: Int) -> T {
  bytes.withUnsafeBytes {
    $0.loadUnaligned(fromByteOffset: offset, as: type)
  }
}

internal struct ImportReader {
  private var cache = Dictionary<URL, Set<String>>()

  internal mutating func imports(of path: URL) throws -> Set<String> {
    if let cached = cache[path] { return cached }

    let data = try contents(path)
    let bytes = data.span
    let image = bytes.bytes
    let names = try image.withUnsafeBytes { buffer in
      guard let address = buffer.baseAddress else {
        throw AnalyzerError("empty PE image: \(path.path)")
      }
      let base = UnsafeMutableRawPointer(mutating: address)
      guard let headers = ImageNtHeader(base) else {
        throw AnalyzerError("invalid PE image: \(path.path)")
      }

      var names = Set<String>()
      try read(in: image, base: base, headers: headers,
               directory: IMAGE_DIRECTORY_ENTRY_IMPORT,
               name: \IMAGE_IMPORT_DESCRIPTOR.Name, into: &names)
      try read(in: image, base: base, headers: headers,
               directory: IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT,
               name: \IMAGE_DELAYLOAD_DESCRIPTOR.DllNameRVA, into: &names)
      return names
    }
    cache[path] = names
    return names
  }

  private func read<Descriptor>(in image: borrowing RawSpan,
                                base: UnsafeMutableRawPointer,
                                headers: PIMAGE_NT_HEADERS64,
                                directory: USHORT,
                                name: KeyPath<Descriptor, UInt32>,
                                into names: inout Set<String>)
      throws(AnalyzerError) {
    var length = ULONG(0)
    var found: PIMAGE_SECTION_HEADER?
    guard let data = ImageDirectoryEntryToDataEx(base, BOOLEAN(0), directory,
                                                 &length, &found) else {
      return
    }

    let stride = MemoryLayout<Descriptor>.stride
    guard stride != 0,
        let directoryOffset = offset(of: data, relativeTo: base, in: image,
                                     count: Int(length)) else {
      throw AnalyzerError("invalid PE import directory")
    }

    let count = Int(length) / stride
    let descriptors = image.extracting(directoryOffset ..<
                                       directoryOffset + Int(length))
    var section: PIMAGE_SECTION_HEADER?
    for index in 0 ..< count {
      let entry = load(Descriptor.self, from: descriptors, at: index * stride)
      let rva = entry[keyPath: name]
      if rva == 0 { break }
      guard let address = ImageRvaToVa(headers, base, rva, &section),
          let nameOffset = offset(of: address, relativeTo: base, in: image,
                                  count: 1) else {
        throw AnalyzerError("invalid PE import name RVA")
      }

      let bytes = image.extracting(nameOffset ..< image.byteCount)
      var length = 0
      for byte in bytes {
        if byte == 0 { break }
        length += 1
      }
      guard length < bytes.byteCount else {
        throw AnalyzerError("unterminated PE import name")
      }
      let name = bytes.extracting(0 ..< length).withUnsafeBytes {
        String(decoding: $0, as: UTF8.self)
      }
      names.insert(name)
    }
  }
}
