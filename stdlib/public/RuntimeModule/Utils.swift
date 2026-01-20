//===--- Utils.swift - Utility functions ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utility functions that are used in the backtracing library.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import WinSDK
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

@_spi(Utils)
public func hex<T: FixedWidthInteger>(_ value: T,
                                        prefix shouldPrefix: Bool = true,
                                        width: Int = MemoryLayout<T>.size * 2)
  -> String {
  let digits = String(value, radix: 16)
  let padding = digits.count >= width ? "" : String(repeating: "0",
                                                    count: width - digits.count)
  let prefix = shouldPrefix ? "0x" : ""

  return "\(prefix)\(padding)\(digits)"
}

@_spi(Utils)
public func hex(_ bytes: some Sequence<UInt8>) -> String {
  return bytes.map{ hex($0, prefix: false) }.joined(separator: "")
}

enum PadAlignment {
  case left
  case right
}

func pad<T>(_ value: T, _ width: Int, align: PadAlignment = .left) -> String {
  let string = String(describing: value)
  let padding = string.count >= width ? "" : String(repeating: " ",
                                                    count: width - string.count)
  switch align {
    case .left:
      return string + padding
    case .right:
      return padding + string
  }
}

func dirname(_ path: String) -> Substring {
  #if os(Windows)
  var lastSep: String.Index? = nil

  if let lastBackslash = path.lastIndex(of: "\\") {
    lastSep = lastBackslash
  }
  if let lastSlash = path.lastIndex(of: "/") {
    if lastSep == nil || lastSep! < lastSlash {
      lastSep = lastSlash
    }
  }
  #else
  let lastSep = path.lastIndex(of: "/")
  #endif

  guard let lastSep else {
    return ""
  }

  return path.prefix(upTo: lastSep)
}

func splitpath(_ path: String) -> (Substring, Substring) {
  #if os(Windows)
  var lastSep: String.Index? = nil

  if let lastBackslash = path.lastIndex(of: "\\") {
    lastSep = lastBackslash
  }
  if let lastSlash = path.lastIndex(of: "/") {
    if lastSep == nil || lastSep! < lastSlash {
      lastSep = lastSlash
    }
  }
  #else
  let lastSep = path.lastIndex(of: "/")
  #endif

  guard let lastSep else {
    return ("", path[...])
  }
  let afterSep = path.index(after: lastSep)

  return (path.prefix(upTo: lastSep), path.suffix(from: afterSep))
}

func realPath(_ path: String) -> String? {
  #if os(Windows)
  let hFile: HANDLE = path.withCString(encodedAs: UTF16.self) {
    return CreateFileW($0,
                       GENERIC_READ,
                       DWORD(FILE_SHARE_READ),
                       nil,
                       DWORD(OPEN_EXISTING),
                       DWORD(FILE_ATTRIBUTE_NORMAL),
                       nil)
  }

  if hFile == INVALID_HANDLE_VALUE {
    return nil
  }
  defer {
    CloseHandle(hFile)
  }

  var bufferSize = 1024
  var result: String? = nil
  while result == nil {
    result = withUnsafeTemporaryAllocation(of: WCHAR.self,
                                           capacity: 1024) { buffer in
      let dwRet = GetFinalPathNameByHandleW(hFile,
                                            buffer.baseAddress,
                                            DWORD(buffer.count),
                                            DWORD(VOLUME_NAME_DOS))
      if dwRet >= bufferSize {
        bufferSize = Int(dwRet + 1)
        return nil
      } else {
        return String(decoding: buffer[0..<Int(dwRet)], as: UTF16.self)
      }
    }
  }

  return result
  #else
  guard let result = realpath(path, nil) else {
    return nil
  }

  let s = String(cString: result)

  free(result)

  return s
  #endif
}

#if os(Linux)
@_spi(Utils)
@available(Backtracing 6.2, *)
public func readString(from file: String) -> String? {
  let fd = open(file, O_RDONLY, 0)
  if fd < 0 {
    return nil
  }
  defer { close(fd) }

  // Files in /proc are awkward; you can't get their length and then
  // read the data in one chunk, because they have zero length and don't
  // support seeking.
  var bytes: [UInt8] = []
  withUnsafeTemporaryAllocation(of: UInt8.self, capacity: 4096) { buffer in
    while true {
      let bytesRead = read(fd, buffer.baseAddress, buffer.count)
      if bytesRead <= 0 {
        break
      }

      bytes.append(contentsOf: buffer[0..<bytesRead])
    }
  }

  return String(decoding: bytes, as: UTF8.self)
}
#endif

@_spi(Utils)
@available(Backtracing 6.2, *)
public func stripWhitespace<S: StringProtocol>(_ s: S)
    -> S.SubSequence {
  guard let firstNonWhitespace = s.firstIndex(where: { !$0.isWhitespace })
  else {
    return ""
  }
  let lastNonWhitespace = s.lastIndex(where: { !$0.isWhitespace })!
  return s[firstNonWhitespace...lastNonWhitespace]
}

/// Escape a JSON string
@_spi(Utils)
public func escapeJSON(_ s: String) -> String {
  var result = ""
  let utf8View = s.utf8
  var chunk = utf8View.startIndex
  var pos = chunk
  let end = utf8View.endIndex

  result.reserveCapacity(utf8View.count)

  while pos != end {
    let scalar = utf8View[pos]
    switch scalar {
      case 0x22, 0x5c, 0x00...0x1f:
        result += s[chunk..<pos]
        result += "\\"
        result += String(Unicode.Scalar(scalar))
        pos = utf8View.index(after: pos)
        chunk = pos
      default:
        pos = utf8View.index(after: pos)
    }
  }

  if chunk != end {
    result += s[chunk..<pos]
  }

  return result
}

/// Strip any Optional from a value.
///
/// This is useful when interfacing with the system C library, because some
/// C libraries have nullability annotations while others do not.
func notOptional<T>(_ optional: T?) -> T {
  return optional!
}

func notOptional<T>(_ value: T) -> T {
  return value
}

/// Convert mutable pointers to non-mutable
///
/// This is useful when interfacing with the system C library, because some
/// C libraries have const annotations in places others do not.
func notMutable<T>(_ mutable: UnsafeMutablePointer<T>) -> UnsafePointer<T> {
  return UnsafePointer<T>(mutable)
}
func notMutable<T>(_ immutable: UnsafePointer<T>) -> UnsafePointer<T> {
  return immutable
}
func notMutable(_ mutable: UnsafeMutableRawPointer) -> UnsafeRawPointer {
  return UnsafeRawPointer(mutable)
}
func notMutable(_ immutable: UnsafeRawPointer) -> UnsafeRawPointer {
  return immutable
}

/// Get the (string) value of an environment variable
func getEnv(_ variableName: String) -> String? {
  #if os(Windows)
  return withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: 4096) { buf in
    variableName.withCString(encodedAs: UTF16.self) { lpName in
      let len = GetEnvironmentVariableW(lpName,
                                        buf.baseAddress,
                                        DWORD(buf.count))
      if len == 0 || Int(len) > buf.count {
        return nil
      }
      return String(decoding: buf[..<Int(len)], as: UTF16.self)
    }
  }
  #else
  return String(cString: getenv(variableName))
  #endif
}
