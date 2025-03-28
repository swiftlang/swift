//===--- Utils.swift - Utility functions ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Utility functions that are used in the swift-backtrace program.
//
//===----------------------------------------------------------------------===//

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(CRT)
import CRT
#endif

import Swift

internal import BacktracingImpl.Runtime

typealias CrashInfo = swift.runtime.backtrace.CrashInfo

#if os(Linux)
typealias thread = swift.runtime.backtrace.thread
#endif

extension String {
  init<T: BinaryInteger>(_ value: T,
                         width: Int,
                         radix: Int = 10,
                         uppercase: Bool = false) {
    let digits = String(value, radix: radix, uppercase: uppercase)
    if digits.count >= width {
      self = digits
      return
    }

    let padding = String(repeating: "0",
                         count: width - digits.count)
    self = padding + digits
  }
}

internal func hex<T: FixedWidthInteger>(_ value: T,
                                        withPrefix: Bool = true) -> String {
  let formatted = String(value, width: value.bitWidth / 4, radix: 16)

  if withPrefix {
    return "0x" + formatted
  } else {
    return formatted
  }
}

internal func hex(_ bytes: [UInt8]) -> String {
  return bytes.map{ hex($0, withPrefix: false) }.joined(separator: "")
}

internal func parseUInt64<S: StringProtocol>(_ s: S) -> UInt64? {
  if s.hasPrefix("0x") {
    return UInt64(s.dropFirst(2), radix: 16)
  } else if s.hasPrefix("0b") {
    return UInt64(s.dropFirst(2), radix: 2)
  } else if s.hasPrefix("0o") {
    return UInt64(s.dropFirst(2), radix: 8)
  } else {
    return UInt64(s, radix: 10)
  }
}

#if os(macOS) || os(Linux)

struct PosixError: Error {
  var errno: Int32

  var description: String {
    return String(cString: strerror(self.errno))
  }
}

internal func recursiveRemoveContents(_ dir: String) throws {
  guard let dirp = opendir(dir) else {
    throw PosixError(errno: errno)
  }
  defer {
    closedir(dirp)
  }
  while let dp = readdir(dirp) {
    let name: String =
      withUnsafePointer(to: &dp.pointee.d_name) {
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
        let len = Int(dp.pointee.d_namlen)
#else
        let len = Int(strlen($0))
#endif
        return String(decoding: UnsafeRawBufferPointer(start: $0,
                                                       count: len),
                      as: UTF8.self)
      }
    if name == "." || name == ".." {
      continue
    }
    let fullPath = "\(dir)/\(name)"
    if dp.pointee.d_type == DT_DIR {
      try recursiveRemove(fullPath)
    } else {
      if unlink(fullPath) != 0 {
        throw PosixError(errno: errno)
      }
    }
  }
}

/// Remove a directory and its contents.
internal func recursiveRemove(_ dir: String) throws {
  try recursiveRemoveContents(dir)

  if rmdir(dir) != 0 {
    throw PosixError(errno: errno)
  }
}

/// Run a closure, passing in the name of a temporary directory that will
/// (optionally) be deleted automatically when the closure returns.
///
/// Parameters:
///
/// - pattern:       A string with some number of trailing 'X's giving the name
///                  to use for the directory; the 'X's will be replaced with a
///                  unique combination of alphanumeric characters.
/// - shouldDelete:  If `true` (the default), the directory and its contents
///                  will be removed automatically when the closure returns.
/// - body:          The closure to execute.
///
/// Returns:
///
/// This function returns whatever the closure returns.
internal func withTemporaryDirectory<R>(
  pattern: String, shouldDelete: Bool = true,
  body: (String) throws -> R
) throws -> R {
  var buf = Array<UInt8>(pattern.utf8)
  buf.append(0)

  guard let dir = buf.withUnsafeMutableBufferPointer({
    if let ptr = mkdtemp($0.baseAddress!) {
      return String(cString: ptr)
    }
    return nil
  }) else {
    throw PosixError(errno: errno)
  }

  defer {
    if shouldDelete {
      try? recursiveRemove(dir)
    }
  }

  return try body(dir)
}

/// Start a program with the specified arguments
internal func spawn(_ path: String, args: [String]) throws {
  var cargs = args.map{ strdup($0) }
  cargs.append(nil)
  let result = cargs.withUnsafeBufferPointer{
    posix_spawn(nil, path, nil, nil, $0.baseAddress!, nil)
  }
  for arg in cargs {
    free(arg)
  }
  if result != 0 {
    throw PosixError(errno: errno)
  }
}

#endif // os(macOS)

/// Test if the specified path is a directory
internal func isDir(_ path: String) -> Bool {
  var st = stat()
  guard stat(path, &st) == 0 else {
    return false
  }
  return (st.st_mode & S_IFMT) == S_IFDIR
}

/// Test if the specified path exists
internal func exists(_ path: String) -> Bool {
  var st = stat()
  guard stat(path, &st) == 0 else {
    return false
  }
  return true
}

extension Sequence {
  /// Return the first element in a Sequence.
  ///
  /// This is not, in general, a safe thing to do, because the sequence might
  /// not be restartable.  For the cases where we're using it here, it's OK
  /// though.
  var consumingFirst: Element? {
    var iterator = makeIterator()
    return iterator.next()
  }
}

struct CFileStream: TextOutputStream {
  var fp: UnsafeMutablePointer<FILE>

  public func write(_ string: String) {
    fputs(string, fp)
  }

  public func flush() {
    fflush(fp)
  }

  public func close() {
    fclose(fp)
  }
}

var standardOutput = CFileStream(fp: stdout)
var standardError = CFileStream(fp: stderr)

/// Format a timespec as an ISO8601 date/time
func formatISO8601(_ time: timespec) -> String {
  var exploded = tm()
  var secs = time.tv_sec

  gmtime_r(&secs, &exploded)

  let isoTime = """
\(String(exploded.tm_year + 1900, width: 4))-\
\(String(exploded.tm_mon + 1, width: 2))-\
\(String(exploded.tm_mday, width: 2))T\
\(String(exploded.tm_hour, width: 2)):\
\(String(exploded.tm_min, width: 2)):\
\(String(exploded.tm_sec, width: 2)).\
\(String(time.tv_nsec / 1000, width: 6))Z
"""

  return isoTime
}

/// Escape a JSON string
func escapeJSON(_ s: String) -> String {
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
