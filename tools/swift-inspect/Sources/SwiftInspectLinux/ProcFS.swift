//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

// utility for reading files under /proc
public enum ProcFS {
  public static func loadFile(for pid: pid_t, _ fileName: String) -> Data? {
    let filePath = "/proc/\(pid)/\(fileName)"
    // Loading contents of files under /proc may not work correctly using
    // Data(contentsOf:) or String(contentsOfFile:) because the files may
    // appear empty from stat(2) and may not be seekable. FileHandle.readToEnd
    // handles these cases.
    guard let fileHandle = FileHandle(forReadingAtPath: filePath) else { return nil }
    defer { fileHandle.closeFile() }
    guard let data = try? fileHandle.readToEnd() else { return nil }
    return data
  }

  public static func loadFileAsString(
    for pid: pid_t, _ fileName: String, encoding: String.Encoding = .utf8
  ) -> String? {
    guard let data = Self.loadFile(for: pid, fileName) else { return nil }
    return String(data: data, encoding: encoding)
  }
}
