//===--- StdlibCoreExtras.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftUnstable
import Darwin
import SwiftUnstableDarwinExtras
import Foundation

//
// These APIs don't really belong in a unit testing library, but they are
// useful in tests, and stdlib does not have such facilities yet.
//

func findSubstring(string: String, substring: String) -> String.Index? {
  if substring.isEmpty {
    return string.startIndex
  }
  return string.rangeOfString(substring)?.startIndex
}

public func createTemporaryFile(
  fileNamePrefix: String, fileNameSuffix: String, contents: String
) -> String {
  var fileName = NSTemporaryDirectory().stringByAppendingPathComponent(
    fileNamePrefix + "XXXXXX" + fileNameSuffix)
  let fd = _stdlib_mkstemps(
    &fileName, CInt(count(fileNameSuffix.utf8)))
  if fd < 0 {
    fatalError("mkstemps() returned an error")
  }
  var stream = _FDOutputStream(fd: fd)
  stream.write(contents)
  if close(fd) != 0 {
    fatalError("close() return an error")
  }
  return fileName
}

