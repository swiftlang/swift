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

import SwiftPrivate
import SwiftPrivateDarwinExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

#if _runtime(_ObjC)
import Foundation
#endif

//
// These APIs don't really belong in a unit testing library, but they are
// useful in tests, and stdlib does not have such facilities yet.
//

func findSubstring(string: String, _ substring: String) -> String.Index? {
  if substring.isEmpty {
    return string.startIndex
  }
#if _runtime(_ObjC)
  return string.rangeOfString(substring)?.startIndex
#else
  // FIXME(performance): This is a very non-optimal algorithm, with a worst
  // case of O((n-m)*m). When non-objc String has a match function that's better,
  // this should be removed in favour of using that.

  // Operate on unicode scalars rather than codeunits.
  let haystack = string.unicodeScalars
  let needle = substring.unicodeScalars

  for matchStartIndex in haystack.indices {
    var matchIndex = matchStartIndex
    var needleIndex = needle.startIndex
    while true {
      if needleIndex == needle.endIndex {
        // if we hit the end of the search string, we found the needle
        return matchStartIndex.samePositionIn(string)
      }
      if matchIndex == haystack.endIndex {
        // if we hit the end of the string before finding the end of the needle,
        // we aren't going to find the needle after that.
        return nil
      }
      if needle[needleIndex] == haystack[matchIndex] {
        // keep advancing through both the string and search string on match
        ++matchIndex
        ++needleIndex
      } else {
        // no match, go back to finding a starting match in the string.
        break
      }
    }
  }
  return nil
#endif
}

public func createTemporaryFile(
  fileNamePrefix: String, _ fileNameSuffix: String, _ contents: String
) -> String {
#if _runtime(_ObjC)
  var fileName = NSTemporaryDirectory().stringByAppendingPathComponent(
    fileNamePrefix + "XXXXXX" + fileNameSuffix)
#else
  var fileName = fileNamePrefix + "XXXXXX" + fileNameSuffix
#endif
  let fd = _stdlib_mkstemps(
    &fileName, CInt(fileNameSuffix.utf8.count))
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

public final class Box<T> {
  public init(_ value: T) { self.value = value }
  public var value: T
}

infix operator <=> {}

public func <=> <T: Comparable>(lhs: T, rhs: T) -> ExpectedComparisonResult {
  return lhs < rhs
    ? .LT
    : lhs > rhs ? .GT : .EQ
}

public struct TypeIdentifier : Hashable, Comparable {
  public init(_ value: Any.Type) {
    self.value = value
  }

  public var hashValue: Int { return objectID.hashValue }
  public var value: Any.Type
  
  internal var objectID : ObjectIdentifier { return ObjectIdentifier(value) }
}

public func < (lhs: TypeIdentifier, rhs: TypeIdentifier) -> Bool {
  return lhs.objectID < rhs.objectID
}

public func == (lhs: TypeIdentifier, rhs: TypeIdentifier) -> Bool {
  return lhs.objectID == rhs.objectID
}

extension TypeIdentifier
  : CustomStringConvertible, CustomDebugStringConvertible {
  public var description: String {
    return String(value)
  }
  public var debugDescription: String {
    return "TypeIdentifier(\(description))"
  }
}
