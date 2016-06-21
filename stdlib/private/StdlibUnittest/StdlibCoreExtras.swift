//===--- StdlibCoreExtras.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftPrivate
import SwiftPrivateLibcExtras
#if os(OSX) || os(iOS)
import Darwin
#elseif os(Linux) || os(FreeBSD) || os(Android)
import Glibc
#endif

#if _runtime(_ObjC)
import Foundation
#endif

//
// These APIs don't really belong in a unit testing library, but they are
// useful in tests, and stdlib does not have such facilities yet.
//

func findSubstring(_ string: String, _ substring: String) -> String.Index? {
  if substring.isEmpty {
    return string.startIndex
  }
#if _runtime(_ObjC)
  return string.range(of: substring)?.lowerBound
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
        return matchStartIndex.samePosition(in: string)
      }
      if matchIndex == haystack.endIndex {
        // if we hit the end of the string before finding the end of the needle,
        // we aren't going to find the needle after that.
        return nil
      }
      if needle[needleIndex] == haystack[matchIndex] {
        // keep advancing through both the string and search string on match
        matchIndex = haystack.index(after: matchIndex)
        needleIndex = haystack.index(after: needleIndex)
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
  _ fileNamePrefix: String, _ fileNameSuffix: String, _ contents: String
) -> String {
#if _runtime(_ObjC)
  let tempDir: NSString = NSTemporaryDirectory() as NSString
  var fileName = tempDir.appendingPathComponent(
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
    ? .lt
    : lhs > rhs ? .gt : .eq
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

func _forAllPermutationsImpl(
  _ index: Int, _ size: Int,
  _ perm: inout [Int], _ visited: inout [Bool],
  _ body: ([Int]) -> Void
) {
  if index == size {
    body(perm)
    return
  }

  for i in 0..<size {
    if visited[i] {
      continue
    }
    visited[i] = true
    perm[index] = i
    _forAllPermutationsImpl(index + 1, size, &perm, &visited, body)
    visited[i] = false
  }
}

/// Generate all permutations.
public func forAllPermutations(_ size: Int, body: ([Int]) -> Void) {
  if size == 0 {
    return
  }

  var permutation = [Int](repeating: 0, count: size)
  var visited = [Bool](repeating: false, count: size)
  _forAllPermutationsImpl(0, size, &permutation, &visited, body)
}

/// Generate all permutations.
public func forAllPermutations<S : Sequence>(
  _ sequence: S, body: ([S.Iterator.Element]) -> Void
) {
  let data = Array(sequence)
  forAllPermutations(data.count) {
    (indices: [Int]) in
    body(indices.map { data[$0] })
    return ()
  }
}
