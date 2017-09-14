//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file contains compiler intrinsics for optimized string switch
// implementations. All functions and types declared in this file are not
// intended to be used by switch source directly.

/// The compiler intrinsic which is called to lookup a string in a table
/// of static string case values.
@_semantics("stdlib_binary_only")
@_semantics("findStringSwitchCase")
public // COMPILER_INTRINSIC
func _findStringSwitchCase(
  cases: [StaticString],
  string: String) -> Int {

  for (idx, s) in cases.enumerated() {
    if String(_builtinStringLiteral: s.utf8Start._rawValue,
              utf8CodeUnitCount: s._utf8CodeUnitCount,
              isASCII: s.isASCII._value) == string {
      return idx
    }
  }
  return -1
}

