//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A C++ type that can be converted to a Boolean value.
///
/// Any C++ type that defines `operator bool()` conforms to this protocol.
public protocol CxxConvertibleToBool {
  /// Do not implement this function manually in Swift.
  func __convertToBool() -> Bool
}

extension Bool {
  @inlinable
  public init<B: CxxConvertibleToBool>(fromCxx convertible: __shared B) {
    self = convertible.__convertToBool()
  }
}
