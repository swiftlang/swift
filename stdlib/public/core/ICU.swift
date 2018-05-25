//===--- ICU.swift --------------------------------------------------------===//
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
import SwiftShims

extension __swift_stdlib_UErrorCode {
  @inlinable // FIXME(sil-serialize-all)
  internal var isFailure: Bool {
    return rawValue > __swift_stdlib_U_ZERO_ERROR.rawValue
  }
  @inlinable // FIXME(sil-serialize-all)
  internal var isWarning: Bool {
    return rawValue < __swift_stdlib_U_ZERO_ERROR.rawValue
  }
  @inlinable // FIXME(sil-serialize-all)
  internal var isSuccess: Bool {
    return rawValue <= __swift_stdlib_U_ZERO_ERROR.rawValue
  }
}
