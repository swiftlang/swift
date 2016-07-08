//===----------------------------------------------------------------------===//
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

@_exported import CallKit
import Foundation

@available(iOS 10.0, *)
extension CXProviderConfiguration {
  @nonobjc
  // FIXME: Due to missing support for SwiftName or SwiftPrivate in apinotes
  // for Swift 2.3, `supportedHandleTypes` cannot be properly hidden, hence the
  // weird looking identifier.
  public final var supportedHandleTypes_: Set<CXHandleType> {
    get {
      return Set(supportedHandleTypes.map {
        CXHandleType(rawValue: $0.integerValue)!
      })
    }
    set {
      supportedHandleTypes = Set(newValue.map { $0.rawValue })
    }
  }
}
