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

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INSetProfileInCarIntent {
  @nonobjc
  public convenience init(
    profileNumber: Int? = nil,
    profileLabel: String? = nil,
    defaultProfile: Int? = nil
  ) {
    self.init(
      __profileNumber: profileNumber.map { NSNumber(value: $0) },
      profileLabel: profileLabel,
      defaultProfile: defaultProfile.map { NSNumber(value: $0) })
  }

  @nonobjc  
  public final var profileNumber: Int? {
    return __profileNumber?.intValue
  }

  @nonobjc  
  public final var defaultProfile: Int? {
    return __defaultProfile?.intValue
  }
}
#endif
