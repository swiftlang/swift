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
extension INSaveProfileInCarIntent {
  @nonobjc
  public convenience init(
    profileNumber: Int? = nil, profileLabel: String? = nil
  ) {
    self.init(__profileNumber: profileNumber.map { NSNumber(value: $0) },
      profileLabel: profileLabel)
  }

  @nonobjc  
  public final var profileNumber: Int? {
    return __profileNumber?.intValue
  }
}
#endif
