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

#if os(iOS) || os(watchOS)
@available(iOS 10.0, watchOS 3.2, *)
extension INSearchCallHistoryIntent {
  @available(iOS 11.0, watchOS 4.0, *)
  @nonobjc
  public convenience init(
    dateCreated: INDateComponentsRange? = nil,
    recipient: INPerson? = nil,
    callCapabilities: INCallCapabilityOptions,
    callTypes: INCallRecordTypeOptions,
    unseen: Bool? = nil
  ) {
    self.init(__dateCreated: dateCreated,
      recipient: recipient,
      callCapabilities: callCapabilities,
      callTypes: callTypes,
      unseen: unseen.map { NSNumber(value: $0) })
  }

  @available(iOS 11.0, watchOS 4.0, *)
  @nonobjc
  public final var unseen: Bool? {
    return __unseen?.boolValue
  }
}
#endif
