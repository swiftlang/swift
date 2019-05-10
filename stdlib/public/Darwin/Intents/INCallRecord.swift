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
@available(iOS 11.0, watchOS 4.0, *)
extension INCallRecord {
  @nonobjc
  public convenience init(
      identifier: String,
      dateCreated: Date? = nil,
      caller: INPerson? = nil,
      callRecordType: INCallRecordType,
      callCapability: INCallCapability,
      callDuration: Double?  = nil,
      unseen: Bool? = nil
  ) {
    self.init(__identifier: identifier,
      dateCreated: dateCreated,
      caller: caller,
      callRecordType: callRecordType,
      callCapability: callCapability,
      callDuration: callDuration.map { NSNumber(value: $0) },
      unseen: unseen.map { NSNumber(value: $0) })
  }

  @nonobjc
  public final var callDuration: Double? {
    return __callDuration?.doubleValue
  }

  @nonobjc
  public final var unseen: Bool? {
    return __unseen?.boolValue
  }
}
#endif
