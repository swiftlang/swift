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
extension INRequestRideIntent {
  @nonobjc
  public convenience init(
    pickupLocation: CLPlacemark? = nil,
    dropOffLocation: CLPlacemark? = nil,
    rideOptionName: INSpeakableString? = nil,
    partySize: Int? = nil,
    paymentMethod: INPaymentMethod? = nil,
    scheduledPickupTime: INDateComponentsRange? = nil
  ) {
    if #available(iOS 10.3, watchOS 3.2, *) {
      self.init(__pickupLocation: pickupLocation,
        dropOffLocation: dropOffLocation,
        rideOptionName: rideOptionName,
        partySize: partySize.map { NSNumber(value: $0) },
        paymentMethod: paymentMethod,
        scheduledPickupTime: scheduledPickupTime)
    } else {
#if os(iOS)
      self.init(__pickupLocation: pickupLocation,
        dropOffLocation: dropOffLocation,
        rideOptionName: rideOptionName,
        partySize: partySize.map { NSNumber(value: $0) },
        paymentMethod: paymentMethod)
#else
      fatalError("The initializer is not available")
#endif
    }
  }

  @nonobjc
  public final var partySize: Int? {
    return __partySize?.intValue
  }
}
#endif
