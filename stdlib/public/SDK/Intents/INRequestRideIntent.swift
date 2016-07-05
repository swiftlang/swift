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

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INRequestRideIntent {
  @nonobjc
  public convenience init(
    pickupLocation: CLPlacemark? = nil,
    dropOffLocation: CLPlacemark? = nil,
    rideOptionName: INSpeakableString? = nil,
    partySize: Int? = nil,
    paymentMethodName: INSpeakableString? = nil,
    usesApplePayForPayment: Bool? = nil
  ) {
    self.init(__pickupLocation: pickupLocation,
      dropOffLocation: dropOffLocation,
      rideOptionName: rideOptionName,
      partySize: partySize.map { NSNumber(long: $0) },
      paymentMethodName: paymentMethodName,
      usesApplePayForPayment:
        usesApplePayForPayment.map { NSNumber(bool: $0) })
  }

  @nonobjc
  public final var partySize: Int? {
    return __partySize?.longValue
  }

  @nonobjc
  public final var usesApplePayForPayment: Bool? {
    return __usesApplePayForPayment?.boolValue
  }
}
#endif
