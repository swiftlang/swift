// We need Objective-C support for this test
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)

import Foundation
import RoundTrip

@objc protocol OurObjCProtocol {}
class OurObjCClass: NSObject, OurObjCProtocol {}

public func test() {
  roundTripType(NSSet.self)
  roundTripType(NSFastEnumeration.self)
  roundTripType(OurObjCProtocol.self)
  roundTripType(NSCache<NSNumber, NSString>.self)
  roundTripType(PropertyListSerialization.WriteOptions.self)

  roundTripType(NSSet.Type.self)
  roundTripType(NSFastEnumeration.Type.self)
  roundTripType(OurObjCProtocol.Type.self)
  roundTripType(NSCache<NSNumber, NSString>.Type.self)
  roundTripType(PropertyListSerialization.WriteOptions.Type.self)

  roundTripType(NSFastEnumeration.Protocol.self)
  roundTripType(OurObjCProtocol.Protocol.self)
}

#else
public func test() {
  print("No Objective-C support, so skipping this test")
}
#endif
