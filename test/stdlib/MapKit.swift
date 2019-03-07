// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import CoreLocation
import MapKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

var mapKit = TestSuite("MapKit")

func coordinatesEqual(_ x: CLLocationCoordinate2D, _ y: CLLocationCoordinate2D)
    -> Bool {
  return x.latitude == y.latitude && x.longitude == y.longitude
}
func spansEqual(_ x: MKCoordinateSpan, _ y: MKCoordinateSpan)
    -> Bool {
  return x.latitudeDelta == y.latitudeDelta
      && x.longitudeDelta == y.longitudeDelta
}

if #available(tvOS 9.2, *) {
  mapKit.test("CLLocationCoordinate2D bridging") {
    expectBridgeToNSValue(CLLocationCoordinate2D(latitude: 17, longitude: 38),
                          nsValueInitializer: { NSValue(mkCoordinate: $0) },
                          nsValueGetter: { $0.mkCoordinateValue },
                          equal: coordinatesEqual)
  }

  mapKit.test("MKCoordinateSpan bridging") {
    expectBridgeToNSValue(MKCoordinateSpan(latitudeDelta: 6,
                                           longitudeDelta: 79),
                          nsValueInitializer: { NSValue(mkCoordinateSpan: $0) },
                          nsValueGetter: { $0.mkCoordinateSpanValue },
                          equal: spansEqual)
  }
}

runAllTests()
