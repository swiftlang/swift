// RUN: %empty-directory(%t/cache)
// RUN: %target-run-simple-swift(-module-cache-path %t/cache) | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop
// UNSUPPORTED: OS=tvos

// Requires swift-version 4
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import MapKit

let rect = MKMapRectMake(1.0, 2.0, 3.0, 4.0)
// CHECK: {{^}}1.0 2.0 3.0 4.0{{$}}
print("\(rect.origin.x) \(rect.origin.y) \(rect.size.width) \(rect.size.height)")
