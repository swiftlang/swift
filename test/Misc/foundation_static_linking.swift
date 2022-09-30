// REQUIRES: OS=linux-gnu
// RUN: %target-swiftc_driver -static-stdlib -o %t/foundation_static_linking %s 
// RUN: not --crash  %t/foundation_static_linking

import Foundation

_ = try! Data(contentsOf: URL(string: "http://httpbin.org/get")!)
