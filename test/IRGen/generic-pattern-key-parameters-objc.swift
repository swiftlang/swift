// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -module-name main -emit-ir %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

protocol P {
  associatedtype T
}
@objc protocol Q {}

// CHECK: @_TMPC4main1A = {{.*}} i16 2,
class A<T: P & Q> {}
// CHECK: @_TMPC4main1B = {{.*}} i16 3,
class B<T: P & Q> where T.T: P & Q {}
