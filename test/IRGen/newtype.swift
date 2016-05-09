// RUN// RUN: %target-swift-frontend -sdk %clang-importer-sdk -I %S/../IDE/Inputs/custom-modules %s -emit-ir -enable-swift-newtype | FileCheck %s
import Newtype

// REQUIRES: objc_interop

// CHECK-LABEL: define %CSo8NSString* @_TF7newtype14getErrorDomainFT_VSC11ErrorDomain()
public func getErrorDomain() -> ErrorDomain {
  // CHECK: load %CSo8NSString*, %CSo8NSString** getelementptr inbounds (%VSC11ErrorDomain, %VSC11ErrorDomain* @SNTErrOne
  return .one
}

// CHECK-LABEL: _TF7newtype6getFooFT_VSC18NSNotificationName
public func getFoo() -> NSNotificationName {
  return NSNotificationName.foo
  // CHECK: load {{.*}} @FooNotification
  // CHECK: ret
}

// CHECK-LABEL: _TF7newtype21getGlobalNotificationFSiSS
public func getGlobalNotification(_ x: Int) -> String {
  switch x {
    case 1: return kNotification
    // CHECK: load {{.*}} @kNotification
    case 2: return Notification
    // CHECK: load {{.*}} @Notification
    case 3: return swiftNamedNotification
    // CHECK: load {{.*}} @kSNNotification
    default: return NSNotificationName.bar.rawValue
    // CHECK: load {{.*}} @kBarNotification
  }
// CHECK: ret
}
