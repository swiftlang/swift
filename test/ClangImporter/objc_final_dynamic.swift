// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import Foundation

// Note: make sure we don't get a bogus error from nowhere,
//   error: a declaration cannot be both 'final' and 'dynamic'
extension NSObject {
  public static let staticIntProperty: Int = 17
}

final class MyClass : NSObject { }

extension MyClass {
  public static var otherStaticIntProperty: Int = 17
}
