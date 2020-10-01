// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil %s -verify
// REQUIRES: objc_interop

import Foundation

func mirrorInstancePropertyAsStaticMethod() {
  // Instance properties are mirrored as static _methods_. Make sure this works.
  let _: AnyClass = NSObject.classForCoder()
}
