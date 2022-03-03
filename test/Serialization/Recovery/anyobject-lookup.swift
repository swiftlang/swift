/// Recover from searching though all possible methods on AnyObject.
/// rdar://89494507

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -DLIB_B %s -module-name B -emit-module-path %t/B.swiftmodule -I %t -I %S/Inputs/custom-modules -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -DLIB_C %s -I %t -verify
// Add this flag to the previous line to get back the original crash: -disable-deserialization-recovery

// REQUIRES: objc_interop

#if LIB_B

import Foundation
@_implementationOnly import AnyObjectLookup

@objc internal class InternalClass: ImplOnlyBase {
  @objc func invisibleMethod() {
  }
}

#elseif LIB_C

import B

func use(_ obj: AnyObject) {
  obj.invisibleMethod() // expected-error {{value of type 'AnyObject' has no member 'invisibleMethod'}}
}

#endif
