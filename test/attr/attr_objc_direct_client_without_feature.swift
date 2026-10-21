// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// A client without the feature can import a Swift '@objcDirect' member, and
// #selector and #keyPath on it are still errors.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule \
// RUN:   -module-name Lib -enable-experimental-feature ObjCDirect %t/Lib.swift

/// The client does not enable the feature.
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/Client.swift

//--- Lib.swift

import Foundation

@objc public class C: NSObject {
  @objcDirect public final func f() {}
  @objcDirect public var q: Int = 0
}

//--- Client.swift

import Foundation
import Lib

func use() {
  _ = #selector(C.f)
  // expected-error@-1 {{argument of '#selector' refers to instance method 'f()' that is a direct method}}
  // expected-note@Lib.C.f:2 {{a direct method is not added to its class's Objective-C method list}}
  _ = #keyPath(C.q)
  // expected-error@-1 {{argument of '#keyPath' refers to property 'q' that is direct}}
  // expected-note@Lib.C.q:2 {{key-value coding cannot reach the property}}
}
