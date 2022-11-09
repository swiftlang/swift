// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/keypath_objc_protocol_extension_other.swift -emit-module-path %t/keypath_objc_protocol_extension_other.swiftmodule -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-ir %s -I %t

// REQUIRES: objc_interop

import keypath_objc_protocol_extension_other

public func foo(array: [any P]) {
  _ = array.filter(\.value)
}
