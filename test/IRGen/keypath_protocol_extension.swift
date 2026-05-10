// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/keypath_protocol_extension_other.swift -emit-module-path %t/keypath_protocol_extension_other.swiftmodule
// RUN: %target-swift-frontend -target %target-swift-5.7-abi-triple -emit-ir %s -I %t

import keypath_protocol_extension_other

public func foo(array: [any P<String>]) {
  _ = array.filter(\.value)
}
