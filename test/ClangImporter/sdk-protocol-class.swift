// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck %s

// RUN: %target-swift-frontend -typecheck %S/Inputs/sdk-protocol-class/os1.swift
// RUN: %target-swift-frontend -typecheck %S/Inputs/sdk-protocol-class/os2.swift
// RUN: %target-swift-frontend -typecheck %S/Inputs/sdk-protocol-class/os3.swift

// REQUIRES: objc_interop

import ObjectiveC

let p: Protocol = objc_getProtocol("NSObject")
