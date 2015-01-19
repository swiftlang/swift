// RUN: rm -rf %t
// RUN: %target-swift-frontend -parse %s

// RUN: %target-swift-frontend -parse %S/Inputs/sdk-protocol-class/os1.swift
// RUN: %target-swift-frontend -parse %S/Inputs/sdk-protocol-class/os2.swift
// RUN: %target-swift-frontend -parse %S/Inputs/sdk-protocol-class/os3.swift

// REQUIRES: sdk

import ObjectiveC

let p: Protocol = objc_getProtocol("NSObject")
