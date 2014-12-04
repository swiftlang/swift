// RUN: rm -rf %t
// RUN: %target-build-swift -parse %s

// RUN: %target-build-swift -parse %S/Inputs/sdk-protocol-class/os1.swift
// RUN: %target-build-swift -parse %S/Inputs/sdk-protocol-class/os2.swift
// RUN: %target-build-swift -parse %S/Inputs/sdk-protocol-class/os3.swift

// REQUIRES: sdk

import ObjectiveC

let p: Protocol = objc_getProtocol("NSObject")
