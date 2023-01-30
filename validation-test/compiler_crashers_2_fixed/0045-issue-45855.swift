// RUN: %target-swift-frontend %s -parse -emit-silgen

// https://github.com/apple/swift/issues/45855

let function: () -> Any = { () -> Void in }
