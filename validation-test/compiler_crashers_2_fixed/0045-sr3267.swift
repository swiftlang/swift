// RUN: %target-swift-frontend %s -parse -emit-silgen

// <https://bugs.swift.org/browse/SR-3267>
let function: () -> Any = { () -> Void in }
