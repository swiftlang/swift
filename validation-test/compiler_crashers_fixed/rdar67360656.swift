// RUN: not %target-swift-frontend -typecheck %s

public typealias GUID = (a: UInt32, b: UInt16, c: UInt16, d: UInt8, e: UInt8, f: UInt8, g: UInt8, h: UInt8, i: UInt8, j: UInt8, k: UInt8)
let function1: @convention(c) (UnsafePointer<GUID>) -> Void
