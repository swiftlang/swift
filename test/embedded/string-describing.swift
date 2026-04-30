// RUN: %target-swift-emit-ir -verify -verify-ignore-unrelated %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct MyStruct: CustomStringConvertible {
  public var description: String { "" }
}

public struct MyStreamableStruct: TextOutputStreamable {
  public func write<Target: TextOutputStream>(to target: inout Target) {}
}

public func foo(s: MyStruct) {
  _ = String(describing: s)
}

public func bar(s: MyStreamableStruct) {
  _ = String(describing: s)
}