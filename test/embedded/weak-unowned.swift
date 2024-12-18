// RUN: %target-swift-emit-ir %s -wmo
// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -verify

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public class MyClass { }

public struct MyStruct {
  var normalVar: MyClass
  weak var weakVar: MyClass? // expected-error {{attribute 'weak' cannot be used in embedded Swift}}
  unowned var unownedVar: MyClass // expected-error {{attribute 'unowned' cannot be used in embedded Swift}}
  unowned(unsafe) var unownedUnsafe: MyClass
}
