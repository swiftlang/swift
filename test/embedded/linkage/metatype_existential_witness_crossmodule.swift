// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/MyModule.ll -emit-module-path %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library -module-name MyModule
// RUN: %FileCheck %s -check-prefix MODULE-IR < %t/MyModule.ll

// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -I %t -enable-experimental-feature Embedded -parse-as-library -module-name Client
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- MyModule.swift

// MODULE-IR-NOT: @"$e8MyModule5ThingVAA1PAAWP"
// MODULE-IR-NOT: @"$e8MyModule10ClassThingCAA1QAAWP"
public protocol P {
  func foo()
}
public struct Thing: P {
  public init() {}
  public func foo() {}
}
public protocol Q: AnyObject {
  func bar()
}
public final class ClassThing: Q {
  public init() {}
  public func bar() {}
}

//--- Client.swift

import MyModule

// CLIENT-IR-DAG: @"$e8MyModule5ThingVAA1PAAWP" = linkonce_odr hidden constant
// CLIENT-IR-DAG: @"$e8MyModule10ClassThingCAA1QAAWP" = linkonce_odr hidden constant
// CLIENT-IR-NOT: @"$e8MyModule5ThingVAA1PAAWP" = external
// CLIENT-IR-NOT: @"$e8MyModule10ClassThingCAA1QAAWP" = external
public func makeStructMeta() -> any P.Type {
  return Thing.self
}
public func makeClassMeta() -> any Q.Type {
  return ClassThing.self
}
