// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_feature_Embedded
// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -emit-ir -emit-module -o %t/MyModule.ll -emit-module-path %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library -module-name MyModule
// RUN: %FileCheck %s -check-prefix MODULE-IR < %t/MyModule.ll

// RUN: %target-swift-frontend -emit-ir -o %t/Client.ll %t/Client.swift -I %t -enable-experimental-feature Embedded -parse-as-library -module-name Client
// RUN: %FileCheck %s -check-prefix CLIENT-IR < %t/Client.ll

//--- MyModule.swift

// MODULE-IR-NOT: @"$e8MyModule4BoomVs5ErrorAAWP"
public struct Boom: Error {
  public init() {}
}

//--- Client.swift

import MyModule

// CLIENT-IR-DAG: @"$e8MyModule4BoomVs5ErrorAAWP" = linkonce_odr {{.*}}constant
// CLIENT-IR-NOT: @"$e8MyModule4BoomVs5ErrorAAWP" = external
public func mightFail() throws {
  throw Boom()
}
