// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/MyModule.swiftinterface) %s -module-name MyModule -internal-import-bridging-header %S/../Inputs/c-bridging-header.h -sdk %clang-importer-sdk -enable-library-evolution

// RUN: %FileCheck %s < %t/MyModule.swiftinterface

// CHECK-NOT: internal-import-bridging-header

// CHECK: public func g()

func getX(point: MyPoint) -> Double { point.x }

public func g() {
}
