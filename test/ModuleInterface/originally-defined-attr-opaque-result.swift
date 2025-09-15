// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/CoreVegetable.swiftinterface) %S/Inputs/CoreVegetable.swift -target %target-swift-5.1-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/CoreVegetable.swiftinterface)
// RUN: %target-swift-emit-module-interface(%t/CoreChef.swiftinterface) %s -module-name CoreChef -I %t -target %target-swift-5.1-abi-triple -DLIB
// RUN: %target-swift-typecheck-module-from-interface(%t/CoreChef.swiftinterface) -module-name CoreChef -I %t -target %target-swift-5.1-abi-triple

// Also build the module itself with -g to exercise debug info round tripping.
// RUN: %target-swift-frontend -emit-ir -g %s -I %t -target %target-swift-5.1-abi-triple

// RUN: %FileCheck %s < %t/CoreChef.swiftinterface

// REQUIRES: OS=macosx
import CoreVegetable

public protocol Soup {}

public struct VegetableSoup : Soup {}

public protocol Chef {
  associatedtype Food

  func cookSoup(_: Vegetable) -> Food
}

public struct SoupChef : Chef {
  public func cookSoup(_: Vegetable) -> some Soup { VegetableSoup() }
}

// CHECK-LABEL: public typealias Food = @_opaqueReturnTypeOf("$s8CoreChef04SoupB0V04cookC0yQr0aC09VegetableVF", 0) __
