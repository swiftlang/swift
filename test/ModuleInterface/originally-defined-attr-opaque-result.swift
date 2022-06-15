// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/CoreVegetable.swiftinterface %S/Inputs/CoreVegetable.swift -disable-availability-checking -enable-library-evolution -swift-version 5
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/CoreChef.swiftinterface -module-name CoreChef %s -I %t -disable-availability-checking -enable-library-evolution -swift-version 5 -DLIB

// Also build the module itself with -g to exercise debug info round tripping.
// RUN: %target-swift-frontend -emit-ir -g %s -I %t -disable-availability-checking

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
