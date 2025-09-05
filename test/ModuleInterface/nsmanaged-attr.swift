// RUN: %empty-directory(%t)

// This test ensures we can properly load modules that have @NSManaged properties.

// 1. Emit this file to a module interface
// RUN: %target-swift-emit-module-interface(%t/Module.swiftinterface) %s -module-name Module

// 2. Check the interface against what we expect
// RUN: %FileCheck %s < %t/Module.swiftinterface

// 3. Ensure we can load this module from its interface
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.swiftinterface) -module-name Module

// REQUIRES: objc_interop

import CoreData
import Foundation

// CHECK: @objc @_inheritsConvenienceInitializers{{ nonisolated | }}public class MyObject : CoreData.NSManagedObject {
public class MyObject: NSManagedObject {
  // CHECK: @objc @NSManaged{{ nonisolated | }}dynamic public var myVar: Swift.String {
  // CHECK-NEXT: @objc get
  // CHECK-NEXT: @objc set
  // CHECK-NEXT: }
  @NSManaged public var myVar: String
  // CHECK: @NSManaged @objc{{ nonisolated | }}dynamic public var myVar2: Swift.String {
  // CHECK-NEXT: @objc get
  // CHECK-NEXT: @objc set
  // CHECK-NEXT: }
  @NSManaged @objc public var myVar2: String
  // CHECK: @NSManaged @objc dynamic{{ nonisolated | }}public var myVar3: Swift.String {
  // CHECK-NEXT: @objc get
  // CHECK-NEXT: @objc set
  // CHECK-NEXT: }
  @NSManaged @objc dynamic public var myVar3: String
  // CHECK: @NSManaged @objc dynamic{{ nonisolated | }}public var myVar4: Swift.String {
  // CHECK-NEXT: @objc get
  // CHECK-NEXT: }
  @NSManaged @objc dynamic public private(set) var myVar4: String
// CHECK: }
}
