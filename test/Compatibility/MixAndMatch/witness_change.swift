// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/SomeObjCModule.swiftmodule -module-name SomeObjCModule -I %t -I %S/Inputs -swift-version 3 %S/Inputs/SomeObjCModuleX.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/SomeSwift3Module.swiftmodule -module-name SomeSwift3Module -I %t -I %S/Inputs -swift-version 3 %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t/SomeSwift4Module.swiftmodule -module-name SomeSwift4Module -I %t -I %S/Inputs -swift-version 4 %S/Inputs/witness_change_swift4.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -c -I %t -I %S/Inputs -swift-version 3 %S/Inputs/witness_change_swift3_leaf.swift

// REQUIRES: objc_interop

// Swift 3 sees the ObjC class NSRuncibleSpoon as the class, and uses methods
// with type signatures involving NSRuncibleSpoon to conform to protocols
// across the language boundary. Swift 4 sees the type as bridged to
// a RuncibleSpoon value type, but still needs to be able to use conformances
// declared by Swift 3.

// Swift 3

import SomeObjCModule

_ = RuncibleSpoon()

public class SomeSwift3Class: NSObject {
  public required init(someObjCInitRequirement x: NSRuncibleSpoon) {
    someObjCPropertyRequirement = x
  }
  public func someObjCMethodRequirement(_: NSRuncibleSpoon) {}
  public var someObjCPropertyRequirement: NSRuncibleSpoon
}
extension SomeSwift3Class: SomeObjCProtocol {}

public protocol SomeSwift3Protocol {
  init(someSwiftInitRequirement: NSRuncibleSpoon)
  func someSwiftMethodRequirement(_: NSRuncibleSpoon)
  var someSwiftPropertyRequirement: NSRuncibleSpoon { get }
}
extension SomeObjCClass: SomeSwift3Protocol {}

