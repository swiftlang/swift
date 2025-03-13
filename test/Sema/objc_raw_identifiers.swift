// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -verify -emit-module -o %t %s -module-name RawIdentifier -disable-objc-attr-requires-foundation-module

// REQUIRES: objc_interop

@objc(`Foo Bar`) public class FooBar {  // expected-error {{'@objc' class name is not a valid Objective-C identifier}}
  @objc func `function with spaces`() {}  // expected-error {{cannot infer '@objc' instance method name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}

  @objc(for) func forFunction1() {}
  @objc(`while`) func whileFunction() {}
  public func `not exported`() {}
}

@objc @objcMembers public class `Class with Spaces` {  // expected-error {{cannot infer '@objc' class name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
  public var `this is exported`: Int = 0  // expected-error {{cannot infer '@objc' property name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}

  public var `another var`: Int {  // expected-error {{cannot infer '@objc' property name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
    get { 0 }
    set {}
  }

  public var `yet another var`: Int {  // expected-error {{cannot infer '@objc' property name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
    @objc(`yet another getter`) get { 0 }  // expected-error {{'@objc' getter name is not a valid Objective-C identifier}}
    @objc(`yet another setter`:) set {}  // expected-error {{'@objc' setter name is not a valid Objective-C identifier}}
  }

  public func `also exported`() {}  // expected-error {{cannot infer '@objc' instance method name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}

  public init() {}

  public init(`label with space`: Int) {}  // expected-error {{cannot infer '@objc' initializer name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}

  public init(goodLabel `internal name`: Int) {}
}

@objc class NormalObjCClass {}

class `Inherits from Objective-C Class`: NormalObjCClass {  // expected-error {{cannot infer '@objc' class name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
}

@objc protocol `Some Protocol` {  // expected-error {{cannot infer '@objc' protocol name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
}

@objc enum `Some Enum`: Int {  // expected-error {{cannot infer '@objc' enum name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
  case `some case A`  // expected-error {{cannot infer '@objc' enum case name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
  @objc(someCaseB) case `some case B`
  @objc(`some case C`) case `some case C`  // expected-error {{'@objc' enum case name is not a valid Objective-C identifier}}
}

@objc enum EnumNameIsOK: Int {
  case `raw identifier`  // expected-error {{cannot infer '@objc' enum case name because the Swift name is not a valid Objective-C identifier; specify the name in '@objc' explicitly}}
}
