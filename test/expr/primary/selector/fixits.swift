// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.overlays)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t.overlays %clang-importer-sdk-path/swift-modules/ObjectiveC.swift -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t.overlays %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t.overlays %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t.overlays %S/Inputs/fixits_helper.swift -module-name Helper

// Make sure we get the right diagnostics.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t.overlays) -typecheck %s -verify

import Foundation
import Helper

func testDeprecatedStringLiteralSelector() {
  let sel1: Selector = "methodWithValue:label:" // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{24-48=#selector(Foo.method(_:label:))}}
  _ = sel1

  _ = "methodWithValue:label:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-43=#selector(Foo.method(_:label:))}}
  _ = "property" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-29=#selector(getter: Foo.property)}}
  _ = "setProperty:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-33=#selector(setter: Foo.property)}}
  _ = "unknownMethodWithValue:label:" as Selector // expected-warning{{no method declared with Objective-C selector 'unknownMethodWithValue:label:'}}{{7-7=Selector(}}{{38-50=)}}
  _ = "badSelector:label" as Selector // expected-warning{{string literal is not a valid Objective-C selector}}
  _ = "method2WithValue:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-38=#selector(Foo.method2(_:))}}
  _ = "method3" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-28=#selector(Foo.method3)}}

  // Overloaded cases
  _ = "overloadedWithInt:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-39=#selector(Bar.overloaded(_:) as (Bar) -> (Int) -> ())}}
  _ = "overloadedWithString:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-42=#selector(Bar.overloaded(_:) as (Bar) -> (String) -> ())}}

  _ = "staticOverloadedWithInt:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-45=#selector(Bar.staticOverloaded(_:) as (Int) -> ())}}
  _ = "staticOverloadedWithString:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-48=#selector(Bar.staticOverloaded(_:) as (String) -> ())}}

  // We don't need coercion here because we get the right selector
  // from the static method.
  _ = "staticOrNonStatic:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-39=#selector(Bar.staticOrNonStatic(_:))}}

  // We need coercion here because we asked for a selector from an
  // instance method with the same name as (but a different selector
  // from) a static method.
  _ = "theInstanceOne:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-36=#selector(Bar.staticOrNonStatic2(_:) as (Bar) -> (Int) -> ())}}

  // Note: from Foundation
  _ = "initWithArray:" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-35=#selector(NSSet.init(array:))}}

  // Note: from Foundation overlay
  _ = "methodIntroducedInOverlay" as Selector // expected-warning{{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{7-46=#selector(NSArray.introducedInOverlay)}}
}

func testSelectorConstruction() {
  _ = Selector("methodWithValue:label:") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-41=#selector(Foo.method(_:label:))}}
  _ = Selector("property") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-27=#selector(getter: Foo.property)}}
  _ = Selector("setProperty:") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-31=#selector(setter: Foo.property)}}

  _ = Selector("unknownMethodWithValue:label:") // expected-warning{{no method declared with Objective-C selector 'unknownMethodWithValue:label:'}}
  // expected-note@-1{{wrap the selector name in parentheses to suppress this warning}}{{16-16=(}}{{47-47=)}}
  _ = Selector(("unknownMethodWithValue:label:"))
  _ = Selector("badSelector:label") // expected-warning{{string literal is not a valid Objective-C selector}}
  _ = Selector("method2WithValue:") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-36=#selector(Foo.method2(_:))}}
  _ = Selector("method3") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-26=#selector(Foo.method3)}}

  // Note: from Foundation
  _ = Selector("initWithArray:") // expected-warning{{use '#selector' instead of explicitly constructing a 'Selector'}}{{7-33=#selector(NSSet.init(array:))}}
}

@objc class Selectors: NSObject {
  func takeSel(_: Selector) {}
  @objc func mySel() {}
  func test() {
    takeSel("mySel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-20=#selector(self.mySel)}}
    takeSel(Selector("mySel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-30=#selector(self.mySel)}}
  }
}

@objc class OtherClass: NSObject {
  func test(s: Selectors) {
    s.takeSel("mySel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-22=#selector(Selectors.mySel)}}
    s.takeSel(Selector("mySel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-32=#selector(Selectors.mySel)}}
  }
}

@objc class Base: NSObject {
  @objc func baseSel() {}
}

@objc class Outer: NSObject {
  func takeSel(_: Selector) {}
  @objc func outerSel() {}

  @objc class Inner: Base {
    func takeSel(_: Selector) {}

    @objc func innerSel() {}

    func test(s: Selectors, o: Outer) {
      s.takeSel("mySel")
      // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{17-24=#selector(Selectors.mySel)}}
      s.takeSel(Selector("mySel"))
      // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{17-34=#selector(Selectors.mySel)}}

      takeSel("innerSel")
      // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-25=#selector(self.innerSel)}}
      takeSel(Selector("innerSel"))
      // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-35=#selector(self.innerSel)}}

      takeSel("baseSel")
      // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-24=#selector(self.baseSel)}}
      takeSel(Selector("baseSel"))
      // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-34=#selector(self.baseSel)}}

      o.takeSel("outerSel")
      // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{17-27=#selector(Outer.outerSel)}}
      o.takeSel(Selector("outerSel"))
      // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{17-37=#selector(Outer.outerSel)}}
    }
  }

  func test(s: Selectors, i: Inner) {
    s.takeSel("mySel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-22=#selector(Selectors.mySel)}}
    s.takeSel(Selector("mySel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-32=#selector(Selectors.mySel)}}

    i.takeSel("innerSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-25=#selector(Inner.innerSel)}}
    i.takeSel(Selector("innerSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-35=#selector(Inner.innerSel)}}

    i.takeSel("baseSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-24=#selector(Base.baseSel)}}
    i.takeSel(Selector("baseSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-34=#selector(Base.baseSel)}}

    takeSel("outerSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-23=#selector(self.outerSel)}}
    takeSel(Selector("outerSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-33=#selector(self.outerSel)}}
  }
}

extension Outer {
  func test2(s: Selectors, i: Inner) {
    s.takeSel("mySel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-22=#selector(Selectors.mySel)}}
    s.takeSel(Selector("mySel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-32=#selector(Selectors.mySel)}}

    i.takeSel("innerSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-25=#selector(Inner.innerSel)}}
    i.takeSel(Selector("innerSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-35=#selector(Inner.innerSel)}}

    i.takeSel("baseSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{15-24=#selector(Base.baseSel)}}
    i.takeSel(Selector("baseSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{15-34=#selector(Base.baseSel)}}

    takeSel("outerSel")
    // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-23=#selector(self.outerSel)}}
    takeSel(Selector("outerSel"))
    // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-33=#selector(self.outerSel)}}
  }
}

func freeTest(s: Selectors, o: Outer, i: Outer.Inner) {
  s.takeSel("mySel")
  // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-20=#selector(Selectors.mySel)}}
  s.takeSel(Selector("mySel"))
  // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-30=#selector(Selectors.mySel)}}

  // FIXME: Fix-it should be 'Outer.Inner.innerSel'.
  i.takeSel("innerSel")
  // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-23=#selector(Inner.innerSel)}}
  i.takeSel(Selector("innerSel"))
  // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-33=#selector(Inner.innerSel)}}

  i.takeSel("baseSel")
  // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-22=#selector(Base.baseSel)}}
  i.takeSel(Selector("baseSel"))
  // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-32=#selector(Base.baseSel)}}

  o.takeSel("outerSel")
  // expected-warning@-1 {{use of string literal for Objective-C selectors is deprecated; use '#selector' instead}}{{13-23=#selector(Outer.outerSel)}}
  o.takeSel(Selector("outerSel"))
  // expected-warning@-1 {{use '#selector' instead of explicitly constructing a 'Selector'}}{{13-33=#selector(Outer.outerSel)}}
}
