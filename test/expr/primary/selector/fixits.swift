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

// Copy the source, apply the Fix-Its, and compile it again, making
// sure that we've cleaned up all of the deprecation warnings.
// RUN: %empty-directory(%t.sources)
// RUN: %empty-directory(%t.remapping)
// RUN: cp %s %t.sources/fixits.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t.overlays) -typecheck %t.sources/fixits.swift -fixit-all -emit-fixits-path %t.remapping/fixits.remap
// RUN: %utils/apply-fixit-edits.py %t.remapping
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t.overlays) -typecheck %t.sources/fixits.swift 2> %t.result

// RUN: %FileCheck %s < %t.result
// RUN: grep -c "warning:" %t.result | grep 3

// CHECK: warning: no method declared with Objective-C selector 'unknownMethodWithValue:label:'
// CHECK: warning: string literal is not a valid Objective-C selector
// CHECK: warning: string literal is not a valid Objective-C selector

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
