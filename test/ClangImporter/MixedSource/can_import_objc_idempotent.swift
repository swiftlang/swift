// RUN: %empty-directory(%t)
// RUN: cp -R %S/Inputs/mixed-target %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -enable-objc-interop -import-objc-header %t/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -typecheck %s -verify

// RUN: rm -rf %t/mixed-target/
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -typecheck %s -verify

// REQUIRES: objc_interop

// Test that 'canImport(Foo)' directives do not open symbols from 'Foo' into the
// current module.  Only an 'import Foo' statement should do this.

#if canImport(AppKit)
  class AppKitView : NSView {} // expected-error {{cannot find type 'NSView' in scope}}
#endif

#if canImport(UIKit)
  class UIKitView : UIView {} // expected-error {{cannot find type 'UIView' in scope}}
#endif

#if canImport(CoreGraphics)
  let square = CGRect(x: 100, y: 100, width: 100, height: 100)
  // expected-error@-1 {{cannot find 'CGRect' in scope}}

  let (r, s) = square.divided(atDistance: 50, from: .minXEdge)
#endif

#if canImport(MixedWithHeader)
let object = NSObject() // expected-error {{cannot find 'NSObject' in scope}}
let someAPI = Derived() // expected-error {{cannot find 'Derived' in scope}}
#endif
