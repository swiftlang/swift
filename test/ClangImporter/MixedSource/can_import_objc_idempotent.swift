// RUN: rm -rf %t && mkdir -p %t
// RUN: cp -R %S/Inputs/mixed-target %t

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -import-objc-header %t/mixed-target/header.h -emit-module-path %t/MixedWithHeader.swiftmodule %S/Inputs/mixed-with-header.swift %S/../../Inputs/empty.swift -module-name MixedWithHeader -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -typecheck %s -verify

// RUN: rm -rf %t/mixed-target/
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -I %S/../Inputs/custom-modules -typecheck %s -verify

// REQUIRES: objc_interop
// REQUIRES: can_import

// Test that 'canImport(Foo)' directives do not open symbols from 'Foo' into the
// current module.  Only an 'import Foo' statement should do this.

#if canImport(AppKit)
  class AppKitView : NSView {} // expected-error {{use of undeclared type 'NSView'}}
#endif

#if canImport(UIKit)
  class UIKitView : UIView {} // expected-error {{use of undeclared type 'UIView'}}
#endif

#if canImport(CoreGraphics)
  let square = CGRect(x: 100, y: 100, width: 100, height: 100) // expected-error {{use of unresolved identifier 'CGRect'}}
  // expected-error@-1 {{'square' used within its own type}}
  // expected-error@-2 {{could not infer type for 'square'}}
  let (r, s) = square.divided(atDistance: 50, from: .minXEdge)
#endif

#if canImport(MixedWithHeader)
let object = NSObject() // expected-error {{use of unresolved identifier 'NSObject'}}
  // expected-error@-1 {{'object' used within its own type}}
  // expected-error@-2 {{could not infer type for 'object'}}
let someAPI = Derived() // expected-error {{use of unresolved identifier 'Derived'}}
#endif
