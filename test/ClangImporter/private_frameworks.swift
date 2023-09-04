// REQUIRES: rdar113413583

// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// Build the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -F %S/Inputs/privateframeworks/withprivate -o %t %S/Inputs/privateframeworks/overlay/SomeKit.swift -enable-library-evolution -emit-module-interface-path %t/SomeKit.swiftinterface

// Use the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate -swift-version 4 %s -import-objc-header %S/Inputs/privateframeworks/bridging-somekitcore.h

// Using the binary overlay without private frameworks should fail.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate -swift-version 4 -import-objc-header %S/Inputs/privateframeworks/bridging-somekit.h %s -verify

// Using an overlay rebuilt from the swiftinterface without private frameworks should work.
// RUN: rm %t/SomeKit.swiftmodule
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate -swift-version 4 -import-objc-header %S/Inputs/privateframeworks/bridging-somekit.h %s

// Build the overlay with public frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -F %S/Inputs/privateframeworks/withoutprivate -o %t %S/Inputs/privateframeworks/overlay/SomeKit.swift

// Use the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate -swift-version 4 %s -import-objc-header %S/Inputs/privateframeworks/bridging-somekitcore.h

// Use the overlay without private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate -swift-version 4 %s -import-objc-header %S/Inputs/privateframeworks/bridging-somekit.h 

// Use something that uses the overlay.
// RUN: echo 'import private_frameworks; testErrorConformance()' > %t/main.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t -F %S/Inputs/privateframeworks/withprivate -swift-version 4 %s -import-objc-header %S/Inputs/privateframeworks/bridging-somekitcore.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate %t/main.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate %t/main.swift

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t -F %S/Inputs/privateframeworks/withoutprivate -swift-version 4 %s -import-objc-header %S/Inputs/privateframeworks/bridging-somekit.h
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate %t/main.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate %t/main.swift

// REQUIRES: objc_interop

import SomeKit

func testWidget(widget: SKWidget) {
  widget.someObjCMethod()
  widget.someObjCExtensionMethod()

  let ext = widget.extensionMethod() // expected-error {{value of type 'SKWidget' has no member 'extensionMethod'}}
  ext.foo()

  widget.doSomethingElse(widget) // expected-error {{value of type 'SKWidget' has no member 'doSomethingElse'; did you mean 'doSomething'?}}
  inlineWidgetOperations(widget) // expected-error {{cannot find 'inlineWidgetOperations' in scope}}

  let _ = widget.name
}

func testError(widget: SKWidget) {
  let c: SKWidget.Error.Code = SKWidget.Error(.boom).getCode(from: widget) // expected-error {{value of type 'SKWidget.Error' has no member 'getCode'}}
  if c.isBoom { } // expected-error {{value of type 'SKWidget.Error.Code' has no member 'isBoom'}}
}

func testGlobals() {
  someKitGlobalFunc()
  SomeKit.someKitOtherGlobalFunc()
  someKitOtherGlobalFunc()
}

public struct ErrorsOnly<T: Error> {}
public func testErrorConformance(_ code: ErrorsOnly<SKWidget.Error>? = nil) {}
