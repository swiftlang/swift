// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// Build the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -F %S/Inputs/privateframeworks/withprivate -o %t %S/Inputs/privateframeworks/overlay/SomeKit.swift

// Use the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate %s -verify

// Use the overlay without private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate -I %t %s

// Build the overlay with public frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -F %S/Inputs/privateframeworks/withoutprivate -o %t %S/Inputs/privateframeworks/overlay/SomeKit.swift

// Use the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withprivate %s -verify

// Use the overlay without private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-sil -o /dev/null -F %S/Inputs/privateframeworks/withoutprivate -I %t %s

// REQUIRES: objc_interop

import SomeKit

func testWidget(widget: SKWidget) {
  _ = widget.someObjCMethod()
  _ = widget.someObjCExtensionMethod()

  let ext = widget.extensionMethod()
  ext.foo()

  widget.doSomethingElse(widget)
  inlineWidgetOperations(widget)
}

func testError(widget: SKWidget) {
  let c: SKWidget.Error.Code = SKWidget.Error(.boom).getCode(from: widget)
  if c.isBoom { }
}

