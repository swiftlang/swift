// RUN: rm -rf %t
// RUN: mkdir -p %t

// Build the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -F %S/Inputs/privateframeworks/withprivate -o %t %S/Inputs/privateframeworks/overlay/SomeKit.swift

// Use the overlay with private frameworks.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -F %S/Inputs/privateframeworks/withprivate -I %t %s -verify

import SomeKit



func testWidget(widget: SKWidget) {
  _ = widget.extensionMethod()
}

