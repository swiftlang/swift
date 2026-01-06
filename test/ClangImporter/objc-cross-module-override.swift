// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module %S/Inputs/overlay_extension_initializer.swift -Xcc -isystem -Xcc %S/Inputs/custom-modules -module-name ImageInitializers -o %t
// RUN: not %target-swift-frontend -I %S/Inputs/custom-modules -I %t -typecheck -primary-file %s -diagnostic-style llvm 2>&1 | %FileCheck %s

// REQUIRES: objc_interop

// N.B. This test is a bit odd since we are going to check for the presence of a
// diagnostic being emitted in a different module. The diagnostic verifier isn't
// really setup to test that yet.

import ImageInitializers

final class MyImage : Image {
  // CHECK: non-'@objc' initializer 'init(imageLiteralResourceName:)' is declared in extension of 'Image' and cannot be overridden
  // Make sure we aren't emitting a fixit into the extant module...
  // CHECK-NOT: add '@objc' to make this declaration overridable
  // CHECK: ImageInitializers.Image.init:{{.*}}: note: overridden declaration is here
  override required convenience init(imageLiteralResourceName name: String) { }
}
