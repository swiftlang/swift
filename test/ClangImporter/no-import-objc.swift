// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/no-import-objc %s -verify

// REQUIRES: objc_interop

// Note that we don't import ObjectiveC.
import People

class MyFrank : Frank {
  override func frank() {}
}
