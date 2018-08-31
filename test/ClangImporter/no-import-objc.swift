// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -enable-objc-interop -I %S/Inputs/no-import-objc %s -verify

// Note that we don't import ObjectiveC.
import People

class MyFrank : Frank {
  override func frank() {}
}
