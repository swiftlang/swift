// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/missing-requirement %s -verify

// REQUIRES: objc_interop
// REQUIRES: can_import

class Unique {}

#if canImport(MissingRequirement)
  class Unique {} // No diagnostic
#endif
