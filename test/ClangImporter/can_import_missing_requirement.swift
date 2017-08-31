// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/missing-requirement %s -verify

// REQUIRES: objc_interop

class Unique {}

#if canImport(MissingRequirement)
  class Unique {} // No diagnostic
#endif
