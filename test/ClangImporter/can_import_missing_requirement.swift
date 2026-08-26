// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/missing-requirement %s -verify

// expected-warning@<unknown> * {{libc not found for }}

class Unique {}

#if canImport(MissingRequirement)
  class Unique {} // No diagnostic
#endif
