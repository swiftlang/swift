// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/a.swift -emit-module-path %t/a.swiftmodule
// RUN: %target-swift-frontend -emit-module -primary-file %S/Inputs/b.swift -emit-module-path %t/b.swiftmodule -I %t
// RUN: not --crash %target-swift-frontend-typecheck -verify -I %t %s

// SR-12526: Fix cross-module deserialization crash involving `@derivative` attribute.

import a
import b

func foo(_ s: Struct) {
  _ = Struct()
  _ = s.method(1)
}
