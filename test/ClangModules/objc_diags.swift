// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

import ObjectiveC

func instanceMethod(b: B) {
  b.method(1, 2.5) // expected-error {{cannot invoke 'method' with an argument list of type '(Int, Double)'}}
}
