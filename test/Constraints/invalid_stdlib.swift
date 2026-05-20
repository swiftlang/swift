// RUN: %target-typecheck-verify-swift -parse-stdlib

// This file is for tests that used to cause the type checker to crash.

class DictStringInt {
  init(dictionaryLiteral xs: ()...) {} // expected-error 2{{broken standard library: cannot find Array type}}
}
