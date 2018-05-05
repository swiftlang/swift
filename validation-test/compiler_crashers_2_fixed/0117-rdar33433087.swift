// RUN: %target-typecheck-verify-swift

class C {
  private init() {} // expected-note {{declared here}}
  init(n: Int) {}
}

_ = C()
// expected-error@-1 {{'C' initializer is inaccessible due to 'private' protection level}}
