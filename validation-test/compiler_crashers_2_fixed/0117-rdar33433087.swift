// RUN: not %target-swift-frontend %s -typecheck

class C {
  private init() {} // expected-error {{declared here}}
  init(n: Int) {}
}

_ = C()
// expected-error@-1 {{'C' initializer is inaccessible due to 'private' protection level}}
