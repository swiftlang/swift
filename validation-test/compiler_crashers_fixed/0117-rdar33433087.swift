// RUN: %target-typecheck-verify-swift

class C {
  private init() {}
  init(n: Int) {} // expected-note {{'init(n:)' declared here}}
}

// TODO(diagnostics): Once "inaccessible members" are ported to the new framework it would be possible
// to bring back `'C' initializer is inaccessible due to 'private' protection level` diagnostic here.
_ = C()
// expected-error@-1 {{missing argument for parameter 'n' in call}} {{7-7=n: <#Int#>}}
