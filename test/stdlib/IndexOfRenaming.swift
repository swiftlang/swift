// RUN: %target-typecheck-verify-swift -swift-version 5

let a = [10, 20, 30, 40, 50, 60]

_ = a.index(of: 30) // expected-warning {{'index(of:)' is deprecated: renamed to 'firstIndex(of:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'firstIndex(of:)' instead}}
_ = a.firstIndex(of: 30)
_ = a.index(where: { $0 > 30 }) // expected-warning {{'index(where:)' is deprecated: renamed to 'firstIndex(where:)'}}{{documentation-file=deprecated-declaration}} expected-note {{use 'firstIndex(where:)' instead}}
_ = a.firstIndex(where: { $0 > 30 })
