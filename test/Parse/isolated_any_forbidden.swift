// RUN: %target-typecheck-verify-swift 

typealias FnType = @isolated(any) () -> () // expected-error {{attribute requires '-enable-experimental-feature IsolatedAny'}}
