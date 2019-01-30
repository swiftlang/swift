// RUN: %target-swift-frontend -typecheck -verify %s

@testable func foo() {} // expected-error {{@testable may only be used on 'import' declarations}} {{1-11=}}
