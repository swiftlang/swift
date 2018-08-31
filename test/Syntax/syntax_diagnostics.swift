// RUN: %target-swift-frontend -emit-syntax -primary-file %s -verify

typealias Inner: Foo // expected-error{{unknown declaration syntax exists in the source}} expected-error{{expected '=' in type alias declaration}}
