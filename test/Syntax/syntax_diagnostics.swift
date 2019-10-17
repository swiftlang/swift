// RUN: %target-swift-frontend -emit-syntax -primary-file %s -verify

typealias Inner: Foo // expected-error{{expected '=' in type alias declaration}}
