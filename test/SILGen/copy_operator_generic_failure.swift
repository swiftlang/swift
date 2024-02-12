// RUN: %target-swift-emit-sil -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -verify

import Swift

func addressOnlyCopy<T>(t: T) -> T {
    _copy(t) // expected-error {{copy() used on a generic or existential value}}
}
