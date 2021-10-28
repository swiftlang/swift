// RUN: %target-swift-emit-sil -enable-experimental-move-only -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -verify

import Swift

func addressOnlyMove<T>(t: T) -> T {
    _move(t) // expected-error {{move() used on a generic or existential value}}
}
