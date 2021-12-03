// RUN: %target-swift-emit-sil -enable-experimental-move-only -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -verify

import Swift

func addressOnlyMove<T>(t: T) -> T {
    _move(t) // expected-error {{_move applied to value that the compiler does not know how to check. Please file a bug or an enhancement request!}}
}
