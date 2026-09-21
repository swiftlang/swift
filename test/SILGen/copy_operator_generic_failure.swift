// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-sil -parse-stdlib %s -disable-access-control -disable-objc-attr-requires-foundation-module -verify

import Swift

func addressOnlyCopy<T>(t: T) -> T {
    copy t
}
