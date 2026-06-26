// RUN: %target-swift-emit-silgen-ossa -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -verify %s

func foo(_ f: (() -> Void) -> Void, _ b: () -> Void) {
    return withoutActuallyEscaping(b, do: f)
}
