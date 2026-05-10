// RUN: %target-swift-emit-silgen -verify %s

func foo(_ f: (() -> Void) -> Void, _ b: () -> Void) {
    return withoutActuallyEscaping(b, do: f)
}
