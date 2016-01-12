// RUN: not --crash %target-swift-frontend %s -emit-silgen
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/codafi (Robert Widmann)

// ASAN Output: stack-overflow on address 0x7ffc688c8fa8 (pc 0x0000008b75dd bp 0x7ffc688c9810 sp 0x7ffc688c8fb0 T0)

struct X<T> {
    let s : X<X>
}
