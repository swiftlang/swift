// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// ASAN Output: stack-overflow on address 0x7ffe8def3f70 (pc 0x000001cf1268 bp 0x7ffe8def48f0 sp 0x7ffe8def3f00 T0)

func b<T {
class A : A.e {
func e: T.e
