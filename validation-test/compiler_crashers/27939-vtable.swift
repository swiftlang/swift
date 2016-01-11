// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// ASAN Output: stack-overflow on address 0x7ffdad0b1cd0 (pc 0x000001cf1268 bp 0x7ffdad0b2650 sp 0x7ffdad0b1c60 T0)

func b<T {
class A : A.e {
func e: T.e
