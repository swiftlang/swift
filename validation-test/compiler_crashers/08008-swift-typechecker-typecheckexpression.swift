// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// ASAN Output: stack-overflow on address 0x7fffe2a98fd8 (pc 0x000001e12adb bp 0x7fffe2a992d0 sp 0x7fffe2a98f60 T0)

class A:A.b{let b=Void{
