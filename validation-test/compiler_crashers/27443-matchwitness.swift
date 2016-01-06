// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// Crash type: memory error ("Invalid read of size 8")
{e
struct B:a{let t:a}
protocol a{let t:a
