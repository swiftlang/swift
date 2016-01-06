// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// Crash type: memory error ("Invalid read of size 4")
class n{protocol a:d var d={class b:a
