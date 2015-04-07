// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

enum S<T> : > T {
       }
}
protocol P {
    func f<T>()(T)-> T
}
func b(c) -> <d>((d) {
}
ias h
