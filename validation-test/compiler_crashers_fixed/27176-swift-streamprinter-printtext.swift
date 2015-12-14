// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

{struct Q{class B:CollectionType}class B<T{struct Q<T where T:B{class A:B<T>
