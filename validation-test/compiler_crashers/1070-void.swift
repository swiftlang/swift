// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %target-swift-frontend %s -module-cache-path %t/clang-module-cache -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import CoreData
if true {
struct Q<T where I.c : a {
deinit {
protocol a {
}
protocol A {
struct S {
}
}
}
println() {
let f = A(x: String = "A>(b: d = A.b = c()
typealias R = F>()
