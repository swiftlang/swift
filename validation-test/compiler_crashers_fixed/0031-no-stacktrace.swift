// RUN: %target-swift-frontend %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://17240590

protocol a {
}
protocol b : a {
}
protocol c : a {
}
protocol d {
    typealias e = a
}
struct e : d {
    typealias e = b
}
func g<H : d where H.e == b> (n: H) {
}
func g<I : d where I.e == c> (n: I) {
}
