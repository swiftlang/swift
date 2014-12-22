// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: asan

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

private class B<C> {
init(c: C) {
self.c<d>(() -> d) {
}
struct d<f : e, g: e where g.h == f.hocol P {
func f<
