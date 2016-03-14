// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
private class B<C> {
init(c: C) {
self.c<d>(() -> d) {
}
struct d<f : e, g: e where g.h == f.hocol P {
func f<
