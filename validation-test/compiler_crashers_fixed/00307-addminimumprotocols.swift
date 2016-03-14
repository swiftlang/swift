// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
func d(e: () -> ()) {}
class e {
    var _ = d() {
class b<g : e, d : e where g.f == d> : e
