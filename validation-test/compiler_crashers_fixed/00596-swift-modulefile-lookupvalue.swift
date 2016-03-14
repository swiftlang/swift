// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
struct D : C {
func g<T where T.E == F>(f: B<T>) {
}
}
struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : A>
