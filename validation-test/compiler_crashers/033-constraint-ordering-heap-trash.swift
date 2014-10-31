// RUN: %swift %s -emit-ir -o %t
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://17240924

struct d<f : e, g: e where g.h == f.h> {
}

protocol e {
    typealias h
}
