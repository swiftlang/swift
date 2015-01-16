// RUN: %target-swift-frontend %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/jvasileff (John Vasileff)

protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
