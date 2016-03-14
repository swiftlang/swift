// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/jvasileff (John Vasileff)

protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
