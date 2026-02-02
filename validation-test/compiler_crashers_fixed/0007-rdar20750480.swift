// RUN: not %target-swift-frontend %s -typecheck

protocol X {
    typealias R
}

extension Array : X {
}

let xs = [1,2,3]

func z<A: X>(A) {
}

let ys = z(xs)
