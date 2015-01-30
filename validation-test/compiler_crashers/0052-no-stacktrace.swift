// RUN: not %target-swift-frontend %s

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18332184
// https://twitter.com/rob_rix/status/510875744667701248

if true {
    struct S {
        let v: Int
    }
    let g = {
        S(v: $0)
    }
    let f = {
        0
    }
    func compose<T, U, V>(g: U -> V, f: T -> U)(x: T) -> V {
        return g(f(x))
    }
    let h = compose(g, f)
}
