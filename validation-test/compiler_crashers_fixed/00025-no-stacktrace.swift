// RUN: %target-swift-frontend %s -emit-ir

// Issue found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/17501507
// https://twitter.com/rob_rix/status/483456023773315073

protocol A {
    associatedtype B
    func b(_: B)
}
struct X<Y> : A {
    func b(b: X.Type) {
    }
}
