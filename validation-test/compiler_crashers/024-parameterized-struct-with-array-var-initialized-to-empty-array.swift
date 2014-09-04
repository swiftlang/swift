// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17662010
// https://twitter.com/rob_rix/status/488692270908973058

struct A<T> {
    let a: [(T, () -> ())] = []
}
