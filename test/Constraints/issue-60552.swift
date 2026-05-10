// RUN: %target-swift-emit-silgen %s

// rdar://150858005
protocol P {}
protocol Q {}

struct MyP: P, Q {}

extension P where Self == MyP {
    static var myP: Self { return MyP() }
}

func test() {
    let _: any P & Q = .myP
}

// rdar://148708774
protocol Wrapper<Wrapped> {
    associatedtype Wrapped
}
struct IntWrapper: Wrapper {
    typealias Wrapped = Int
}

extension Wrapper where Self == IntWrapper {
    static var int: Self { fatalError() }
}

let crashes: any Wrapper<Int> = .int
let ok1: some Wrapper<Int> = .int
let ok2: any Wrapper = .int
