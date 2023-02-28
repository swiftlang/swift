// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-move-only) | %FileCheck %s

@_moveOnly
struct Butt {
    var value: Int
    deinit { print("disposing \(value)") }

    mutating func merge(with: borrowing Butt) {
        value += with.value
    }

    func report() {
        print("got \(value)")
    }
}

func foo(x: consuming Butt, y: borrowing Butt) -> Butt {
    x.merge(with: y)

    return x
}

func main() {
    let x = Butt(value: 17)

    // CHECK: disposing 38
    let y = foo(x: x, y: Butt(value: 38))

    // CHECK-NEXT: got 55
    y.report()
    // CHECK-NEXT: disposing 55
}
main()
