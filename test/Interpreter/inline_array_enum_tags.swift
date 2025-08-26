// RUN: %target-run-simple-swift(-Xfrontend -disable-availability-checking) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime || use_os_stdlib

struct Foo {
    let x: UInt64
    let y: InlineArray<2, UInt64>
}

struct Bar {
    let x: UInt64
    let y: [UInt64]
}

enum Baz {
    case foo(Foo)
    case bar(Bar)
}

@inline(never)
func createEnum() -> Baz {
    return .foo(Foo(x: 0, y: [0, 0xff00000000000000]))
}


let x = createEnum()

// CHECK: 0 - 18374686479671623680
switch x {
    case .bar: fatalError("Expected .foo")
    case .foo(let x): print("\(x.y[0]) - \(x.y[1])")
}
