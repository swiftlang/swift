// RUN: not --crash %target-swift-frontend %s -emit-ir

class Foo {}

extension Set where Element: Foo {
    private func function1() {
        for foo: Foo in self {
            print(foo)
        }
    }
}
