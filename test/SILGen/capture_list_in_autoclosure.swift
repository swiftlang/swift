// RUN: %target-swift-frontend -emit-silgen -verify %s

func block(_ f: () -> Void) -> Int { return 42 }
func oneOf(_ a: Int?, _ b: @autoclosure () -> Int) -> Int { return 0 }
class Foo {
    private var value: Int?
    func refresh() {
        _ = oneOf(self.value, block({
            [unowned self] in _ = self
        }))
    }
}
