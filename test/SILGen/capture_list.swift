// RUN: %target-swift-emit-silgen-ossa %s

// Capture list with weak capture vs noescape closure
func transform<T>(fn: () -> T) -> T {
  return fn()
}

// Make sure weak and unowned are captured by box, even from a noescape closure.

class Bar {
  var x: Int = 27

  func test() {
    transform { [weak self] in
      return self!.x
    }
  }
}


// Capture list vs autoclosure.

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
