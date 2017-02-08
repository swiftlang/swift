// RUN: %target-typecheck-verify-swift

func foo() {}
func bar() {}

struct S {
  init(b:Bool) {
    foo()
    if b { return }
    bar()
  }
}

class C {
  var b:Bool
  init(b:Bool) {
    self.b = b
    foo()
    if b { return }
    bar()
  }

  deinit {
    foo()
    if b { return }
    bar()
  }
}

