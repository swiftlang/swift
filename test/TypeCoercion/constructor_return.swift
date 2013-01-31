// RUN: %swift %s -parse

func foo() {}
func bar() {}

struct S {
  constructor(b:Bool) {
    foo()
    if b { return }
    bar()
  }
}

class C {
  var b:Bool
  constructor(b:Bool) {
    this.b = b
    foo()
    if b { return }
    bar()
  }

  destructor {
    foo()
    if b { return }
    bar()
  }
}

