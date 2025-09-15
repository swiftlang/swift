// RUN: %target-swift-frontend %s -enable-experimental-feature Lifetimes -emit-sil 

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

// Ensure we don't crash

struct Wrapper : ~Copyable {
}

struct NE : ~Escapable {
  @_lifetime(borrow w)
  init(_ w: borrowing Wrapper) {}
}

struct MyBox : ~Copyable {
  public var value: NE {
    _read { 
      let w = Wrapper()
      yield NE(w)
    }
  }
}

func use(_ n: borrowing NE) {}

func foo() {
  let box = MyBox()
  let value = box.value
  use(value)
}

