// RUN: %target-swift-frontend %s -enable-experimental-feature LifetimeDependence -emit-sil 

// Ensure we don't crash

struct MyBox {
  struct Value: ~Escapable {
    @usableFromInline
    @_transparent
    init(a: borrowing Int) {}
  }

  public var value: Value? {
    @_transparent
    @lifetime(borrow self)
    _read {
      let val = Value(a: 1)
      yield val
    }
  }
}

func main() {
  let myBox = MyBox()
  precondition(myBox.value != nil, "No nil")
}

