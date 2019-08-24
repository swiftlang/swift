// RUN: %target-swift-frontend %s -O -emit-sil

// Make sure we are not crashing on this one.

class X { func ping() {} }
class Y : X { override func ping() {} }

func foo(y : Y) {
  var x : X = y
  x.ping()
}

