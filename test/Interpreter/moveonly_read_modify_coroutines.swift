// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

struct NC: ~Copyable {
  deinit { print("destroy") }
}

class C {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
    _modify { yield &_data }
  }
}

func borrow(_ nc: borrowing NC) {}
func mod(_ nc: inout NC) {}

defer {
    // CHECK: starting
    print("starting")
    do {
        // CHECK-NEXT: destroy
        test(C())
    }
    // CHECK: done
    print("done")
}
func test(_ c: C) {
  borrow(c.data)
  borrow(c.data)

  mod(&c.data)
  mod(&c.data)
}
