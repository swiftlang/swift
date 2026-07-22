// RUN: %target-swift-frontend -emit-sil -verify %s -o /dev/null

struct S<T> {
  let t: T // expected-note {{'self.t.1' not initialized}}
}

extension S where T == (Int, String) {
  init(x: ()) {
    t.0 = 1
    t.1 = "hi"
  }

  init(y: ()) {// expected-error {{return from initializer without initializing all stored properties}} {{15:3-3=self.t = t\n}} {{13-13=, t: T}}
    t.0 = 1
  }
}
