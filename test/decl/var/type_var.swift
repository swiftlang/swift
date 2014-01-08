// RUN: %swift -parse %s -verify

struct S {
  type var x: Int

  var y = 1

  type var z = 5
}

class C {
  type var x: Int // expected-error{{static variables not yet supported in classes}}
}

enum E {
  type var y: Int { }
}

extension S {
  type var zz: Int { return 5 } // expected-error{{static variables not yet supported in this context}}
}
