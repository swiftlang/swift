// RUN: %swift -parse %s -verify

struct S {
  type func f() { }

  var x = 1

  type func g() { }
}

class C {
  type func f() { }
}

enum E {
  type func f() { }
}

extension S {
  type func h() { }
}
