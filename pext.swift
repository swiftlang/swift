protocol P {
}

protocol Q : P {
  static var x: Int { get set }
}

extension P {
  static var x: Int { get { return 0 } set { } }
}

func foo(inout x: Int) {}

struct S : Q {}

func f() {
  foo(&S.x)
}
