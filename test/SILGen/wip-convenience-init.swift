struct Designated {}
struct RequiredDesignated {}
struct Convenience {}
struct DoubleConvenience {}
struct RequiredConvenience {}
struct RequiredDoubleConvenience {}

class C {
  init(_: Designated) {}
  required init(_: RequiredDesignated) {}

  convenience init(_: Convenience) {
    self.init(Designated())
  }

  convenience init(_: DoubleConvenience) {
    self.init(Convenience())
  }

  required convenience init(_: RequiredConvenience) {
    self.init(RequiredDesignated())
  }
  required convenience init(_: RequiredDoubleConvenience) {
    self.init(RequiredConvenience())
  }
}

class D: C {
  override init(_: Designated) { super.init(Designated()) }
  required init(_: RequiredDesignated) { super.init(RequiredDesignated()) }
}

func foo(ct: C.Type, dt: D.Type) {
  _ = C(Convenience())
  _ = C(RequiredConvenience())
  _ = C(RequiredDoubleConvenience())
  _ = D(Convenience())
  _ = D(RequiredConvenience())
  _ = D(RequiredDoubleConvenience())
  _ = ct.init(RequiredConvenience())
  _ = dt.init(RequiredConvenience())
  _ = ct.init(RequiredDoubleConvenience())
  _ = dt.init(RequiredDoubleConvenience())
}
