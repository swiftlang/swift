import ForeignReferenceVirtual

// `super` may call Base::describe() before it is defined.
extension Derived {
  // int Derived::describe() const override; the key function.
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() * 2 }

  // int Derived::hide() const; hides Base::hide().
  @cxx @implementation
  public func hide() -> Int32 { return super.hide() + 1 }
}

extension Base {
  // virtual int Base::anchor() const; the key function.
  @cxx @implementation
  public func anchor() -> Int32 { return 0 }

#if !BASE_DESCRIBE_IN_CXX
  // virtual int Base::describe() const;
  @cxx @implementation
  public func describe() -> Int32 { return 200 + value }
#endif
}

extension Leaf {
  // int Leaf::describe() const override; the key function.
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() + 1 }

  // int Leaf::tag() const override;
  @cxx @implementation
  public func tag() -> Int32 { return super.tag() + 1000 }
}

extension MultiDerived {
  // int MultiDerived::describe() const override; the key function.
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() * 3 }

  // int MultiDerived::fromSecond() const override;
  @cxx @implementation
  public func fromSecond() -> Int32 { return value + second }
}

extension ConcreteDerived {
  // int ConcreteDerived::pure() const override; the key function.
  @cxx @implementation
  public func pure() -> Int32 { return 8 }
}
