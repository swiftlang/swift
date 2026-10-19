import Virtual

extension Shape {
  // virtual int Shape::area() const; the key function.
  @cxx @implementation
  public func area() -> Int32 { return sides * sides }

  // virtual void Shape::scale(int factor);
  @cxx @implementation
  public mutating func scale(_ factor: Int32) { sides *= factor }
}

extension SimpleBase {
  // virtual int SimpleBase::simple() const;
  @cxx @implementation
  public func simple() -> Int32 { return stored }
}

extension SimpleDerived {
  // int SimpleDerived::simple() const override;
  @cxx @implementation
  public func simple() -> Int32 { return stored * 2 }
}

extension Abstract {
  // virtual int Abstract::anchor() const; the key function.
  @cxx @implementation
  public func anchor() -> Int32 { return 7 }
}

extension CloneDerived {
  // RetC *CloneDerived::clone() override; a covariant return type.
  @cxx @implementation
  public mutating func clone() -> UnsafeMutablePointer<RetC> {
    return sharedRetC()
  }
}

extension MIDerived {
  // virtual void MIDerived::miAnchor(); the key function.
  @cxx @implementation
  public mutating func miAnchor() {}

  // void MIDerived::firstA() override; overrides the primary base.
  @cxx @implementation
  public mutating func firstA() { a += 100 }

  // int MIDerived::fromB() const override; overrides the non-primary base.
  @cxx @implementation
  public func fromB() -> Int32 { return a + b }
}

extension VDerived {
  // virtual void VDerived::vAnchor(); the key function.
  @cxx @implementation
  public mutating func vAnchor() {}

  // int VDerived::vbMethod() const override; overrides the virtual base.
  @cxx @implementation
  public func vbMethod() -> Int32 { return vd }
}

extension Engine {
  // virtual int Engine::status() const; the key function.
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  // virtual void Engine::boost(int amount);
  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }
}

extension AbstractEngine {
  // virtual int AbstractEngine::aeAnchor() const; the key function.
  @cxx @implementation
  public func aeAnchor() -> Int32 { return 11 }
}

// Swift implements the `int` overloads on top of the `double` ones, which stay
// in C++.

extension Mixer {
  // virtual int Mixer::mix(int amount) const;
  @cxx @implementation
  public func mix(_ amount: Int32) -> Int32 { return mix(Double(amount)) * 2 }
}

extension Gauge {
  // virtual int Gauge::read(int scale) const;
  @cxx @implementation
  public func read(_ scale: Int32) -> Int32 {
    return read(Double(scale)) + 1000
  }
}
