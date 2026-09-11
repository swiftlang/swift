import Virtual

extension Shape {
  // virtual int Shape::area() const;
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

extension Engine {
  // virtual int Engine::status() const;
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  // virtual void Engine::boost(int amount);
  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }
}
