import Methods

extension Counter {
  // static Counter Counter::make(int v);
  @cxx @implementation
  public static func make(_ v: Int32) -> Counter { return Counter(value: v) }

  // int Counter::get() const;
  @cxx @implementation
  public func get() -> Int32 { return value }

  // void Counter::add(int d);
  @cxx @implementation
  public mutating func add(_ d: Int32) { value += d }

  // int Counter::overloadedByArity() const;
  @cxx @implementation
  public func overloadedByArity() -> Int32 { return value }

  // int Counter::overloadedByArity(int x) const;
  @cxx @implementation
  public func overloadedByArity(_ x: Int32) -> Int32 { return value + x }

  // int Counter::renamedTarget() const;
  @cxx(renamedTarget) @implementation
  public func swiftRenamed() -> Int32 { return value * 2 }
}

extension Pair {
  // int Pair::adjust(int x) const;
  @cxx @implementation
  public func adjust(_ x: Int32) -> Int32 { return value + x }

  // int Pair::adjust(int x);
  @cxx(adjust) @implementation
  public mutating func adjustMutating(_ x: Int32) -> Int32 { value += x; return value }

  // int Pair::adjust(int x, int y);
  @cxx @implementation
  public mutating func adjust(_ x: Int32, _ y: Int32) -> Int32 { value += x + y; return value }
}

extension Holder {
  // Triple Holder::spread(int k) const;
  @cxx @implementation
  public func spread(_ k: Int32) -> Triple {
    return Triple(a: CLong(value), b: CLong(k), c: CLong(value + k))
  }

  // static Triple Holder::makeTriple(long a);
  @cxx @implementation
  public static func makeTriple(_ a: CLong) -> Triple { return Triple(a: a, b: a + 1, c: a + 2) }
}

extension NonTrivialReceiver {
  // int NonTrivialReceiver::read() const;
  @cxx @implementation
  public func read() -> Int32 { return value }

  // void NonTrivialReceiver::write(int v);
  @cxx @implementation
  public mutating func write(_ v: Int32) { value = v }
}
