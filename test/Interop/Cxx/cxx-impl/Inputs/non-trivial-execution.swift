import NonTrivial

// int takesTracked(Tracked t);
@cxx @implementation
public func takesTracked(_ t: Tracked) -> Int32 { return t.value }

// int takesTwoTracked(Tracked a, Tracked b);
@cxx @implementation
public func takesTwoTracked(_ a: Tracked, _ b: Tracked) -> Int32 { return a.value + b.value }

// int copiesTracked(Tracked t);
// Mutates a copy; the caller's argument is unchanged.
@cxx @implementation
public func copiesTracked(_ t: Tracked) -> Int32 {
  var copy = t
  copy.value += 100
  return copy.value
}

// Tracked returnsTracked(int v);
@cxx @implementation
public func returnsTracked(_ v: Int32) -> Tracked { return Tracked(v) }

// Tracked passesThroughTracked(Tracked t);
@cxx @implementation
public func passesThroughTracked(_ t: Tracked) -> Tracked { return t }

// int takesMovable(Movable m);
@cxx @implementation
public func takesMovable(_ m: Movable) -> Int32 { return m.value }

// Movable returnsMovable(int v);
@cxx @implementation
public func returnsMovable(_ v: Int32) -> Movable { return Movable(v) }

// MoveOnly returnsMoveOnly(int v);
@cxx @implementation
public func returnsMoveOnly(_ v: Int32) -> MoveOnly { return MoveOnly(v) }

// int takesPolymorphic(Polymorphic p);
@cxx @implementation
public func takesPolymorphic(_ p: Polymorphic) -> Int32 { return p.value + p.tag() }

extension Box {
  // int Box::take(Tracked t) const;
  @cxx @implementation
  public func take(_ t: Tracked) -> Int32 { return base + t.value }

  // int Box::add(Tracked t);
  @cxx @implementation
  public mutating func add(_ t: Tracked) -> Int32 {
    base += t.value
    return base
  }

  // Tracked Box::produce() const;
  @cxx @implementation
  public func produce() -> Tracked { return Tracked(base) }

  // static Tracked Box::wrap(int v);
  @cxx @implementation
  public static func wrap(_ v: Int32) -> Tracked { return Tracked(v) }
}

// int readTracked(const Tracked &t);
@cxx @implementation
public func readTracked(_ t: UnsafePointer<Tracked>) -> Int32 { return t.pointee.value }

// void bumpTracked(Tracked &t);
@cxx @implementation
public func bumpTracked(_ t: UnsafeMutablePointer<Tracked>) { t.pointee.value += 1 }

// void assignTracked(Tracked &dst, const Tracked &src);
@cxx @implementation
public func assignTracked(_ dst: UnsafeMutablePointer<Tracked>, _ src: UnsafePointer<Tracked>) {
  dst.pointee = src.pointee
}
