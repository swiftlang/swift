import Operators

extension Vector {
  // bool Vector::operator==(const Vector &other) const;
  @unsafe @cxx(`operator==`) @implementation
  public func equals(_ other: borrowing Vector) -> Bool { return x == other.x }

  // bool Vector::operator<(const Vector &other) const;
  @unsafe @cxx(`operator<`) @implementation
  public func less(_ other: borrowing Vector) -> Bool { return x < other.x }

  // Vector Vector::operator+(const Vector &other) const;
  @unsafe @cxx(`operator+`) @implementation
  public func plus(_ other: borrowing Vector) -> Vector { return Vector(x: x + other.x) }

  // Vector Vector::operator+(int k) const;
  @cxx(`operator+`) @implementation
  public func plus(_ k: Int32) -> Vector { return Vector(x: x + k) }

  // Vector Vector::operator-() const;
  @cxx(`operator-`) @implementation
  public func negated() -> Vector { return Vector(x: -x) }

  // Vector Vector::operator-(const Vector &other) const;
  @unsafe @cxx(`operator-`) @implementation
  public func minus(_ other: borrowing Vector) -> Vector { return Vector(x: x - other.x) }

  // Vector &Vector::operator+=(const Vector &other);
  @unsafe @cxx(`operator+=`) @implementation
  public mutating func plusEquals(_ other: borrowing Vector) -> UnsafeMutablePointer<Vector> {
    x += other.x
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // int Vector::operator[](int i) const;
  @cxx(`operator[]`) @implementation
  public func element(_ i: Int32) -> Int32 { return x + i }

  // int Vector::operator()(int i) const;
  @cxx(`operator()`) @implementation
  public func call(_ i: Int32) -> Int32 { return x * i }

  // Vector &Vector::operator++();
  @unsafe @cxx(`operator++`) @implementation
  public mutating func increment() -> UnsafeMutablePointer<Vector> {
    x += 1
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // Vector Vector::operator++(int);
  @cxx(`operator++`) @implementation
  public mutating func postIncrement(_: Int32) -> Vector {
    let old = self
    x += 1
    return old
  }
}

// bool operator!=(const Vector &a, const Vector &b);
@unsafe @cxx @implementation
public func != (a: borrowing Vector, b: borrowing Vector) -> Bool { return a.x != b.x }

// Vector operator*(const Vector &a, int k);
@unsafe @cxx(`operator*`) @implementation
public func times(_ a: borrowing Vector, _ k: Int32) -> Vector { return Vector(x: a.x * k) }

// bool Outer::operator==(const Point &a, const Point &b);
@unsafe @cxx @implementation
public func == (a: borrowing Outer.Point, b: borrowing Outer.Point) -> Bool { return a.v == b.v }

extension Handle {
  // bool Handle::operator==(const Handle &other) const;
  @cxx(`operator==`) @implementation
  public func equals(_ other: borrowing Handle) -> Bool { return value == other.value }
}

// bool operator<(const Handle &a, const Handle &b);
@cxx @implementation
public func < (a: borrowing Handle, b: borrowing Handle) -> Bool { return a.value < b.value }

// int swiftCallsOperators(const Vector &a, const Vector &b);
@unsafe @cxx @implementation
public func swiftCallsOperators(_ a: borrowing Vector, _ b: borrowing Vector) -> Int32 {
  var v = copy a
  let w = copy b
  var n: Int32 = 0
  if v == w { n += 1 }
  if v != w { n += 10 }
  if v < w { n += 100 }
  v += w
  n += 1000 * v.x
  n += 10000 * (v + w).x
  n += 100000 * (-v).x
  return n
}
