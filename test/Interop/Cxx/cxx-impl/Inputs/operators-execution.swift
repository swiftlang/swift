import Operators

extension Vector {
  // bool Vector::operator==(const Vector &other) const;
  @cxx(`operator==`) @implementation
  public func equals(_ other: UnsafePointer<Vector>) -> Bool { return x == other.pointee.x }

  // bool Vector::operator<(const Vector &other) const;
  @cxx(`operator<`) @implementation
  public func less(_ other: UnsafePointer<Vector>) -> Bool { return x < other.pointee.x }

  // Vector Vector::operator+(const Vector &other) const;
  @cxx(`operator+`) @implementation
  public func plus(_ other: UnsafePointer<Vector>) -> Vector { return Vector(x: x + other.pointee.x) }

  // Vector Vector::operator+(int k) const;
  @cxx(`operator+`) @implementation
  public func plus(_ k: Int32) -> Vector { return Vector(x: x + k) }

  // Vector Vector::operator-() const;
  @cxx(`operator-`) @implementation
  public func negated() -> Vector { return Vector(x: -x) }

  // Vector Vector::operator-(const Vector &other) const;
  @cxx(`operator-`) @implementation
  public func minus(_ other: UnsafePointer<Vector>) -> Vector { return Vector(x: x - other.pointee.x) }

  // Vector &Vector::operator+=(const Vector &other);
  @cxx(`operator+=`) @implementation
  public mutating func plusEquals(_ other: UnsafePointer<Vector>) -> UnsafeMutablePointer<Vector> {
    x += other.pointee.x
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // int Vector::operator[](int i) const;
  @cxx(`operator[]`) @implementation
  public func element(_ i: Int32) -> Int32 { return x + i }

  // int Vector::operator()(int i) const;
  @cxx(`operator()`) @implementation
  public func call(_ i: Int32) -> Int32 { return x * i }

  // Vector &Vector::operator++();
  @cxx(`operator++`) @implementation
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
@cxx @implementation
public func != (a: UnsafePointer<Vector>, b: UnsafePointer<Vector>) -> Bool { return a.pointee.x != b.pointee.x }

// Vector operator*(const Vector &a, int k);
@cxx(`operator*`) @implementation
public func times(_ a: UnsafePointer<Vector>, _ k: Int32) -> Vector { return Vector(x: a.pointee.x * k) }

// bool Outer::operator==(const Point &a, const Point &b);
@cxx @implementation
public func == (a: UnsafePointer<Outer.Point>, b: UnsafePointer<Outer.Point>) -> Bool { return a.pointee.v == b.pointee.v }

extension Handle {
  // bool Handle::operator==(const Handle &other) const;
  @cxx(`operator==`) @implementation
  public func equals(_ other: Handle) -> Bool { return value == other.value }
}

// bool operator<(const Handle &a, const Handle &b);
@cxx @implementation
public func < (a: Handle, b: Handle) -> Bool { return a.value < b.value }

// int swiftCallsOperators(const Vector &a, const Vector &b);
// Swift-side uses of the imported operators reach the implementations above.
@cxx @implementation
public func swiftCallsOperators(_ a: UnsafePointer<Vector>, _ b: UnsafePointer<Vector>) -> Int32 {
  var v = a.pointee
  let w = b.pointee
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
