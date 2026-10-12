import OperatorsCxx23

extension Grid {
  // int Grid::operator[]() const;
  @cxx(`operator[]`) @implementation
  public func first() -> Int32 { return width }

  // int Grid::operator[](int i) const;
  @cxx(`operator[]`) @implementation
  public func at(_ i: Int32) -> Int32 { return i }

  // int Grid::operator[](int row, int col) const;
  @cxx(`operator[]`) @implementation
  public func at(_ row: Int32, _ col: Int32) -> Int32 { return row * width + col }

  // double Grid::operator[](double row, double col) const;
  @cxx(`operator[]`) @implementation
  public func at(_ row: Double, _ col: Double) -> Double {
    return row * Double(width) + col
  }

  // int &Grid::operator[](int row, int col, int layer);
  @cxx(`operator[]`) @implementation
  public mutating func at(_ row: Int32, _ col: Int32, _ layer: Int32) -> UnsafeMutablePointer<Int32> {
    return withUnsafeMutablePointer(to: &width) { $0 }
  }
}

extension StaticGrid {
  // static int StaticGrid::operator[](int i);
  @cxx(`operator[]`) @implementation
  public static func at(_ i: Int32) -> Int32 { return i * 2 }

  // static int StaticGrid::operator[](int row, int col);
  @cxx(`operator[]`) @implementation
  public static func at(_ row: Int32, _ col: Int32) -> Int32 { return row * 10 + col }
}

// int swiftCallsSubscripts(Grid &g);
@unsafe @cxx @implementation
public func swiftCallsSubscripts(_ g: inout Grid) -> Int32 {
  let s = StaticGrid()
  g[0, 0, 0] = 3
  return g[] + 10 * g[1] + 100 * g[1, 2] + 1000 * Int32(g[1.5, 2.5]) +
    10000 * s[4] + 100000 * s[0, 1]
}
