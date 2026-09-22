import Overloads

// int overloadedByType(int x);
@cxx @implementation
public func overloadedByType(_ x: Int32) -> Int32 { return x + 1 }

// double overloadedByType(double x);
@cxx @implementation
public func overloadedByType(_ x: Double) -> Double { return x * 2 }

// int overloadedByType(int *p);
@cxx @implementation
public func overloadedByType(_ p: UnsafeMutablePointer<Int32>?) -> Int32 {
  p!.pointee += 1
  return p!.pointee
}

// int overloadedByType(Point p);
@cxx @implementation
public func overloadedByType(_ p: Point) -> Int32 { return p.x + p.y }

// int overloadedByArityAndType(int x);
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32) -> Int32 { return x + 1 }

// double overloadedByArityAndType(double x);
@cxx @implementation
public func overloadedByArityAndType(_ x: Double) -> Double { return x * 2 }

// int overloadedByArityAndType(int x, int y);
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32, _ y: Int32) -> Int32 { return x + y }

// int overloadedByArityAndType(Point p, int z);
@cxx @implementation
public func overloadedByArityAndType(_ p: Point, _ z: Int32) -> Int32 { return (p.x + p.y) * z }

// int overloadedByEnumType(int x);
@cxx @implementation
public func overloadedByEnumType(_ x: Int32) -> Int32 { return x }

// int overloadedByEnumType(unsigned x);
@cxx @implementation
public func overloadedByEnumType(_ x: UInt32) -> Int32 { return Int32(x) + 10 }

// int overloadedByEnumType(EnumFoo x);
@cxx @implementation
public func overloadedByEnumType(_ x: EnumFoo) -> Int32 { return Int32(x.rawValue) + 20 }

// int overloadedByEnumType(EnumBar x);
@cxx @implementation
public func overloadedByEnumType(_ x: EnumBar) -> Int32 { return x.rawValue + 30 }

// int renamedOverload(int x);
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadInt(_ x: Int32) -> Int32 { return x + 1 }

// double renamedOverload(double x);
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadDouble(_ x: Double) -> Double { return x * 2 }
