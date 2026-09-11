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

// int overloadedByArityAndType(int x);
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32) -> Int32 { return x + 1 }

// double overloadedByArityAndType(double x);
@cxx @implementation
public func overloadedByArityAndType(_ x: Double) -> Double { return x * 2 }

// int overloadedByArityAndType(int x, int y);
@cxx @implementation
public func overloadedByArityAndType(_ x: Int32, _ y: Int32) -> Int32 { return x + y }

// int renamedOverload(int x);
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadInt(_ x: Int32) -> Int32 { return x + 1 }

// double renamedOverload(double x);
@cxx(renamedOverload) @implementation
public func swiftRenamedOverloadDouble(_ x: Double) -> Double { return x * 2 }
