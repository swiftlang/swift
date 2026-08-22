import Namespaces

extension Outer {
  // int Outer::add(int a, int b);
  @cxx @implementation
  public static func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }

  // int Outer::renamedTarget(int x);
  @cxx(renamedTarget) @implementation
  public static func swiftRenamed(_ x: Int32) -> Int32 { return x * 2 }

  // int Outer::overloadedByArity(int x);
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32) -> Int32 { return x + 1 }

  // int Outer::overloadedByArity(int x, int y);
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32, _ y: Int32) -> Int32 { return x + y }
}

extension Outer.Inner {
  // int Outer::Inner::nestedFunc(int x);
  @cxx @implementation
  public static func nestedFunc(_ x: Int32) -> Int32 { return -x }
}
