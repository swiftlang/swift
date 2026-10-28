import Namespaces

extension Outer {
  // int Outer::add(int a, int b);
  @cxx @implementation
  public static func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }

  // void Outer::voidNoArgs();
  @cxx @implementation
  public static func voidNoArgs() { Outer.voidNoArgsFlag = 42 }

  // int Outer::renamedTarget(int x);
  @cxx(renamedTarget) @implementation
  public static func swiftRenamed(_ x: Int32) -> Int32 { return x * 2 }

  // int Outer::overloadedByArity(int x);
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32) -> Int32 { return x + 1 }

  // int Outer::overloadedByArity(int x, int y);
  @cxx @implementation
  public static func overloadedByArity(_ x: Int32, _ y: Int32) -> Int32 { return x + y }

  // Pure Swift. It keeps the `@thin Outer.Type` self parameter.
  static func combine(_ a: Int32, _ b: Int32) -> Int32 { return a * 10 + b }

  // int Outer::callsSwiftHelper(int x);
  @cxx @implementation
  public static func callsSwiftHelper(_ x: Int32) -> Int32 { return combine(x, 7) }
}

extension Outer.Inner {
  // int Outer::Inner::nestedFunc(int x);
  @cxx @implementation
  public static func nestedFunc(_ x: Int32) -> Int32 { return -x }

  // Pure Swift. It keeps the `@thin Outer.Inner.Type` self parameter.
  static func negate(_ x: Int32) -> Int32 { return -x }

  // int Outer::Inner::nestedCallsSwiftHelper(int x);
  @cxx @implementation
  public static func nestedCallsSwiftHelper(_ x: Int32) -> Int32 { return negate(x) }
}
