import Functions

// int returnsInt();
@cxx @implementation
public func returnsInt() -> CInt { return 42 }

// int64_t takesInt64(int64_t x);
@cxx @implementation
public func takesInt64(_ x: Int64) -> Int64 { return x }

// int *returnsPtrToInt();
@cxx @implementation
public func returnsPtrToInt() -> UnsafeMutablePointer<CInt>? { return nil }

// void swapInts(int *p, int *q);
@cxx @implementation
public func swapInts(_ p: UnsafeMutablePointer<CInt>?, _ q: UnsafeMutablePointer<CInt>?) {
  let tmp = p!.pointee
  p!.pointee = q!.pointee
  q!.pointee = tmp
}

// TrivialStruct returnsTrivialStruct();
@cxx @implementation
public func returnsTrivialStruct() -> TrivialStruct {
  return TrivialStruct(x: 7, y: 9)
}

// int overloadedByArity(int x);
@cxx @implementation
public func overloadedByArity(_ x: CInt) -> CInt { return x + 1 }

// int overloadedByArity(int x, int y);
@cxx @implementation
public func overloadedByArity(_ x: CInt, _ y: CInt) -> CInt { return x + y }

// int withDefaultArg(int a, int b = 10);
@cxx @implementation
public func withDefaultArg(_ a: CInt, _ b: CInt) -> CInt { return a + b }

// extern "C" int externCFunc(int x);
@cxx @implementation
public func externCFunc(_ x: CInt) -> CInt { return -x }

// int foo(int x);
@cxx(foo) @implementation
public func swiftRenamedFoo(_ x: CInt) -> CInt { return x * 2 }
