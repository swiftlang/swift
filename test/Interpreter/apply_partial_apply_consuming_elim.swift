// RUN: %target-run-simple-swift(-Osize)

// REQUIRES: executable_test

@inline(never)
func consumeSelf<T>(_ t : __owned T) {
  print("Consuming self!")
  print(t)
}

class Klass {}
struct S<T> {
  let t: T? = (Klass() as! T)

  @inline(__always)
  __consuming func foo(_ t: T) {
    consumeSelf(self)
  }
}

public func test<T>(_ t: __owned T) {
  let k = S<T>()
  let f = k.foo

  for _ in 0..<1024 {
    f(t)
  }
}

test(Klass())
