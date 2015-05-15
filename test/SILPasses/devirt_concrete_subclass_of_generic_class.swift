// RUN: %target-swift-frontend -emit-sil -O %s

// Check that devirtualizer can properly handle concrete non-generic subclasses 
// of generic classes.
// It should not crash on them.

public class Base<T> {
   func foo() -> Int32 {
     return 1
   }
   
   func boo() -> Int32 {
     return 11
   }
}

public class Derived: Base<Int> {
   override func foo() -> Int32 {
     return 2
   }
}

@inline(never)
public func test1() -> Int32 {
  let o = Derived()
  return o.foo() + o.boo()
}

@inline(never)
public func test2(o: Derived) -> Int32 {
  return o.foo() + o.boo()
}


@inline(never)
public func test3() -> Int32 {
  let o: Base<Int> = Derived()
  return o.foo() + o.boo()
}

@inline(never)
public func test4(o: Base<Int>) -> Int32 {
  return o.foo() + o.boo()
}

@inline(never)
public func test5<T>(o: Base<T>) -> Int32 {
  return o.foo() + o.boo()
}

print(test1())
print(test2(Derived()))

print(test3())
print(test4(Derived()))

print(test5(Derived()))
