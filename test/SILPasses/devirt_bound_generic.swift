// RUN: %target-swift-frontend -emit-sil -O -emit-object %s

// We used to crash on this when trying to devirtualize a.doSomething(),
// because a is A<Int> and B is a subclass of A<String>, but not a 
// subclass A<Int>. And we were not filtring the results of the
// ClassHierarchyAnalysis to handle such cases properly and
// as a result, we were trying to cast A<Int> into B, which is
// impossible.
//
// rdar://21188939

public class A<T> {
  @inline(never)
  func doSomething() -> Int32 {
    return 100
  }
}


final class B: A<String> {

}

final class C<T>: A<T> {

}

final class D<T>: A<String> {

}


@inline(never)
public func testDevirt(a: A<Int>) -> Int32 {
  return a.doSomething()
}

