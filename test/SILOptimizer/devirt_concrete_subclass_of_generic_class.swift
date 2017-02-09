// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil -O %s | %FileCheck %s

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

// CHECK-LABEL: sil [noinline] @_T041devirt_concrete_subclass_of_generic_class5test1s5Int32VyF
@inline(never)
public func test1() -> Int32 {
  let o = Derived()
  return o.foo() + o.boo()
}

@inline(never)
public func test2(_ o: Derived) -> Int32 {
  return o.foo() + o.boo()
}


@inline(never)
public func test3() -> Int32 {
  let o: Base<Int> = Derived()
  return o.foo() + o.boo()
}

@inline(never)
public func test4(_ o: Base<Int>) -> Int32 {
  return o.foo() + o.boo()
}

@inline(never)
public func test5<T>(_ o: Base<T>) -> Int32 {
  return o.foo() + o.boo()
}

print(test1())
print(test2(Derived()))

print(test3())
print(test4(Derived()))

print(test5(Derived()))

// Check that we handle indirect devirtualization through an intermediate
// method. rdar://problem/24993618

private class IndirectMethodCall<T> {
    func bug() {
        overrideMe()
    }
    
    @inline(never)
    func overrideMe() { }
}

private class IndirectChildConcrete: IndirectMethodCall<Int> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildTuple<U>: IndirectMethodCall<(U, U)> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildTupleConcrete: IndirectChildTuple<Int> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildMeta<U>: IndirectMethodCall<U.Type> {
    @inline(never)
    override func overrideMe() { }
}
private class IndirectChildMetaConcrete: IndirectChildMeta<Int> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildBoundGeneric<U>: IndirectMethodCall<Array<U>> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildBoundGenericConcrete:
      IndirectChildBoundGeneric<Int> {
    @inline(never)
    override func overrideMe() { }
}

private class IndirectChildFunction<U>: IndirectMethodCall<(U) -> U> {
    @inline(never)
    override func overrideMe() { }
}
private class IndirectChildFunctionConcrete: IndirectChildFunction<Int> {
    @inline(never)
    override func overrideMe() { }
}

// CHECK-LABEL: sil {{.*}} @{{.*}}test6
@inline(never)
func test6() {
  // CHECK: function_ref @{{.*}}IndirectChildConcrete{{.*}}overrideMe
  IndirectChildConcrete().bug()
  // CHECK: function_ref @{{.*}}IndirectChildTuple{{.*}}overrideMe
  IndirectChildTuple<Int>().bug()
  // CHECK: function_ref @{{.*}}IndirectChildTupleConcrete{{.*}}overrideMe
  IndirectChildTupleConcrete().bug()
  // CHECK: function_ref @{{.*}}IndirectChildMeta{{.*}}overrideMe
  IndirectChildMeta<Int>().bug()
  // CHECK: function_ref @{{.*}}IndirectChildMetaConcrete{{.*}}overrideMe
  IndirectChildMetaConcrete().bug()
  // CHECK: function_ref @{{.*}}IndirectChildBoundGeneric{{.*}}overrideMe
  IndirectChildBoundGeneric<Int>().bug()
  // CHECK: function_ref @{{.*}}IndirectChildBoundGenericConcrete{{.*}}overrideMe
  IndirectChildBoundGenericConcrete().bug()
  // CHECK: function_ref @{{.*}}IndirectChildFunction{{.*}}overrideMe
  IndirectChildFunction<Int>().bug()
  // CHECK: function_ref @{{.*}}IndirectChildFunctionConcrete{{.*}}overrideMe
  IndirectChildFunctionConcrete().bug()
}

print(test6())
