// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

// Check that compiler does not crash on the devirtualization of materializeForSet methods
// and produces a correct code.

// CHECK-LABEL: sil [transparent] [thunk] @_TTWC24devirt_materializeForSet7BaseFooS_3FooS_FS1_m3barSS
// CHECK: checked_cast_br [exact] %{{.*}} : $BaseFoo to $ChildFoo
// CHECK: convert_function %{{.*}} : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout ChildFoo, @thick ChildFoo.Type) -> () to $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout BaseFoo, @thick BaseFoo.Type) -> ()
// CHECK: enum $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout BaseFoo, @thick BaseFoo.Type) -> ()>, #Optional.Some!enumelt.1, %{{.*}} : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout BaseFoo, @thick BaseFoo.Type) -> ()
// CHECK: tuple (%{{.*}} : $Builtin.RawPointer, %{{.*}} : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout BaseFoo, @thick BaseFoo.Type) -> ()>)

public protocol Foo {
    var bar: String { get set }
}

public class BaseFoo: Foo {
    public var bar: String = "hello"
}

public class ChildFoo: BaseFoo {
    private var _bar: String = "world"

    override public var bar: String {
        get {
            return _bar
        }
        set {
            _bar = newValue
        }
    }
}


@inline(never)
public func test1(bf: BaseFoo) {
  bf.bar = "test1"
  print(bf.bar)
}

@inline(never)
public func test2(var f: Foo) {
  f.bar = "test2"
  print(f.bar)
}


//test1(BaseFoo())
//test1(ChildFoo())

//test2(BaseFoo())
//test2(ChildFoo())