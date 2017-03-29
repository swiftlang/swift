// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// Check that compiler does not crash on the devirtualization of materializeForSet methods
// and produces a correct code.

// CHECK-LABEL: sil [transparent] [serialized] [thunk] @_T024devirt_materializeForSet7BaseFooCAA0F0A2aDP3barSSfmTW
// CHECK: checked_cast_br [exact] %{{.*}} : $BaseFoo to $ChildFoo
// CHECK: thin_function_to_pointer %{{.*}} : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout ChildFoo, @thick ChildFoo.Type) -> () to $Builtin.RawPointer
// CHECK: enum $Optional<Builtin.RawPointer>, #Optional.some!enumelt.1, %{{.*}} : $Builtin.RawPointer
// CHECK: tuple (%{{.*}} : $Builtin.RawPointer, %{{.*}} : $Optional<Builtin.RawPointer>)

public protocol Foo {
    var bar: String { get set }
}

open class BaseFoo: Foo {
    open var bar: String = "hello"
}

open class ChildFoo: BaseFoo {
    private var _bar: String = "world"

    override open var bar: String {
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
public func test2(f: Foo) {
  var f = f
  f.bar = "test2"
  print(f.bar)
}


//test1(BaseFoo())
//test1(ChildFoo())

//test2(BaseFoo())
//test2(ChildFoo())
