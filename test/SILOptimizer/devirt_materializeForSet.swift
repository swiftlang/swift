// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// Check that compiler does not crash on the devirtualization of materializeForSet methods
// and produces a correct code.
//
// Note: now we no longer speculatively devirtualize inside thunks, so this test does nothing.

// CHECK-LABEL: sil shared [transparent] [thunk] @_T024devirt_materializeForSet7BaseFooCAA0F0A2aDP3barSSvmTW

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
