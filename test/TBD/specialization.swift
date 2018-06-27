// Validate the the specializations actually exist (if they don't then we're not
// validating that they end up with the correct linkages):
// RUN: %target-swift-frontend -emit-sil -o- -O -validate-tbd-against-ir=none %s | %FileCheck %s

// RUN: %target-swift-frontend -emit-ir -o/dev/null -O -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -O -validate-tbd-against-ir=all -enable-resilience %s

// With -enable-testing:
// RUN: %target-swift-frontend -emit-ir -o/dev/null -O -validate-tbd-against-ir=all -enable-testing %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -O -validate-tbd-against-ir=all -enable-resilience -enable-testing %s

// rdar://problem/40738913

open class Foo {
    @inline(never)
    fileprivate func foo<T>(_: T.Type) {}
}

open class Bar<T> {
    public init() {
        bar()
    }

    @inline(never)
    fileprivate func bar() {}
}


public func f() {
    Foo().foo(Int.self)
    Bar<Int>().bar()
}


// Generic specialization, from the foo call in f
// CHECK-LABEL: // specialized Foo.foo<A>(_:)
// CHECK-NEXT: sil private [noinline] @$S14specialization3FooC3foo33_A6E3E43DB6679655BDF5A878ABC489A0LLyyxmlFSi_Tg5Tf4dd_n : $@convention(thin) () -> ()

// Function signature specialization, from the bar call in Bar.init
// CHECK-LABEL: // specialized Bar.bar()
// CHECK-NEXT: sil private [noinline] @$S14specialization3BarC3bar33_A6E3E43DB6679655BDF5A878ABC489A0LLyyFTf4d_n : $@convention(thin) () -> () {

// Generic specialization, from the bar call in f
// CHECK-LABEL: // specialized Bar.bar()
// CHECK-NEXT: sil private [noinline] @$S14specialization3BarC3bar33_A6E3E43DB6679655BDF5A878ABC489A0LLyyFSi_Tg5Tf4d_n : $@convention(thin) () -> ()
