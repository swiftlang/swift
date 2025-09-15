// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -g -experimental-serialize-debug-info -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -O
// RUN: %target-swift-frontend -g -I %t %t/Main.swift -experimental-serialize-debug-info -emit-sil -o - -O -Xllvm -sil-print-types| %FileCheck %s

// Shadow copies should not be emitted for imported functions as the imported
// function might be optimized which breaks the unoptimized invariant of
// emitting shadow copies. This invocation crashes if shadow copies are not
// disabled for imported functions
// RUN: %target-swift-frontend -g -I %t %t/Main.swift -experimental-serialize-debug-info -o - -Onone -emit-ir

// BEGIN MyModule.swift
@_alwaysEmitIntoClient @inline(never)
public func bar(_ x: [Int64], sum: Int64) -> Int64 {
    var temp = sum
    for i in x {
        temp += i
    }
    return temp
}

// Make sure the generic signature of foo is always serialized. Otherwise, the
// sil scope in fooCaller refers to the generic version of foo
@inlinable @inline(__always)
func foo<T: AdditiveArithmetic>(_ x: T, _ y : T) -> T {
    return x + y
}

@inline(never) @_alwaysEmitIntoClient
public func fooCaller<T: AdditiveArithmetic>(_ x: T, _ y : T) -> T {
    return foo(x, y)
}

// BEGIN Main.swift
import MyModule
// sil_scope should refer to the specialized version of foo
//CHECK: sil_scope {{.*}} { loc "{{.*}}MyModule.swift":13:6 parent @$s8MyModule3fooyxx_xts18AdditiveArithmeticRzlFSi_TG5 {{.*}} inlined_at {{.*}} }
let _ = fooCaller(1, 2)

func test() {
    let _ = bar([10], sum: 0)
}
// CHECK: sil {{.*}} @$s8MyModule3bar_3sums5Int64VSayAEG_AEtF : $@convention(thin) (@guaranteed Array<Int64>, Int64) -> Int64 {

// CHECK: debug_value %0 : $Array<Int64>, let, name "x", argno 1, loc "{{.*}}MyModule.swift":2:19
// CHECK: debug_value %1 : $Int64, let, name "sum", argno 2, loc "{{.*}}MyModule.swift":2:31
// CHECK: debug_value {{.*}}, var, (name "$i$generator", loc "{{.*}}MyModule.swift":4:14), type $IndexingIterator<Array<Int64>>, expr op_fragment:#IndexingIterator._position:op_fragment:#Int._value, loc "{{.*}}MyModule.swift":4:14
// CHECK: debug_value {{.*}}, var, (name "$i$generator", loc "{{.*}}MyModule.swift":4:14), type $IndexingIterator<Array<Int64>>, expr op_fragment:#IndexingIterator._position:op_fragment:#Int._value
// CHECK: debug_value {{.*}} : $Builtin.Int64, var, (name "temp", loc "{{.*}}MyModule.swift":3:9, scope {{.*}}), type $Int64, expr op_fragment:#Int64._value, loc "{{.*}}MyModule.swift":5:14, scope

test()
// CHECK-NOT: UnknownCode
