// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library %s -O -sil-verify-all -Xllvm -sil-disable-pass=FunctionSignatureOpts -module-name=test -Xllvm -sil-print-types -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library %s -O -sil-verify-all -Xllvm -sil-disable-pass=FunctionSignatureOpts -module-name=test -emit-ir | %FileCheck %s -check-prefix=CHECK-LLVM

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -O -Xllvm -sil-disable-pass=FunctionSignatureOpts -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=x86_64
// REQUIRES: swift_in_compiler

public class C {
  var x: Int

  init(x: Int) {
    self.x = x
  }
}

public class D: C {}

// CHECK-LABEL: sil_global private @$s4test1c_WZTv_ : $C = {
// CHECK-NEXT:    %0 = integer_literal $Builtin.Int64, 27         // user: %1
// CHECK-NEXT:    %1 = struct $Int (%0 : $Builtin.Int64)          // user: %2
// CHECK-NEXT:    %initval = object $C (%1 : $Int)
// CHECK-NEXT:  }

// CHECK-LLVM-LABEL: @"$s4test1c_WZTv_" ={{.*}} global %{{[a-zA-Z_0-9]*}}c { [1 x i64] zeroinitializer, %{{[a-zA-Z_0-9]*}} <{ %swift.refcounted zeroinitializer, %TSi <{ i64 27 }> }

public let c = C(x: 27)


// CHECK-LABEL: sil [noinline] @$s4test6testitAA1CCyF : $@convention(thin) () -> @owned C {
// CHECK:         [[C:%.*]] = global_value @$s4test1c_WZTv_ : $C
// CHECK-NEXT:    return [[C]]
// CHECK:       } // end sil function '$s4test6testitAA1CCyF'

// CHECK-LLVM-LABEL: define {{.*}} @"$s4test6testitAA1CCyF"
// CHECK-LLVM:         [[C:%.*]] = tail call ptr @swift_initStaticObject({{.*}} getelementptr {{.*}}, ptr @"$s4test1c_WZTv_"
// CHECK-LLVM-NEXT:    ret ptr [[C]]
// CHECK-LLVM:       }
@inline(never)
public func testit() -> C {
  return c
}

// Cannot allocate a ManagedBuffer in a data section, because it calls malloc_size on the class instance.
// CHECK-LABEL: sil private [global_init_once_fn] @$s4test10managedBuf_WZ :
// CHECK:         alloc_ref [tail_elems $UInt8 * %{{[0-9]*}} : $Builtin.Word] $ManagedBuffer<(), UInt8>
// CHECK:       } // end sil function '$s4test10managedBuf_WZ'
public let managedBuf = ManagedBuffer<Void, UInt8>.create(minimumCapacity: 0, makingHeaderWith: { _ in })

@main struct Main {
  static func main() {
    // CHECK-OUTPUT: c.x=27
    print("c.x=\(testit().x)")
    // CHECK-OUTPUT: c=test.C
    print("c=\(testit())")
  }
}

