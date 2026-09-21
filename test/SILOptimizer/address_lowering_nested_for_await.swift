// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -emit-sil -Onone %s | %FileCheck %s

// REQUIRES: concurrency

// Regression test: Make sure that `dealloc_stack` are placed when required
// when a block ultimately exits via `unreachable`.

public func makeStream() -> AsyncStream<String> {
  AsyncStream { c in
    c.yield("a")
    c.finish()
  }
}

// CHECK-LABEL: sil @${{.*}}testNestedForAwait{{.*}} :
// Outer-loop iterator storage is allocated up-front in the entry block.
// CHECK:       bb0:
// CHECK:         [[OUTER_ITER:%[^,]+]] = alloc_stack [var_decl] $IndexingIterator<Range<Int>>
// CHECK:         br [[OUTER_HEADER:bb[0-9]+]]
//
// CHECK:       [[OUTER_HEADER]]:
// CHECK:         switch_enum {{.*}}, case #Optional.some!enumelt: [[OUTER_BODY:bb[0-9]+]], case #Optional.none!enumelt: [[OUTER_EXIT:bb[0-9]+]]
//
// Outer-loop body allocates the per-outer-iteration AsyncStream iterator
// storage. The `[lexical] [var_decl]` alloc is the `$x$generator`.
// CHECK:       [[OUTER_BODY]]({{.*}}):
// CHECK:         [[STREAM:%[^,]+]] = alloc_stack $AsyncStream<String>
// CHECK:         [[ITER:%[^,]+]] = alloc_stack $AsyncStream<String>.Iterator
// CHECK:         [[GEN:%[^,]+]] = alloc_stack [lexical] [var_decl] $AsyncStream<String>.Iterator
// CHECK:         br [[INNER_HEADER:bb[0-9]+]]
//
// CHECK:       [[INNER_HEADER]]:
// CHECK:         switch_enum {{.*}}, case #Optional.some!enumelt: {{bb[0-9]+}}, case #Optional.none!enumelt: [[OUTER_BACK:bb[0-9]+]]
//
// Outer-loop back-edge: deallocates ALL the outer-body allocs, then loops
// back to the outer header.
// CHECK:       [[OUTER_BACK]]:
// CHECK:         destroy_addr [[GEN]]
// CHECK:         dealloc_stack [[GEN]]
// CHECK:         dealloc_stack [[ITER]]
// CHECK:         dealloc_stack [[STREAM]]
// CHECK-NEXT:    br [[OUTER_HEADER]]
//
// Outer-loop exit: only the outer iterator is deallocated.
// CHECK:       [[OUTER_EXIT]]:
// CHECK:         dealloc_stack [[OUTER_ITER]]
// CHECK-NEXT:    tuple
// CHECK-NEXT:    return
// CHECK-LABEL: } // end sil function '${{.*}}testNestedForAwait{{.*}}'
public func testNestedForAwait() async {
  for _ in 0..<2 {
    for await x in makeStream() {
      _ = x
    }
  }
}
