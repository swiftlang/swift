// RUN: %target-swift-frontend -O -parse-stdlib -parse-as-library -emit-sil %s | FileCheck %s

// Make sure specialization of stdlib_binary_only functions are not marked
// shared. Marking them shared would make their visibility hidden. Because
// stdlib_binary_only implies public external linkage in other modules we would
// get linking errors.

@asmname("unknown")
public func unknown() -> ()

@inline(never)
@_semantics("stdlib_binary_only")
public func doSomething3<T>(a:T) {
    unknown()
}

struct A {}
@inline(never)
public func callDoSomething3() {
    doSomething3(A())
}

// CHECK-NOT: sil {{.*}}shared{{.*}} {{.*}}12doSomething3
