// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Foo)
// RUN: %target-swift-frontend -swift-version 5 -O -emit-module %s -emit-module-path %t/Foo/Foo.swiftmodule -module-name Foo -emit-module-interface-path %t/Foo/Foo.swiftinterface -enable-library-evolution

// RUN: %target-swift-frontend -swift-version 5 -compile-module-from-interface -module-name Foo -o %t/Foo/FooFromInterface.swiftmodule -O -Xllvm -sil-print-after=inline %t/Foo/Foo.swiftinterface 2>&1 | %FileCheck %s --check-prefix SKIPPING

// This test ensures that we don't run the Perf Inliner after serializing a
// module, if we're stopping optimizations after serializing. 

@inline(never)
public func _blackHole(_ x: Int) {}

@inlinable
public func inlinableFunction(_ x: Int) -> Int {
  return x + 1
}

public func caller() {
  _blackHole(inlinableFunction(20))
}

// SKIPPING-NOT: *** SIL function after {{.*}}, stage MidLevel,Function, pass {{.*}}: PerfInliner (inline)
