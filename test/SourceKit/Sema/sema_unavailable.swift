// RUN: %sourcekitd-test -req=sema %s -- %s| %FileCheck %s
find([1,2,3], 1)

// CHECK-NOT: source.lang.swift.ref.function.free
