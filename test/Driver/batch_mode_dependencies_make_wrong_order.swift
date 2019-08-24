// RUN: %empty-directory(%t)
// RUN: echo 'class a {}' >%t/a.swift
// RUN: echo 'class b : a {}' >%t/b.swift
// RUN: echo 'class c : b {}' >%t/c.swift
// RUN: echo 'class d : c {}' >%t/d.swift
// RUN: echo 'public func main() {}' >%t/main.swift
//
// First prime the incremental state, but note that we're building in the d c b a (reverse-alphabetical) order.
// RUN: cd %t && %swiftc_driver -enable-batch-mode -incremental -output-file-map %S/Inputs/abcd_filemap.yaml -module-name main -j 1 d.swift c.swift b.swift a.swift main.swift
//
// Now perturb the interface of a.swift and delete its output
// RUN: echo 'class a { var x : Int = 10 }' >%t/a.swift
// RUN: rm %t/a.o
//
// Now rebuild, which will rebuild a.swift then do a cascading dep-graph invalidation
// RUN: cd %t && %swiftc_driver -enable-batch-mode -incremental -output-file-map %S/Inputs/abcd_filemap.yaml -module-name main -j 1 d.swift c.swift b.swift a.swift main.swift -driver-show-incremental -driver-show-job-lifecycle >%t/out.txt 2>&1
// RUN: %FileCheck %s <%t/out.txt
//
// Check that we saw invalidation happen in alphabetic order
// CHECK: Queuing because of dependencies discovered later: {compile: b.o <= b.swift}
// CHECK: Queuing because of dependencies discovered later: {compile: c.o <= c.swift}
// CHECK: Queuing because of dependencies discovered later: {compile: d.o <= d.swift}
// CHECK: Batchable: {compile: b.o <= b.swift}
// CHECK: Batchable: {compile: c.o <= c.swift}
// CHECK: Batchable: {compile: d.o <= d.swift}
//
// But check that we still issued the job in reverse-alphabetic order
// CHECK: Adding batch job to task queue: {compile: d.o c.o b.o <= d.swift c.swift b.swift}
