// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/out)
// RUN: split-file %s %t

// An edited function body is re-checked on its own. Its signature spans several
// lines, and it contains an inactive '#if' region whose code must not be type checked.

// RUN: %sourcekitd-test \
// RUN:     -shell -- echo "# start" == \
// RUN:     -shell -- cp %t/t1.1.swift %t/t1.swift == \
// RUN:     -req=compile -name c1 -- -c %t/t1.swift -module-name TestModule -o %t/out/test.o == \
// RUN:     -shell -- cp %t/t1.2.swift %t/t1.swift == \
// RUN:     -shell -- echo "# modified" == \
// RUN:     -req=compile -name c1 -- -c %t/t1.swift -module-name TestModule -o %t/out/test.o == \
// RUN:     -shell -- echo "# close" == \
// RUN:     -req=compile.close -name c1 \
// RUN: | %FileCheck %s

// CHECK-LABEL: # start
// CHECK-NEXT: {
// CHECK-NEXT:   key.diagnostics: [
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.value: 0
// CHECK-NEXT: }

// CHECK-LABEL: # modified
// CHECK-NEXT: {
// CHECK-NEXT:   key.diagnostics: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.line: 8,
// CHECK-NEXT:       key.column: 10,
// CHECK-NEXT:       key.filepath: "{{.*[/\\]}}t1.swift",
// CHECK-NEXT:       key.severity: source.diagnostic.severity.error,
// CHECK-NEXT:       key.id: "cannot_find_in_scope",
// CHECK-NEXT:       key.description: "cannot find 'unknownName' in scope",
// CHECK-NEXT:       key.ranges: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.offset: {{[0-9]+}},
// CHECK-NEXT:           key.length: 11
// CHECK-NEXT:         }
// CHECK-NEXT:       ]
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.value: 1
// CHECK-NEXT: }

// CHECK-LABEL: # close
// CHECK-NEXT: {
// CHECK-NEXT: }

//--- t1.1.swift
public func test1(
  a: Int,
  b: Int
) -> Int {
  return a
}

//--- t1.2.swift
public func test1(
  a: Int,
  b: Int
) -> Int {
#if os(NotARealOS)
  inactiveUnknownName()
#endif
  return unknownName
}
