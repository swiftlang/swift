// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/out)
// RUN: split-file %s %t

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
// CHECK-NEXT:       key.line: 6,
// CHECK-NEXT:       key.column: 3,
// CHECK-NEXT:       key.filepath: "{{.*[/\\]}}t1.swift",
// CHECK-NEXT:       key.severity: source.diagnostic.severity.error,
// CHECK-NEXT:       key.id: "cannot_find_in_scope",
// CHECK-NEXT:       key.description: "cannot find 'foobar' in scope",
// CHECK-NEXT:       key.ranges: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.offset: 105,
// CHECK-NEXT:           key.length: 6
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
public func test1() {
  struct English {}
  print("This is just a test")
}

//--- t1.2.swift
/// Adding comment.
public func test1() {
  print("This is a test refined\(1 + 20)");
  struct Spanish { }
  print("test")
  foobar
}

