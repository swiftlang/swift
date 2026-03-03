// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %t/spec.json %t/main.swift | %FileCheck %s

// REQUIRES: swift_swift_parser

//--- main.swift
func foo() {}

// Make sure we don't crash on the unrelated comment refs:
// foo()
// foo(0)
// foo(a: 0)
// foo {}
// foo {} a: {}

// Nor when written in code:
foo()
foo(0)
foo(a: 0)
foo {}
foo {} a: {}

// CHECK:      source.edit.kind.active:
// CHECK-NEXT:   1:6-1:9 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.comment:
// CHECK-NEXT:   4:4-4:7 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.unknown:
// CHECK-NEXT:   5:4-5:7 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.unknown:
// CHECK-NEXT:   6:4-6:7 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.unknown:
// CHECK-NEXT:   7:4-7:7 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.unknown:
// CHECK-NEXT:   8:4-8:7 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.active:
// CHECK-NEXT:   11:1-11:4 source.refactoring.range.kind.basename
// CHECK-NEXT: source.edit.kind.mismatch:
// CHECK-NEXT: source.edit.kind.mismatch:
// CHECK-NEXT: source.edit.kind.mismatch:
// CHECK-NEXT: source.edit.kind.mismatch:

//--- spec.json
[
  {
    "key.name": "foo()",
    "key.locations": [
      {
        "key.line": 1,
        "key.column": 6,
        "key.nametype": source.syntacticrename.definition
      },
      {
        "key.line": 4,
        "key.column": 4,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 5,
        "key.column": 4,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 6,
        "key.column": 4,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 7,
        "key.column": 4,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 8,
        "key.column": 4,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 11,
        "key.column": 1,
        "key.nametype": source.syntacticrename.call
      },
      {
        "key.line": 12,
        "key.column": 1,
        "key.nametype": source.syntacticrename.call
      },
      {
        "key.line": 13,
        "key.column": 1,
        "key.nametype": source.syntacticrename.call
      },
      {
        "key.line": 14,
        "key.column": 1,
        "key.nametype": source.syntacticrename.call
      },
      {
        "key.line": 15,
        "key.column": 1,
        "key.nametype": source.syntacticrename.call
      },
    ]
  }
]
