// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

//--- input.swift
struct Sss {
}

_ = Mismatch()
//  Mismatch()
_ = Sss()

//--- dummy.txt
// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %t/rename-spec.json %t/input.swift | %FileCheck %s

// RUN: %empty-directory(%t.ranges)
// RUN: %sourcekitd-test -req=find-rename-ranges -rename-spec %t/rename-spec.json %t/input.swift | %FileCheck %s

// CHECK: source.edit.kind.active:
// CHECK:   5:8-5:11 source.refactoring.range.kind.basename
// CHECK: source.edit.kind.unknown:
// CHECK: source.edit.kind.mismatch:
// CHECK: source.edit.kind.active:
// CHECK:   10:5-10:8 source.refactoring.range.kind.basename

//--- rename-spec.json

[
  {
    "key.name": "Sss",
    "key.newname": "Ttt",
    "key.is_function_like": 0,
    "key.is_non_protocol_type": 1,
    "key.locations": [
      {
        "key.line": 5,
        "key.column": 8,
        "key.nametype": source.syntacticrename.definition
      },
      {
        "key.line": 8,
        "key.column": 5,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 9,
        "key.column": 5,
        "key.nametype": source.syntacticrename.unknown
      },
      {
        "key.line": 10,
        "key.column": 5,
        "key.nametype": source.syntacticrename.reference
      }
    ]
  }
]
