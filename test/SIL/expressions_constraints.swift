// RUN: %swift -parse-as-library -dump-sil %s | FileCheck %s

// FIXME: This is in a separate file because expressions.swift causes crashes
// in the constraint checker.

// CHECK: func_decl containers
func containers() -> (Int[], Dictionary) {
  return ([1, 2, 3], ["Ankeny": 1, "Burnside": 2, "Couch": 3]);
}
