func foo(o: Int?) {
  switch o {
  case nil:
    break
  case .none:
    break
  }
}


// RUN: %sourcekitd-test -req=open %s -- %s == -req=print-annotations %s -- %s | %FileCheck %s

// CHECK:      [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.ref.struct,
// CHECK-NEXT:     key.offset: 12,
// CHECK-NEXT:     key.length: 3,
// CHECK-NEXT:     key.is_system: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.ref.var.local,
// CHECK-NEXT:     key.offset: 29,
// CHECK-NEXT:     key.length: 1
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.ref.enumelement,
// CHECK-NEXT:     key.offset: 63,
// CHECK-NEXT:     key.length: 4,
// CHECK-NEXT:     key.is_system: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ]
