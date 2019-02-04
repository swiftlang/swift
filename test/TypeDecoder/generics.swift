// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/generics -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/generics -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

protocol First {
  associatedtype Assoc : First
}

protocol Second {
  associatedtype Assoc : Second
}

struct OuterFirst<A : First, B : First> {
  struct Inner<C : First, D : First> {
    func method(a: A, b: B, c: C, d: D) {
      do {
        let fn: (A, A.Assoc, A.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (B, B.Assoc, B.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (C, C.Assoc, C.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (D, D.Assoc, D.Assoc.Assoc) -> () = { _, _, _ in }
      }
    }
  }
}

struct OuterBoth<A : First & Second, B : First & Second> {
  struct Inner<C : First & Second, D : First & Second> {
    func method(a: A, b: B, c: C, d: D) {
      do {
        let fn: (A, A.Assoc, A.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (B, B.Assoc, B.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (C, C.Assoc, C.Assoc.Assoc) -> () = { _, _, _ in }
      }
      do {
        let fn: (D, D.Assoc, D.Assoc.Assoc) -> () = { _, _, _ in }
      }
    }
  }
}

// DEMANGLE: $sxD
// DEMANGLE: $s5AssocQzD
// DEMANGLE: $s5Assoc_AAQZD

// CHECK: τ_0_0
// CHECK: τ_0_0.Assoc
// CHECK: τ_0_0.Assoc.Assoc

// DEMANGLE: $sq_D
// DEMANGLE: $s5AssocQy_D
// DEMANGLE: $s5Assoc_AAQY_D

// CHECK: τ_0_1
// CHECK: τ_0_1.Assoc
// CHECK: τ_0_1.Assoc.Assoc

// DEMANGLE: $sqd__D
// DEMANGLE: $s5AssocQyd__D
// DEMANGLE: $s5Assoc_AAQYd__D

// CHECK: τ_1_0
// CHECK: τ_1_0.Assoc
// CHECK: τ_1_0.Assoc.Assoc

// DEMANGLE: $sqd_0_D
// DEMANGLE: $s5AssocQyd_0_D
// DEMANGLE: $s5Assoc_AAQYd_0_D

// CHECK: τ_1_1
// CHECK: τ_1_1.Assoc
// CHECK: τ_1_1.Assoc.Assoc

// DEMANGLE: $s5Assoc8generics5FirstPQzD
// DEMANGLE: $s5Assoc8generics5FirstP_AaDQZD

// CHECK: τ_0_0.Assoc
// CHECK: τ_0_0.Assoc.Assoc

// DEMANGLE: $s5Assoc8generics5FirstPQy_D
// DEMANGLE: $s5Assoc8generics5FirstP_AaDQY_D

// CHECK: τ_0_1.Assoc
// CHECK: τ_0_1.Assoc.Assoc

// DEMANGLE: $s5Assoc8generics5FirstPQyd__D
// DEMANGLE: $s5Assoc8generics5FirstP_AaDQYd__D

// CHECK: τ_1_0.Assoc
// CHECK: τ_1_0.Assoc.Assoc

// DEMANGLE: $s5Assoc8generics5FirstPQyd_0_D
// DEMANGLE: $s5Assoc8generics5FirstP_AaDQYd_0_D

// CHECK: τ_1_1.Assoc
// CHECK: τ_1_1.Assoc.Assoc
