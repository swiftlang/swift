// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/generics -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/generics -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/generics -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

func blackHole(_: Any...) {}

protocol First {
  associatedtype Assoc : First

  // Just to confuse things -- a method with the same name as an
  // associated type
  func Assoc(_: Int) -> Int
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

// DEMANGLE-TYPE: $sxD
// DEMANGLE-TYPE: $s5AssocQzD
// DEMANGLE-TYPE: $s5Assoc_AAQZD

// CHECK-TYPE: τ_0_0
// CHECK-TYPE: τ_0_0.Assoc
// CHECK-TYPE: τ_0_0.Assoc.Assoc

// DEMANGLE-TYPE: $sq_D
// DEMANGLE-TYPE: $s5AssocQy_D
// DEMANGLE-TYPE: $s5Assoc_AAQY_D

// CHECK-TYPE: τ_0_1
// CHECK-TYPE: τ_0_1.Assoc
// CHECK-TYPE: τ_0_1.Assoc.Assoc

// DEMANGLE-TYPE: $sqd__D
// DEMANGLE-TYPE: $s5AssocQyd__D
// DEMANGLE-TYPE: $s5Assoc_AAQYd__D

// CHECK-TYPE: τ_1_0
// CHECK-TYPE: τ_1_0.Assoc
// CHECK-TYPE: τ_1_0.Assoc.Assoc

// DEMANGLE-TYPE: $sqd_0_D
// DEMANGLE-TYPE: $s5AssocQyd_0_D
// DEMANGLE-TYPE: $s5Assoc_AAQYd_0_D

// CHECK-TYPE: τ_1_1
// CHECK-TYPE: τ_1_1.Assoc
// CHECK-TYPE: τ_1_1.Assoc.Assoc

// DEMANGLE-TYPE: $s5Assoc8generics5FirstPQzD
// DEMANGLE-TYPE: $s5Assoc8generics5FirstP_AaDQZD

// CHECK-TYPE: τ_0_0.Assoc
// CHECK-TYPE: τ_0_0.Assoc.Assoc

// DEMANGLE-TYPE: $s5Assoc8generics5FirstPQy_D
// DEMANGLE-TYPE: $s5Assoc8generics5FirstP_AaDQY_D

// CHECK-TYPE: τ_0_1.Assoc
// CHECK-TYPE: τ_0_1.Assoc.Assoc

// DEMANGLE-TYPE: $s5Assoc8generics5FirstPQyd__D
// DEMANGLE-TYPE: $s5Assoc8generics5FirstP_AaDQYd__D

// CHECK-TYPE: τ_1_0.Assoc
// CHECK-TYPE: τ_1_0.Assoc.Assoc

// DEMANGLE-TYPE: $s5Assoc8generics5FirstPQyd_0_D
// DEMANGLE-TYPE: $s5Assoc8generics5FirstP_AaDQYd_0_D

// CHECK-TYPE: τ_1_1.Assoc
// CHECK-TYPE: τ_1_1.Assoc.Assoc

// DEMANGLE-DECL: $s8generics5FirstP5AssocQa
// CHECK-DECL: generics.(file).First.Assoc

// DEMANGLE-DECL: $s8generics6SecondP5AssocQa
// CHECK-DECL: generics.(file).Second.Assoc