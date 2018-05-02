// Check we don't crash when verifying debug info, and that the debug
// scopes for alloc_stack instructions inserted by the LoadableByAddress
// pass are sensible.

// REQUIRES: asserts
// RUN: %target-swift-frontend -Onone -emit-ir %s \
// RUN:   -sil-verify-all -Xllvm -verify-di-holes \
// RUN:   -Xllvm -debug-only=loadable-address -Xllvm -sil-print-debuginfo \
// RUN:   -g -o /dev/null 2>&1 | %FileCheck %s

struct m {
  let major: Int
  let minor: Int
  let n: Int
  let o: [String]
  let p: [String]

// CHECK-LABEL: LoadableByAddress REWROTE: $S4null1mV5major5minor1n1o1pACSi_S2iSaySSGAItcfC
// CHECK: sil_scope 1 { loc {{.*}}:[[@LINE+2]]:3 parent @$S4null1mV5major5minor1n1o1pACSi_S2iSaySSGAItcfC
// CHECK: alloc_stack $m, var, name "self", {{.*}} scope 1
  init(major: Int, minor: Int, n: Int, o: [String], p: [String]) {
    self.major = major
    self.minor = minor
    self.n = n
    self.o = o
    self.p = p
  }
// CHECK: // end sil function
}

enum a {
  case any
  case b(m)
}

struct c<e>  {
  enum f {
    case g(a)
  }
}

struct h<i>{
  typealias j = i
  typealias d = j
  typealias f = c<d>.f
  subscript(identifier: d) -> f {
      return .g(.any)
  }

// CHECK-LABEL: LoadableByAddress REWROTE: $S4null1hV1k1l10identifierACyxGAA1cV1fOyx_G_xtF
// CHECK: sil_scope 1 { loc {{.*}}:[[@LINE+4]]:8 parent @$S4null1hV1k1l10identifierACyxGAA1cV1fOyx_G_xtF
// CHECK: sil_scope 2 { loc {{.*}}:[[@LINE+8]]:3 parent 1
// CHECK: alloc_stack $c<i>.f, {{.*}} scope 2
// CHECK: alloc_stack $i, {{.*}} scope 2
  func k(l: f, identifier: d) -> h {
    switch (l, self[identifier]) {
    default:
        return self
    }
  }
// CHECK: // end sil function
}
