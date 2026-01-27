// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Make sure we don't crash when emitting SILGen
// RUN: %target-swift-emit-silgen %t/main.swift -I %t -cxx-interoperability-mode=default

//--- header.h
struct Foo {
  void bar() const;
};

//--- module.modulemap
module CModule {
  header "header.h"
  requires cplusplus
  export *
}

//--- main.swift
import CModule

func test(_ foo: Foo) {
  let a0 = 0, b0 = 0, c0 = 0, d0 = 0, e0 = 0, f0 = 0, g0 = 0, h0 = 0, i0 = 0, j0 = 0, k0 = 0, l0 = 0, m0 = 0, n0 = 0, o0 = 0, p0 = 0, q0 = 0, r0 = 0, s0 = 0, t0 = 0, u0 = 0, v0 = 0, w0 = 0, x0 = 0, y0 = 0, z0 = 0, a1 = 0, b1 = 0, c1 = 0, d1 = 0, e1 = 0, f1 = 0, g1 = 0, h1 = 0, i1 = 0, j1 = 0, k1 = 0, l1 = 0, m1 = 0, n1 = 0, o1 = 0, p1 = 0, q1 = 0, r1 = 0, s1 = 0, t1 = 0, u1 = 0
  foo.bar()
}
