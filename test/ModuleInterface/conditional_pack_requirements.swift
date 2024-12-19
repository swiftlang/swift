// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/conditional_pack_requirements.swiftinterface) %s -target %target-swift-5.9-abi-triple
// RUN: %FileCheck %s < %t/conditional_pack_requirements.swiftinterface

public protocol P {
  associatedtype A
}

public protocol Q {}

public class C<each T> {}

public struct GG1<A: P, each B: P> where A.A == C<repeat (each B).A> {}

extension GG1: Q where A: Q, repeat each B: Q {}

// CHECK-LABEL: public struct GG1<A, each B> where A : conditional_pack_requirements.P, repeat each B : conditional_pack_requirements.P, A.A == conditional_pack_requirements.C<repeat (each B).A> {
// CHECK-LABEL: extension conditional_pack_requirements.GG1 : conditional_pack_requirements.Q where A : conditional_pack_requirements.Q, repeat each B : conditional_pack_requirements.Q {


public struct GG2<each A: P> {
  public struct Nested<each B: P> where repeat (each A).A == (each B).A {}
}

extension GG2.Nested: Q where repeat each A: Q, repeat each B: Q {}

// CHECK-LABEL: public struct GG2<each A> where repeat each A : conditional_pack_requirements.P {
// CHECK-LABEL: public struct Nested<each B> where repeat each B : conditional_pack_requirements.P, repeat (each A).A == (each B).A {
// CHECK-LABEL: extension conditional_pack_requirements.GG2.Nested : conditional_pack_requirements.Q where repeat each A : conditional_pack_requirements.Q, repeat each B : conditional_pack_requirements.Q {


public struct GG3<A: P, each B: P> where A.A : C<repeat (each B).A> {}

extension GG3: Q where A: Q, repeat each B: Q {}

// CHECK-LABEL: public struct GG3<A, each B> where A : conditional_pack_requirements.P, repeat each B : conditional_pack_requirements.P, A.A : conditional_pack_requirements.C<repeat (each B).A> {
// CHECK-LABEL: extension conditional_pack_requirements.GG3 : conditional_pack_requirements.Q where A : conditional_pack_requirements.Q, repeat each B : conditional_pack_requirements.Q {


public struct GG4<each A: P> {
  public struct Nested<each B: P> where repeat (each A).A : C<(each B).A> {}
}

extension GG4.Nested: Q where repeat each A: Q, repeat each B: Q {}

// CHECK-LABEL: public struct GG4<each A> where repeat each A : conditional_pack_requirements.P {
// CHECK-LABEL: public struct Nested<each B> where repeat each B : conditional_pack_requirements.P, repeat (each A).A : conditional_pack_requirements.C<(each B).A> {
// CHECK-LABEL: extension conditional_pack_requirements.GG4.Nested : conditional_pack_requirements.Q where repeat each A : conditional_pack_requirements.Q, repeat each B : conditional_pack_requirements.Q {
