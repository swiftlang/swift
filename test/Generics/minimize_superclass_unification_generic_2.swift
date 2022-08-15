// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

class A<T, U, V> {}

class B<X, Y, Z, W> : A<X, (Y) -> Z, Int> {}

class C<I, J : Sequence, K> : B<String, I, [J.Element], K> {}


// ---------------------------------------------- //
// Unifying two requirements in the same protocol //
// ---------------------------------------------- //

// CHECK-LABEL: .Pab@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pab]A1 == Self.[Pab]B1,
// CHECK-SAME: Self.[Pab]A2 == (Self.[Pab]B2) -> Self.[Pab]B3,
// CHECK-SAME: Self.[Pab]A3 == Int,

// CHECK-SAME: Self.[Pab]T : B<Self.[Pab]A1, Self.[Pab]B2, Self.[Pab]B3, Self.[Pab]B4>>

protocol Pab {
  associatedtype T where T : A<A1, A2, A3>, T : B<B1, B2, B3, B4>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4
}

// CHECK-LABEL: .Pba@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pba]A1 == Self.[Pba]B1,
// CHECK-SAME: Self.[Pba]A2 == (Self.[Pba]B2) -> Self.[Pba]B3,
// CHECK-SAME: Self.[Pba]A3 == Int,

// CHECK-SAME: Self.[Pba]T : B<Self.[Pba]A1, Self.[Pba]B2, Self.[Pba]B3, Self.[Pba]B4>>

protocol Pba {
  associatedtype T where T : B<B1, B2, B3, B4>, T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4
}

// CHECK-LABEL: .Pac@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pac]A1 == String,
// CHECK-SAME: Self.[Pac]A2 == (Self.[Pac]C1) -> [Self.[Pac]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pac]A3 == Int,

// CHECK-SAME: Self.[Pac]C2 : Sequence,

// CHECK-SAME: Self.[Pac]T : C<Self.[Pac]C1, Self.[Pac]C2, Self.[Pac]C3>>

protocol Pac {
  associatedtype T where T : A<A1, A2, A3>, T : C<C1, C2, C3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pca@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pca]A1 == String,
// CHECK-SAME: Self.[Pca]A2 == (Self.[Pca]C1) -> [Self.[Pca]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pca]A3 == Int,

// CHECK-SAME: Self.[Pca]C2 : Sequence,

// CHECK-SAME: Self.[Pca]T : C<Self.[Pca]C1, Self.[Pca]C2, Self.[Pca]C3>>

protocol Pca {
  associatedtype T where T : C<C1, C2, C3>, T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pbc@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pbc]B1 == String,
// CHECK-SAME: Self.[Pbc]B2 == Self.[Pbc]C1,
// CHECK-SAME: Self.[Pbc]B3 == [Self.[Pbc]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pbc]B4 == Self.[Pbc]C3,

// CHECK-SAME: Self.[Pbc]C2 : Sequence,

// CHECK-SAME: Self.[Pbc]T : C<Self.[Pbc]B2, Self.[Pbc]C2, Self.[Pbc]B4>>

protocol Pbc {
  associatedtype T where T : B<B1, B2, B3, B4>, T : C<C1, C2, C3>

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pcb@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pcb]B1 == String,
// CHECK-SAME: Self.[Pcb]B2 == Self.[Pcb]C1,
// CHECK-SAME: Self.[Pcb]B3 == [Self.[Pcb]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pcb]B4 == Self.[Pcb]C3,

// CHECK-SAME: Self.[Pcb]C2 : Sequence,

// CHECK-SAME: Self.[Pcb]T : C<Self.[Pcb]B2, Self.[Pcb]C2, Self.[Pcb]B4>>

protocol Pcb {
  associatedtype T where T : C<C1, C2, C3>, T : B<B1, B2, B3, B4>

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}


// ------------------------------------------------ //
// Unifying three requirements in the same protocol //
// ------------------------------------------------ //

// CHECK-LABEL: .Pabc@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pabc]A1 == String,
// CHECK-SAME: Self.[Pabc]A2 == (Self.[Pabc]B2) -> [Self.[Pabc]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pabc]A3 == Int,

// CHECK-SAME: Self.[Pabc]B1 == String,
// CHECK-SAME: Self.[Pabc]B2 == Self.[Pabc]C1,
// CHECK-SAME: Self.[Pabc]B3 == [Self.[Pabc]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pabc]B4 == Self.[Pabc]C3,

// CHECK-SAME: Self.[Pabc]C2 : Sequence,

// CHECK-SAME: Self.[Pabc]T : C<Self.[Pabc]B2, Self.[Pabc]C2, Self.[Pabc]B4>>

protocol Pabc {
  associatedtype T where T : A<A1, A2, A3>, T : B<B1, B2, B3, B4>, T : C<C1, C2, C3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pacb@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pacb]A1 == String,
// CHECK-SAME: Self.[Pacb]A2 == (Self.[Pacb]B2) -> [Self.[Pacb]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pacb]A3 == Int,

// CHECK-SAME: Self.[Pacb]B1 == String,
// CHECK-SAME: Self.[Pacb]B2 == Self.[Pacb]C1,
// CHECK-SAME: Self.[Pacb]B3 == [Self.[Pacb]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pacb]B4 == Self.[Pacb]C3,

// CHECK-SAME: Self.[Pacb]C2 : Sequence,

// CHECK-SAME: Self.[Pacb]T : C<Self.[Pacb]B2, Self.[Pacb]C2, Self.[Pacb]B4>>

protocol Pacb {
  associatedtype T where T : A<A1, A2, A3>, T : C<C1, C2, C3>, T : B<B1, B2, B3, B4>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pbac@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pbac]A1 == String,
// CHECK-SAME: Self.[Pbac]A2 == (Self.[Pbac]B2) -> [Self.[Pbac]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pbac]A3 == Int,

// CHECK-SAME: Self.[Pbac]B1 == String,
// CHECK-SAME: Self.[Pbac]B2 == Self.[Pbac]C1,
// CHECK-SAME: Self.[Pbac]B3 == [Self.[Pbac]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pbac]B4 == Self.[Pbac]C3,

// CHECK-SAME: Self.[Pbac]C2 : Sequence,

// CHECK-SAME: Self.[Pbac]T : C<Self.[Pbac]B2, Self.[Pbac]C2, Self.[Pbac]B4>>

protocol Pbac {
  associatedtype T where T : B<B1, B2, B3, B4>, T : A<A1, A2, A3>, T : C<C1, C2, C3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pbca@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pbca]A1 == String,
// CHECK-SAME: Self.[Pbca]A2 == (Self.[Pbca]B2) -> [Self.[Pbca]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pbca]A3 == Int,

// CHECK-SAME: Self.[Pbca]B1 == String,
// CHECK-SAME: Self.[Pbca]B2 == Self.[Pbca]C1,
// CHECK-SAME: Self.[Pbca]B3 == [Self.[Pbca]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pbca]B4 == Self.[Pbca]C3,

// CHECK-SAME: Self.[Pbca]C2 : Sequence,

// CHECK-SAME: Self.[Pbca]T : C<Self.[Pbca]B2, Self.[Pbca]C2, Self.[Pbca]B4>>

protocol Pbca {
  associatedtype T where T : B<B1, B2, B3, B4>, T : C<C1, C2, C3>, T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pcab@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pcab]A1 == String,
// CHECK-SAME: Self.[Pcab]A2 == (Self.[Pcab]B2) -> [Self.[Pcab]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pcab]A3 == Int,

// CHECK-SAME: Self.[Pcab]B1 == String,
// CHECK-SAME: Self.[Pcab]B2 == Self.[Pcab]C1,
// CHECK-SAME: Self.[Pcab]B3 == [Self.[Pcab]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pcab]B4 == Self.[Pcab]C3,

// CHECK-SAME: Self.[Pcab]C2 : Sequence,

// CHECK-SAME: Self.[Pcab]T : C<Self.[Pcab]B2, Self.[Pcab]C2, Self.[Pcab]B4>>

protocol Pcab {
  associatedtype T where T : C<C1, C2, C3>, T : A<A1, A2, A3>, T : B<B1, B2, B3, B4>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .Pcba@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[Pcba]A1 == String,
// CHECK-SAME: Self.[Pcba]A2 == (Self.[Pcba]B2) -> [Self.[Pcba]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pcba]A3 == Int,

// CHECK-SAME: Self.[Pcba]B1 == String,
// CHECK-SAME: Self.[Pcba]B2 == Self.[Pcba]C1,
// CHECK-SAME: Self.[Pcba]B3 == [Self.[Pcba]C2.[Sequence]Element],
// CHECK-SAME: Self.[Pcba]B4 == Self.[Pcba]C3,

// CHECK-SAME: Self.[Pcba]C2 : Sequence,

// CHECK-SAME: Self.[Pcba]T : C<Self.[Pcba]B2, Self.[Pcba]C2, Self.[Pcba]B4>>

protocol Pcba {
  associatedtype T where T : C<C1, C2, C3>, T : B<B1, B2, B3, B4>, T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}


// --------------------------------------------------------- //
// Unifying two requirements in another and current protocol //
// --------------------------------------------------------- //

protocol Pa {
  associatedtype T where T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3
}

protocol Pb {
  associatedtype T where T : B<B1, B2, B3, B4>

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4
}

protocol Pc {
  associatedtype T where T : C<C1, C2, C3>

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .PaQb@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PaQb]T : Pa,

// CHECK-SAME: Self.[PaQb]T.[Pa]T : B<Self.[PaQb]B1, Self.[PaQb]B2, Self.[PaQb]B3, Self.[PaQb]B4>>

protocol PaQb {
  associatedtype T : Pa where T.T : B<B1, B2, B3, B4>

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4
}

// CHECK-LABEL: .PbQa@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PbQa]A1 == Self.[PbQa]T.[Pb]B1,
// CHECK-SAME: Self.[PbQa]A2 == (Self.[PbQa]T.[Pb]B2) -> Self.[PbQa]T.[Pb]B3,
// CHECK-SAME: Self.[PbQa]A3 == Int,

// CHECK-SAME: Self.[PbQa]T : Pb>

protocol PbQa {
  associatedtype T : Pb where T.T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3
}

// CHECK-LABEL: .PaQc@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PaQc]C2 : Sequence,

// CHECK-SAME: Self.[PaQc]T : Pa,

// CHECK-SAME: Self.[PaQc]T.[Pa]T : C<Self.[PaQc]C1, Self.[PaQc]C2, Self.[PaQc]C3>>

protocol PaQc {
  associatedtype T : Pa where T.T : C<C1, C2, C3>

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .PcQa@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PcQa]A1 == String,
// CHECK-SAME: Self.[PcQa]A2 == (Self.[PcQa]T.[Pc]C1) -> [Self.[PcQa]T.[Pc]C2.[Sequence]Element],
// CHECK-SAME: Self.[PcQa]A3 == Int,

// CHECK-SAME: Self.[PcQa]T : Pc>

protocol PcQa {
  associatedtype T : Pc where T.T : A<A1, A2, A3>

  associatedtype A1
  associatedtype A2
  associatedtype A3
}

// CHECK-LABEL: .PbQc@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PbQc]C2 : Sequence,

// CHECK-SAME: Self.[PbQc]T : Pb,

// CHECK-SAME: Self.[PbQc]T.[Pb]T : C<Self.[PbQc]C1, Self.[PbQc]C2, Self.[PbQc]C3>>

protocol PbQc {
  associatedtype T : Pb where T.T : C<C1, C2, C3>

  associatedtype C1
  associatedtype C2 : Sequence
  associatedtype C3
}

// CHECK-LABEL: .PcQb@
// CHECK-NEXT: Requirement signature: <Self where

// CHECK-SAME: Self.[PcQb]B1 == String,
// CHECK-SAME: Self.[PcQb]B2 == Self.[PcQb]T.[Pc]C1,
// CHECK-SAME: Self.[PcQb]B3 == [Self.[PcQb]T.[Pc]C2.[Sequence]Element],
// CHECK-SAME: Self.[PcQb]B4 == Self.[PcQb]T.[Pc]C3,

// CHECK-SAME: Self.[PcQb]T : Pc>

protocol PcQb {
  associatedtype T : Pc where T.T : B<B1, B2, B3, B4>

  associatedtype B1
  associatedtype B2
  associatedtype B3
  associatedtype B4
}
