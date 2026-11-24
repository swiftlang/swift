trait S {
  type A;
  type B;
  type C: S<A = Self::B, C = Self>;
}

fn f<T: S>(x: <T::C as S>::B) -> T::A {
  return x;
  // note: expected associated type `<T as S>::A'
  // found associated type `<<T as S>::C as S>::B'
}
