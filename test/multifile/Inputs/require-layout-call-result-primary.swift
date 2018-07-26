func foo<T, U: C<T>>(_ t: T, _ u: U) {
  // Calling a function that returns a C<T> requests its layout
  _ = bar(t, u)
}
