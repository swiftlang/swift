struct Generic<Element> {}

extension Generic {
  // It is important that this has the same name as a standard library type.
  struct Iterator {}
}
