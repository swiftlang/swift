extension B {
  public init?<T : A>(_: T) where T.T == Self {
    return nil
  }
}
