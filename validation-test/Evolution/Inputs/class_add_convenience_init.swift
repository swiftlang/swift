open class AddConvenienceInit {
  public let age: Int

  public init(x: Int) {
    self.age = x
  }

#if BEFORE

  public convenience init(z: Int) {
    self.init(x: z * z + 2)
  }

#else

  public convenience init(z: Int) {
    self.init(y: z * z)
  }

  public convenience init(y: Int) {
    self.init(x: y + 2)
  }

#endif

}
