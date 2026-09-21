public struct Holder {
  var foo: Int {
    @storageRestrictions(initializes: backing)
    init(initialValue) {}
    get { fatalError() }
  }

  var backing: Int

  var bar: Int = 0 {
    @storageRestrictions(initializes: backing)
    init(initialValue) {}
    get { fatalError() }
  }
}
