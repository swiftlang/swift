class OtherWithInit {
  @available(OSX, introduced=10.11)
  init(withInt i: Int) { }
}

class SubOfOtherWithInit : OtherWithInit {
  // A trapping version of init(withInt:) will be synthesized here.

  @available(OSX, introduced=10.11)
  init() {
    super.init(withInt:77)
  }
}
