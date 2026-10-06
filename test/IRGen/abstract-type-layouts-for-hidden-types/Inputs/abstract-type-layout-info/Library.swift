public struct Wrapper {
  private var hidden: HiddenCStruct

  public init(value: Int32) {
    hidden = HiddenCStruct(value: value)
  }
}
