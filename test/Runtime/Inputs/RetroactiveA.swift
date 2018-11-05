import RetroactiveCommon

extension CommonStruct : CommonP1 {
  public typealias AssocType = Int
}

public func getCommonRequiresP1_Struct() -> Any.Type {
  return CommonRequiresP1<CommonStruct>.self
}
