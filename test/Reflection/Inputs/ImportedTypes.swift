import ctypes

public struct HasCTypes {
  let mcs = MyCStruct()
  let mce = MyCEnum(0)
  let mcu = MyCUnion()
  let mcsbf = MyCStructWithBitfields()
}

