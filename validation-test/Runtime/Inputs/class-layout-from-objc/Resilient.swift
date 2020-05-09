public struct GrowsToInt64 {
  #if SMALL
  var value: Int32
  #elseif BIG
  var value: Int64
  #else
  #error("Must define SMALL or BIG")
  #endif

  public init() { self.value = 0 }
}
