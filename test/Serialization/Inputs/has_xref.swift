import has_alias
@exported import struct_with_operators

func numeric(x: MyInt64) {}
func conditional(x: AliasWrapper.Boolean) {}
func longInt(x: Int.EspeciallyMagicalInt) {}

func numericArray(x: IntSlice) {}


protocol ExtraIncrementable {
  @prefix func +++(inout base: Self)
}

extension SpecialInt : ExtraIncrementable {}
