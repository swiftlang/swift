
@available(OSX 10.11, *)
enum MTLPixelFormat : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case invalid
  case a8Unorm
  case r8Unorm
  case r8Snorm
  case r8Uint
  case r8Sint
  case r16Unorm
  case r16Snorm
  case r16Uint
  case r16Sint
  case r16Float
  case rg8Unorm
  case rg8Snorm
  case rg8Uint
  case rg8Sint
  case r32Uint
  case r32Sint
  case r32Float
  case rg16Unorm
  case rg16Snorm
  case rg16Uint
  case rg16Sint
  case rg16Float
  case rgba8Unorm
  case rgba8Unorm_sRGB
  case rgba8Snorm
  case rgba8Uint
  case rgba8Sint
  case bgra8Unorm
  case bgra8Unorm_sRGB
  case rgb10A2Unorm
  case rgb10A2Uint
  case rg11B10Float
  case rgb9E5Float
  case rg32Uint
  case rg32Sint
  case rg32Float
  case rgba16Unorm
  case rgba16Snorm
  case rgba16Uint
  case rgba16Sint
  case rgba16Float
  case rgba32Uint
  case rgba32Sint
  case rgba32Float
  @available(OSX 10.11, *)
  case BC1_RGBA
  @available(OSX 10.11, *)
  case bc1_RGBA_sRGB
  @available(OSX 10.11, *)
  case BC2_RGBA
  @available(OSX 10.11, *)
  case bc2_RGBA_sRGB
  @available(OSX 10.11, *)
  case BC3_RGBA
  @available(OSX 10.11, *)
  case bc3_RGBA_sRGB
  @available(OSX 10.11, *)
  case bc4_RUnorm
  @available(OSX 10.11, *)
  case bc4_RSnorm
  @available(OSX 10.11, *)
  case bc5_RGUnorm
  @available(OSX 10.11, *)
  case bc5_RGSnorm
  @available(OSX 10.11, *)
  case bc6H_RGBFloat
  @available(OSX 10.11, *)
  case bc6H_RGBUfloat
  @available(OSX 10.11, *)
  case bc7_RGBAUnorm
  @available(OSX 10.11, *)
  case bc7_RGBAUnorm_sRGB
  case GBGR422
  case BGRG422
  case depth32Float
  case stencil8
  @available(OSX 10.11, *)
  case depth24Unorm_Stencil8
  @available(OSX 10.11, *)
  case depth32Float_Stencil8
}
