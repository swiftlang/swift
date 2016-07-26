
@available(tvOS 8.0, *)
enum MTLPixelFormat : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case invalid
  case a8Unorm
  case r8Unorm
  @available(tvOS 8.0, *)
  case r8Unorm_sRGB
  case r8Snorm
  case r8Uint
  case r8Sint
  case r16Unorm
  case r16Snorm
  case r16Uint
  case r16Sint
  case r16Float
  case rg8Unorm
  @available(tvOS 8.0, *)
  case rg8Unorm_sRGB
  case rg8Snorm
  case rg8Uint
  case rg8Sint
  @available(tvOS 8.0, *)
  case b5g6R5Unorm
  @available(tvOS 8.0, *)
  case a1bgr5Unorm
  @available(tvOS 8.0, *)
  case abgr4Unorm
  @available(tvOS 8.0, *)
  case bgr5A1Unorm
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
  @available(tvOS 8.0, *)
  case PVRTC_RGB_2BPP
  @available(tvOS 8.0, *)
  case pvrtc_RGB_2BPP_sRGB
  @available(tvOS 8.0, *)
  case PVRTC_RGB_4BPP
  @available(tvOS 8.0, *)
  case pvrtc_RGB_4BPP_sRGB
  @available(tvOS 8.0, *)
  case PVRTC_RGBA_2BPP
  @available(tvOS 8.0, *)
  case pvrtc_RGBA_2BPP_sRGB
  @available(tvOS 8.0, *)
  case PVRTC_RGBA_4BPP
  @available(tvOS 8.0, *)
  case pvrtc_RGBA_4BPP_sRGB
  @available(tvOS 8.0, *)
  case eac_R11Unorm
  @available(tvOS 8.0, *)
  case eac_R11Snorm
  @available(tvOS 8.0, *)
  case eac_RG11Unorm
  @available(tvOS 8.0, *)
  case eac_RG11Snorm
  @available(tvOS 8.0, *)
  case EAC_RGBA8
  @available(tvOS 8.0, *)
  case eac_RGBA8_sRGB
  @available(tvOS 8.0, *)
  case ETC2_RGB8
  @available(tvOS 8.0, *)
  case etc2_RGB8_sRGB
  @available(tvOS 8.0, *)
  case ETC2_RGB8A1
  @available(tvOS 8.0, *)
  case etc2_RGB8A1_sRGB
  @available(tvOS 8.0, *)
  case astc_4x4_sRGB
  @available(tvOS 8.0, *)
  case astc_5x4_sRGB
  @available(tvOS 8.0, *)
  case astc_5x5_sRGB
  @available(tvOS 8.0, *)
  case astc_6x5_sRGB
  @available(tvOS 8.0, *)
  case astc_6x6_sRGB
  @available(tvOS 8.0, *)
  case astc_8x5_sRGB
  @available(tvOS 8.0, *)
  case astc_8x6_sRGB
  @available(tvOS 8.0, *)
  case astc_8x8_sRGB
  @available(tvOS 8.0, *)
  case astc_10x5_sRGB
  @available(tvOS 8.0, *)
  case astc_10x6_sRGB
  @available(tvOS 8.0, *)
  case astc_10x8_sRGB
  @available(tvOS 8.0, *)
  case astc_10x10_sRGB
  @available(tvOS 8.0, *)
  case astc_12x10_sRGB
  @available(tvOS 8.0, *)
  case astc_12x12_sRGB
  @available(tvOS 8.0, *)
  case astc_4x4_LDR
  @available(tvOS 8.0, *)
  case astc_5x4_LDR
  @available(tvOS 8.0, *)
  case astc_5x5_LDR
  @available(tvOS 8.0, *)
  case astc_6x5_LDR
  @available(tvOS 8.0, *)
  case astc_6x6_LDR
  @available(tvOS 8.0, *)
  case astc_8x5_LDR
  @available(tvOS 8.0, *)
  case astc_8x6_LDR
  @available(tvOS 8.0, *)
  case astc_8x8_LDR
  @available(tvOS 8.0, *)
  case astc_10x5_LDR
  @available(tvOS 8.0, *)
  case astc_10x6_LDR
  @available(tvOS 8.0, *)
  case astc_10x8_LDR
  @available(tvOS 8.0, *)
  case astc_10x10_LDR
  @available(tvOS 8.0, *)
  case astc_12x10_LDR
  @available(tvOS 8.0, *)
  case astc_12x12_LDR
  case GBGR422
  case BGRG422
  case depth32Float
  case stencil8
  @available(tvOS 9.0, *)
  case depth32Float_Stencil8
}
