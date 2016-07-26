
typealias cl_char = Int8
typealias cl_uchar = UInt8
typealias cl_short = Int16
typealias cl_ushort = UInt16
typealias cl_int = Int32
typealias cl_uint = UInt32
typealias cl_long = Int64
typealias cl_ulong = UInt64
typealias cl_half = UInt16
typealias cl_float = Float
typealias cl_double = Double
var CL_CHAR_BIT: Int32 { get }
var CL_SCHAR_MAX: Int32 { get }
var CL_CHAR_MAX: Int32 { get }
var CL_UCHAR_MAX: Int32 { get }
var CL_SHRT_MAX: Int32 { get }
var CL_USHRT_MAX: Int32 { get }
var CL_INT_MAX: Int32 { get }
var CL_UINT_MAX: UInt32 { get }
var CL_FLT_DIG: Int32 { get }
var CL_FLT_MANT_DIG: Int32 { get }
var CL_FLT_MAX_10_EXP: Int32 { get }
var CL_FLT_MAX_EXP: Int32 { get }
var CL_FLT_MIN_10_EXP: Int32 { get }
var CL_FLT_MIN_EXP: Int32 { get }
var CL_FLT_RADIX: Int32 { get }
var CL_FLT_MAX: Float { get }
var CL_FLT_MIN: Float { get }
var CL_FLT_EPSILON: Float { get }
var CL_DBL_DIG: Int32 { get }
var CL_DBL_MANT_DIG: Int32 { get }
var CL_DBL_MAX_10_EXP: Int32 { get }
var CL_DBL_MAX_EXP: Int32 { get }
var CL_DBL_MIN_10_EXP: Int32 { get }
var CL_DBL_MIN_EXP: Int32 { get }
var CL_DBL_RADIX: Int32 { get }
var CL_DBL_MAX: Double { get }
var CL_DBL_MIN: Double { get }
var CL_DBL_EPSILON: Double { get }
var CL_M_E: Double { get }
var CL_M_LOG2E: Double { get }
var CL_M_LOG10E: Double { get }
var CL_M_LN2: Double { get }
var CL_M_LN10: Double { get }
var CL_M_PI: Double { get }
var CL_M_PI_2: Double { get }
var CL_M_PI_4: Double { get }
var CL_M_1_PI: Double { get }
var CL_M_2_PI: Double { get }
var CL_M_2_SQRTPI: Double { get }
var CL_M_SQRT2: Double { get }
var CL_M_SQRT1_2: Double { get }
var CL_M_E_F: Float { get }
var CL_M_LOG2E_F: Float { get }
var CL_M_LOG10E_F: Float { get }
var CL_M_LN2_F: Float { get }
var CL_M_LN10_F: Float { get }
var CL_M_PI_F: Float { get }
var CL_M_PI_2_F: Float { get }
var CL_M_PI_4_F: Float { get }
var CL_M_1_PI_F: Float { get }
var CL_M_2_PI_F: Float { get }
var CL_M_2_SQRTPI_F: Float { get }
var CL_M_SQRT2_F: Float { get }
var CL_M_SQRT1_2_F: Float { get }
var CL_MAXFLOAT: Float { get }
typealias cl_GLuint = UInt32
typealias cl_GLint = Int32
typealias cl_GLenum = UInt32
typealias __cl_float4 = float4
var __CL_FLOAT4__: Int32 { get }
var __CL_UCHAR16__: Int32 { get }
var __CL_CHAR16__: Int32 { get }
var __CL_USHORT8__: Int32 { get }
var __CL_SHORT8__: Int32 { get }
var __CL_INT4__: Int32 { get }
var __CL_UINT4__: Int32 { get }
var __CL_ULONG2__: Int32 { get }
var __CL_LONG2__: Int32 { get }
var __CL_DOUBLE2__: Int32 { get }
var __CL_UCHAR8__: Int32 { get }
var __CL_CHAR8__: Int32 { get }
var __CL_USHORT4__: Int32 { get }
var __CL_SHORT4__: Int32 { get }
var __CL_INT2__: Int32 { get }
var __CL_UINT2__: Int32 { get }
var __CL_ULONG1__: Int32 { get }
var __CL_LONG1__: Int32 { get }
var __CL_FLOAT2__: Int32 { get }
var CL_HAS_NAMED_VECTOR_FIELDS: Int32 { get }
var CL_HAS_HI_LO_VECTOR_FIELDS: Int32 { get }
struct cl_char2 {
  var s: (cl_char, cl_char)
  init(s s: (cl_char, cl_char))
  init()
}
struct cl_char4 {
  var s: (cl_char, cl_char, cl_char, cl_char)
  init(s s: (cl_char, cl_char, cl_char, cl_char))
  init()
}
typealias cl_char3 = cl_char4
struct cl_char8 {
  var s: (cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char)
  init(s s: (cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char))
  init()
}
struct cl_char16 {
  var s: (cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char)
  init(s s: (cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char, cl_char))
  init()
}
struct cl_uchar2 {
  var s: (cl_uchar, cl_uchar)
  init(s s: (cl_uchar, cl_uchar))
  init()
}
struct cl_uchar4 {
  var s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar)
  init(s s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar))
  init()
}
typealias cl_uchar3 = cl_uchar4
struct cl_uchar8 {
  var s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar)
  init(s s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar))
  init()
}
struct cl_uchar16 {
  var s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar)
  init(s s: (cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar, cl_uchar))
  init()
}
struct cl_short2 {
  var s: (cl_short, cl_short)
  init(s s: (cl_short, cl_short))
  init()
}
struct cl_short4 {
  var s: (cl_short, cl_short, cl_short, cl_short)
  init(s s: (cl_short, cl_short, cl_short, cl_short))
  init()
}
typealias cl_short3 = cl_short4
struct cl_short8 {
  var s: (cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short)
  init(s s: (cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short))
  init()
}
struct cl_short16 {
  var s: (cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short)
  init(s s: (cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short, cl_short))
  init()
}
struct cl_ushort2 {
  var s: (cl_ushort, cl_ushort)
  init(s s: (cl_ushort, cl_ushort))
  init()
}
struct cl_ushort4 {
  var s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort)
  init(s s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort))
  init()
}
typealias cl_ushort3 = cl_ushort4
struct cl_ushort8 {
  var s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort)
  init(s s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort))
  init()
}
struct cl_ushort16 {
  var s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort)
  init(s s: (cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort, cl_ushort))
  init()
}
struct cl_int2 {
  var s: (cl_int, cl_int)
  init(s s: (cl_int, cl_int))
  init()
}
struct cl_int4 {
  var s: (cl_int, cl_int, cl_int, cl_int)
  init(s s: (cl_int, cl_int, cl_int, cl_int))
  init()
}
typealias cl_int3 = cl_int4
struct cl_int8 {
  var s: (cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int)
  init(s s: (cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int))
  init()
}
struct cl_int16 {
  var s: (cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int)
  init(s s: (cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int, cl_int))
  init()
}
struct cl_uint2 {
  var s: (cl_uint, cl_uint)
  init(s s: (cl_uint, cl_uint))
  init()
}
struct cl_uint4 {
  var s: (cl_uint, cl_uint, cl_uint, cl_uint)
  init(s s: (cl_uint, cl_uint, cl_uint, cl_uint))
  init()
}
typealias cl_uint3 = cl_uint4
struct cl_uint8 {
  var s: (cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint)
  init(s s: (cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint))
  init()
}
struct cl_uint16 {
  var s: (cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint)
  init(s s: (cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint, cl_uint))
  init()
}
struct cl_long2 {
  var s: (cl_long, cl_long)
  init(s s: (cl_long, cl_long))
  init()
}
struct cl_long4 {
  var s: (cl_long, cl_long, cl_long, cl_long)
  init(s s: (cl_long, cl_long, cl_long, cl_long))
  init()
}
typealias cl_long3 = cl_long4
struct cl_long8 {
  var s: (cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long)
  init(s s: (cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long))
  init()
}
struct cl_long16 {
  var s: (cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long)
  init(s s: (cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long, cl_long))
  init()
}
struct cl_ulong2 {
  var s: (cl_ulong, cl_ulong)
  init(s s: (cl_ulong, cl_ulong))
  init()
}
struct cl_ulong4 {
  var s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong)
  init(s s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong))
  init()
}
typealias cl_ulong3 = cl_ulong4
struct cl_ulong8 {
  var s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong)
  init(s s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong))
  init()
}
struct cl_ulong16 {
  var s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong)
  init(s s: (cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong, cl_ulong))
  init()
}
struct cl_float2 {
  var s: (cl_float, cl_float)
  init(s s: (cl_float, cl_float))
  init()
}
struct cl_float4 {
  var s: (cl_float, cl_float, cl_float, cl_float)
  var v4: __cl_float4
  init(s s: (cl_float, cl_float, cl_float, cl_float))
  init(v4 v4: __cl_float4)
  init()
}
typealias cl_float3 = cl_float4
struct cl_float8 {
  var s: (cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float)
  var v4: (__cl_float4, __cl_float4)
  init(s s: (cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float))
  init(v4 v4: (__cl_float4, __cl_float4))
  init()
}
struct cl_float16 {
  var s: (cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float)
  var v4: (__cl_float4, __cl_float4, __cl_float4, __cl_float4)
  init(s s: (cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float, cl_float))
  init(v4 v4: (__cl_float4, __cl_float4, __cl_float4, __cl_float4))
  init()
}
struct cl_double2 {
  var s: (cl_double, cl_double)
  init(s s: (cl_double, cl_double))
  init()
}
struct cl_double4 {
  var s: (cl_double, cl_double, cl_double, cl_double)
  init(s s: (cl_double, cl_double, cl_double, cl_double))
  init()
}
typealias cl_double3 = cl_double4
struct cl_double8 {
  var s: (cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double)
  init(s s: (cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double))
  init()
}
struct cl_double16 {
  var s: (cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double)
  init(s s: (cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double, cl_double))
  init()
}
