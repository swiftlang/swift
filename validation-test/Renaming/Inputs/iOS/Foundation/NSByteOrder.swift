
var NS_UnknownByteOrder: Int { get }
var NS_LittleEndian: Int { get }
var NS_BigEndian: Int { get }
@discardableResult
func NSHostByteOrder() -> Int
@discardableResult
func NSSwapShort(_ inv: UInt16) -> UInt16
@discardableResult
func NSSwapInt(_ inv: UInt32) -> UInt32
@discardableResult
func NSSwapLong(_ inv: UInt) -> UInt
@discardableResult
func NSSwapLongLong(_ inv: UInt64) -> UInt64
@discardableResult
func NSSwapBigShortToHost(_ x: UInt16) -> UInt16
@discardableResult
func NSSwapBigIntToHost(_ x: UInt32) -> UInt32
@discardableResult
func NSSwapBigLongToHost(_ x: UInt) -> UInt
@discardableResult
func NSSwapBigLongLongToHost(_ x: UInt64) -> UInt64
@discardableResult
func NSSwapHostShortToBig(_ x: UInt16) -> UInt16
@discardableResult
func NSSwapHostIntToBig(_ x: UInt32) -> UInt32
@discardableResult
func NSSwapHostLongToBig(_ x: UInt) -> UInt
@discardableResult
func NSSwapHostLongLongToBig(_ x: UInt64) -> UInt64
@discardableResult
func NSSwapLittleShortToHost(_ x: UInt16) -> UInt16
@discardableResult
func NSSwapLittleIntToHost(_ x: UInt32) -> UInt32
@discardableResult
func NSSwapLittleLongToHost(_ x: UInt) -> UInt
@discardableResult
func NSSwapLittleLongLongToHost(_ x: UInt64) -> UInt64
@discardableResult
func NSSwapHostShortToLittle(_ x: UInt16) -> UInt16
@discardableResult
func NSSwapHostIntToLittle(_ x: UInt32) -> UInt32
@discardableResult
func NSSwapHostLongToLittle(_ x: UInt) -> UInt
@discardableResult
func NSSwapHostLongLongToLittle(_ x: UInt64) -> UInt64
struct NSSwappedFloat {
  var v: UInt32
  init()
  init(v v: UInt32)
}
struct NSSwappedDouble {
  var v: UInt64
  init()
  init(v v: UInt64)
}
@discardableResult
func NSConvertHostFloatToSwapped(_ x: Float) -> NSSwappedFloat
@discardableResult
func NSConvertSwappedFloatToHost(_ x: NSSwappedFloat) -> Float
@discardableResult
func NSConvertHostDoubleToSwapped(_ x: Double) -> NSSwappedDouble
@discardableResult
func NSConvertSwappedDoubleToHost(_ x: NSSwappedDouble) -> Double
@discardableResult
func NSSwapFloat(_ x: NSSwappedFloat) -> NSSwappedFloat
@discardableResult
func NSSwapDouble(_ x: NSSwappedDouble) -> NSSwappedDouble
@discardableResult
func NSSwapBigDoubleToHost(_ x: NSSwappedDouble) -> Double
@discardableResult
func NSSwapBigFloatToHost(_ x: NSSwappedFloat) -> Float
@discardableResult
func NSSwapHostDoubleToBig(_ x: Double) -> NSSwappedDouble
@discardableResult
func NSSwapHostFloatToBig(_ x: Float) -> NSSwappedFloat
@discardableResult
func NSSwapLittleDoubleToHost(_ x: NSSwappedDouble) -> Double
@discardableResult
func NSSwapLittleFloatToHost(_ x: NSSwappedFloat) -> Float
@discardableResult
func NSSwapHostDoubleToLittle(_ x: Double) -> NSSwappedDouble
@discardableResult
func NSSwapHostFloatToLittle(_ x: Float) -> NSSwappedFloat
