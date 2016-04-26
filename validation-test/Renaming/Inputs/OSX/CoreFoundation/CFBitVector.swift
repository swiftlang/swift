
typealias CFBit = UInt32
class CFBitVector {
}
class CFMutableBitVector {
}
@discardableResult
func CFBitVectorGetTypeID() -> CFTypeID
@discardableResult
func CFBitVectorCreate(_ allocator: CFAllocator!, _ bytes: UnsafePointer<UInt8>!, _ numBits: CFIndex) -> CFBitVector!
@discardableResult
func CFBitVectorCreateCopy(_ allocator: CFAllocator!, _ bv: CFBitVector!) -> CFBitVector!
@discardableResult
func CFBitVectorCreateMutable(_ allocator: CFAllocator!, _ capacity: CFIndex) -> CFMutableBitVector!
@discardableResult
func CFBitVectorCreateMutableCopy(_ allocator: CFAllocator!, _ capacity: CFIndex, _ bv: CFBitVector!) -> CFMutableBitVector!
@discardableResult
func CFBitVectorGetCount(_ bv: CFBitVector!) -> CFIndex
@discardableResult
func CFBitVectorGetCountOfBit(_ bv: CFBitVector!, _ range: CFRange, _ value: CFBit) -> CFIndex
@discardableResult
func CFBitVectorContainsBit(_ bv: CFBitVector!, _ range: CFRange, _ value: CFBit) -> Bool
@discardableResult
func CFBitVectorGetBitAtIndex(_ bv: CFBitVector!, _ idx: CFIndex) -> CFBit
func CFBitVectorGetBits(_ bv: CFBitVector!, _ range: CFRange, _ bytes: UnsafeMutablePointer<UInt8>!)
@discardableResult
func CFBitVectorGetFirstIndexOfBit(_ bv: CFBitVector!, _ range: CFRange, _ value: CFBit) -> CFIndex
@discardableResult
func CFBitVectorGetLastIndexOfBit(_ bv: CFBitVector!, _ range: CFRange, _ value: CFBit) -> CFIndex
func CFBitVectorSetCount(_ bv: CFMutableBitVector!, _ count: CFIndex)
func CFBitVectorFlipBitAtIndex(_ bv: CFMutableBitVector!, _ idx: CFIndex)
func CFBitVectorFlipBits(_ bv: CFMutableBitVector!, _ range: CFRange)
func CFBitVectorSetBitAtIndex(_ bv: CFMutableBitVector!, _ idx: CFIndex, _ value: CFBit)
func CFBitVectorSetBits(_ bv: CFMutableBitVector!, _ range: CFRange, _ value: CFBit)
func CFBitVectorSetAllBits(_ bv: CFMutableBitVector!, _ value: CFBit)
