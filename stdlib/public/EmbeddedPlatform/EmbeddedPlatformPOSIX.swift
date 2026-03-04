// POSIX dependencies
@_extern(c, "posix_memalign")
func posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: Int, _: Int) -> CInt

@_extern(c, "free")
func free(_ p: UnsafeMutableRawPointer?)

@_extern(c, "arc4random_buf")
func arc4random_buf(_ buf: UnsafeMutableRawPointer, _ nbytes: Int)

@_extern(c, "putchar")
func putchar(_: CInt) -> CInt

// Implementations of the Embedded Swift Platform layer on top of the POSIX dependencies.
@implementation @c
public func _swift_alignedAllocate(_ pointer: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _ alignment: Int, _ size: Int) -> CInt {
  posix_memalign(pointer, alignment, size)
}

@implementation @c
public func _swift_alignedFree(_ p: UnsafeMutableRawPointer, _ alignment: Int, _ size: Int) {
  free(p)
}

@implementation @c
public func _swift_generateRandom(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
  arc4random_buf(buf, nbytes)
}

@implementation @c
public func _swift_generateRandomHashSeed(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
  arc4random_buf(buf, nbytes)
}

@implementation @c
public func _swift_writeCharToStandardOutput(_ c: CInt) -> CInt {
  putchar(c)
}
