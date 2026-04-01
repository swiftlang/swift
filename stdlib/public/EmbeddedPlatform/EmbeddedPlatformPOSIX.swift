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

@implementation @c
public func _swift_typedAllocate(_ buf: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _ size: Int, _ alignMask: Int, _ typeId: UInt64) {
  if (size == 0) {
    return unsafe _swift_typedAllocate(buf, 1, alignMask, typeId)
  }

#if SWIFT_STDLIB_HAS_MALLOC_TYPE
  if #available(macOS 15, iOS 17, tvOS 17, watchOS 10, *) {
    // This check also forces "default" alignment to use malloc_memalign().
    if (alignMask <= MALLOC_ALIGN_MASK) {
      buf.pointee = unsafe malloc_type_malloc(size, typeId);
    } else {
      if alignMask == -1 {
        alignment = _swift_MinAllocationAlignment
      } else {
        alignment = alignMask + 1
      }

      // Do not use malloc_type_aligned_alloc() here, because we want this
      // to work if `size` is not an integer multiple of `alignment`, which
      // was a requirement of the latter in C11 (but not C17 and later).
      let err = unsafe malloc_type_posix_memalign(buf, alignment, size, typeId)
      if (err != 0) {
        buf.pointee = nil
      }
    }
  }
#else
  buf.pointee = unsafe swift_slowAlloc(size, alignMask)
#endif
}
