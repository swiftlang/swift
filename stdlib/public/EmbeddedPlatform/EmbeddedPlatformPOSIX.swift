import Builtin

// POSIX dependencies
@_extern(c, "posix_memalign")
func posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: Int, _: Int) -> CInt

@_extern(c, "free")
func free(_ p: UnsafeMutableRawPointer?)

@_extern(c, "arc4random_buf")
func arc4random_buf(_ buf: UnsafeMutableRawPointer, _ nbytes: Int)

@_extern(c, "putchar")
func putchar(_: CInt) -> CInt

@_extern(c, "exit")
func exit(_: CInt)

@_extern(c, "malloc_type_malloc")
func malloc_type_malloc(_ : Int, _ : UInt64) -> UnsafeMutableRawPointer

@_extern(c, "malloc_type_posix_memalign")
func malloc_type_posix_memalign(_: UnsafeMutablePointer<UnsafeMutableRawPointer?>, _: Int, _: Int, _:UInt64) -> CInt

@_extern(c, "malloc_type_free")
func malloc_type_free(_ : UnsafeMutableRawPointer, _ : UInt64)

@inline(never)
private func clearMemory(pointer: UnsafeMutableRawPointer, numBytes: Int) {
  let bytePtr = unsafe pointer.assumingMemoryBound(to: UInt8.self)
  for i in 0..<numBytes {
    unsafe bytePtr[i] = 0
  }
}

// Implementations of the Embedded Swift Platform layer on top of the POSIX
// dependencies.

@export(interface)
@implementation @c
public func _swift_allocate(_ alignment: Int, _ size: Int, _ flags: SwiftAllocateFlags) -> UnsafeMutableRawPointer? {
  var pointer: UnsafeMutableRawPointer? = nil
  guard posix_memalign(&pointer, alignment, size) == 0 else {
    return nil
  }

  // Clear the memory if requested.
  if flags.contains(.zeroMemory), let pointer {
    clearMemory(pointer: pointer, numBytes: size)
  }

  return pointer
}

@export(interface)
@implementation @c
public func _swift_deallocate(_ pointer: UnsafeMutableRawPointer, _ alignment: Int, _ size: Int, _ flags: SwiftDeallocFlags) {
  free(pointer)
}

@export(interface)
@implementation @c
public func _swift_generateRandom(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
  arc4random_buf(buf, nbytes)
}

@export(interface)
@implementation @c
public func _swift_generateRandomHashSeed(_ buf: UnsafeMutableRawPointer, _ nbytes: Int) {
  arc4random_buf(buf, nbytes)
}

@export(interface)
@implementation @c
public func _swift_writeToStandardOutput(
  _ pointer: UnsafePointer<UInt8>?,
  _ count: Int
) -> Int {
  for unsafe char in unsafe UnsafeBufferPointer(start: pointer, count: count) {
    _ = putchar(CInt(char))
  }
  return count
}

@export(interface)
@implementation @c
public func _swift_exit(_ code: Int) {
  exit(CInt(code))
}

/// The human-readable prefix that precedes an error message, chosen by the
/// error kind held in the low 8 bits of `flags` (a `swift_error_kind_t` /
/// `SwiftErrorKind` value).
private func _reportErrorPrefix(_ flags: UInt64) -> StaticString {
  switch flags & 0xff {
  case 1: return "Precondition failed" // SwiftErrorKind.precondition
  case 2: return "Assertion failed"    // SwiftErrorKind.assertion
  default: return "Fatal error"        // SwiftErrorKind.fatal
  }
}

/// Writes a NUL-terminated static string to standard output.
private func _writeStaticString(_ string: StaticString) {
  string.withUTF8Buffer { buffer in
    _ = unsafe _swift_writeToStandardOutput(buffer.baseAddress, buffer.count)
  }
}

/// Writes a non-negative integer to standard output in decimal.
private func _writeInt(_ value: Int) {
  if value >= 10 { _writeInt(value / 10) }
  _ = putchar(CInt(UInt8(ascii: "0") + UInt8(value % 10)))
}

/// Prints a fatal error report to standard output, optionally prefixed by a
/// source location, e.g. "file:line: Fatal error: message". Output goes through
/// `putchar` (via `_swift_writeToStandardOutput`) so it doesn't allocate and
/// honors any platform-provided unbuffered `putchar`.
private func _reportError(
  prefix: StaticString,
  fileName: UnsafePointer<UInt8>?, fileNameCount: Int, line: Int,
  message: UnsafePointer<UInt8>?, messageCount: Int
) {
  // Optional "<file>:<line>: " source-location prefix.
  if let fileName, fileNameCount > 0 {
    _ = unsafe _swift_writeToStandardOutput(fileName, fileNameCount)
    _writeStaticString(":")
    _writeInt(line)
    _writeStaticString(": ")
  }

  // The error-kind prefix, followed by ": <message>" when a message is given.
  _writeStaticString(prefix)
  if let message, messageCount > 0 {
    _writeStaticString(": ")
    _ = unsafe _swift_writeToStandardOutput(message, messageCount)
  }
  _writeStaticString("\n")
}

@export(interface)
@implementation @c
public func _swift_reportError(
  _ message: UnsafePointer<UInt8>?,
  _ messageCount: Int,
  _ flags: UInt64
) -> Never {
  unsafe _reportError(
    prefix: _reportErrorPrefix(flags),
    fileName: nil, fileNameCount: 0, line: 0,
    message: message, messageCount: messageCount)
  Builtin.int_trap()
}

@export(interface)
@implementation @c
public func _swift_reportErrorAt(
  _ message: UnsafePointer<UInt8>?,
  _ messageCount: Int,
  _ fileName: UnsafePointer<UInt8>?,
  _ fileNameCount: Int,
  _ line: Int,
  _ flags: UInt64
) -> Never {
  unsafe _reportError(
    prefix: _reportErrorPrefix(flags),
    fileName: fileName, fileNameCount: fileNameCount, line: line,
    message: message, messageCount: messageCount)
  Builtin.int_trap()
}

#if SWIFT_STDLIB_HAS_MALLOC_TYPE
// `#available` calls the non-inlinable `_stdlib_isOSVersionAtLeast`, which
// has no body in the embedded stdlib; call the inlinable `_AEIC` entry
// point directly instead.
@inline(__always)
private func _isMallocTypeOSVersionAtLeast() -> Bool {
#if os(macOS)
  return Bool(_stdlib_isOSVersionAtLeast_AEIC(15._builtinWordValue, 0._builtinWordValue, 0._builtinWordValue))
#elseif os(iOS)
  return Bool(_stdlib_isOSVersionAtLeast_AEIC(17._builtinWordValue, 0._builtinWordValue, 0._builtinWordValue))
#elseif os(tvOS)
  return Bool(_stdlib_isOSVersionAtLeast_AEIC(17._builtinWordValue, 0._builtinWordValue, 0._builtinWordValue))
#elseif os(watchOS)
  return Bool(_stdlib_isOSVersionAtLeast_AEIC(10._builtinWordValue, 0._builtinWordValue, 0._builtinWordValue))
#else
  return false
#endif
}
#endif

@export(interface)
@implementation @c
public func _swift_typedAllocate(_ size: Int, _ alignMask: Int, _ flags: SwiftAllocateFlags, _ typeId: UInt64) -> UnsafeMutableRawPointer? {
  if (size == 0) {
    return unsafe _swift_typedAllocate(1, alignMask, flags, typeId)
  }

#if SWIFT_STDLIB_HAS_MALLOC_TYPE
  if _isMallocTypeOSVersionAtLeast() {
    // This check also forces "default" alignment to use malloc_memalign().
    let MALLOC_ALIGN_MASK = 15
    if (alignMask <= MALLOC_ALIGN_MASK) {
      return unsafe malloc_type_malloc(size, typeId);
    } else {
      var alignment: Int
      if alignMask == -1 {
        let _swift_MinAllocationAlignment = 16
        alignment = _swift_MinAllocationAlignment
      } else {
        alignment = alignMask + 1
      }

      // Do not use malloc_type_aligned_alloc() here, because we want this
      // to work if `size` is not an integer multiple of `alignment`, which
      // was a requirement of the latter in C11 (but not C17 and later).
      var pointer: UnsafeMutableRawPointer? = nil
      guard unsafe malloc_type_posix_memalign(&pointer, alignment, size, typeId) == 0 else {
        return nil
      }
      return pointer
    }
  }
#endif
  return unsafe swift_slowAlloc(size, alignMask)
}

@export(interface)
@implementation @c
public func _swift_typedDeallocate(_ pointer: UnsafeMutableRawPointer, _ size: Int, _ alignMask: Int, _ flags: SwiftDeallocFlags, _ typeId: UInt64) {
#if SWIFT_STDLIB_HAS_MALLOC_TYPE
  if _isMallocTypeOSVersionAtLeast() {
    unsafe malloc_type_free(pointer, typeId);
    return
  }
#endif
  swift_slowDealloc(pointer, size, alignMask);
}
