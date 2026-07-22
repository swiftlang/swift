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

@export(interface)
@implementation @c
public func _swift_typedAllocate(_ size: Int, _ alignMask: Int, _ flags: SwiftAllocateFlags, _ typeId: UInt64) -> UnsafeMutableRawPointer? {
  if (size == 0) {
    return unsafe _swift_typedAllocate(1, alignMask, flags, typeId)
  }

#if SWIFT_STDLIB_HAS_MALLOC_TYPE
  var pointer: UnsafeMutableRawPointer? = nil
  if #available(macOS 15, iOS 17, tvOS 17, watchOS 10, *) {
    // This check also forces "default" alignment to use malloc_memalign().
    if (alignMask <= MALLOC_ALIGN_MASK) {
      return unsafe malloc_type_malloc(size, typeId);
    } else {
      if alignMask == -1 {
        alignment = _swift_MinAllocationAlignment
      } else {
        alignment = alignMask + 1
      }

      // Do not use malloc_type_aligned_alloc() here, because we want this
      // to work if `size` is not an integer multiple of `alignment`, which
      // was a requirement of the latter in C11 (but not C17 and later).
      guard unsafe malloc_type_posix_memalign(&pointer, alignment, size, typeId) == 0 else {
        return nil
      }
    }
  }

  return pointer
#else
  return unsafe swift_slowAlloc(size, alignMask)
#endif
}
