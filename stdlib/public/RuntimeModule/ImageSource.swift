//===--- ImageSource.swift - A place from which to read image data --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines ImageSource, which tells us where to look for image data.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

enum ImageSourceError: Error {
  case outOfBoundsRead
  case posixError(Int32)
}

struct ImageSource {

  private class Storage {
    /// Says how we allocated the buffer.
    private enum MemoryBufferKind {
      /// Currently empty
      case empty

      /// Allocated with UnsafeRawBufferPointer.allocate()
      case allocated(Int)

      /// Allocated by mapping memory with mmap() or similar
      case mapped

      /// A reference to a subordinate storage
      case substorage(Storage)

      /// Not allocated (probably points to a loaded image)
      case unowned
    }

    private var kind: MemoryBufferKind

    /// The pointer to the actual memory
    private(set) var bytes: UnsafeRawBufferPointer!

    /// Gets a mutable pointer to the actual memory
    var mutableBytes: UnsafeMutableRawBufferPointer {
      guard case let .allocated(count) = kind else {
        fatalError("attempted to get mutable reference to immutable ImageSource")
      }
      return UnsafeMutableRawBufferPointer(
        mutating: UnsafeRawBufferPointer(rebasing: bytes[0..<count])
      )
    }

    /// Gets a mutable pointer to the unused space
    var unusedBytes: UnsafeMutableRawBufferPointer {
      guard case let .allocated(count) = kind else {
        fatalError("attempted to get mutable reference to immutable ImageSource")
      }
      return UnsafeMutableRawBufferPointer(
        mutating: UnsafeRawBufferPointer(rebasing: bytes[count...])
      )
    }

    /// Return the number of bytes in this ImageSource
    var count: Int {
      switch kind {
        case .empty:
          return 0
        case let .allocated(count):
          return count
        case .mapped, .substorage, .unowned:
          return bytes.count
      }
    }

    @inline(__always)
    private func _rangeCheck(_ ndx: Int) {
      if ndx < 0 || ndx >= count {
        fatalError("ImageSource access out of range")
      }
    }

    init() {
      self.kind = .empty
      self.bytes = nil
    }

    init(unowned buffer: UnsafeRawBufferPointer) {
      self.kind = .unowned
      self.bytes = buffer
    }

    init(mapped buffer: UnsafeRawBufferPointer) {
      self.kind = .mapped
      self.bytes = buffer
    }

    init(allocated buffer: UnsafeMutableRawBufferPointer, count: Int? = nil) {
      self.kind = .allocated(count ?? buffer.count)
      self.bytes = UnsafeRawBufferPointer(buffer)
    }

    convenience init(capacity: Int, alignment: Int = 0x4000) {
      self.init(allocated: UnsafeMutableRawBufferPointer.allocate(
                  byteCount: capacity,
                  alignment: 0x1000
                ),
                count: 0)
    }

    init(parent: Storage, range: Range<Int>) {
      let chunk = UnsafeRawBufferPointer(rebasing: parent.bytes[range])

      self.kind = .substorage(parent)
      self.bytes = chunk
    }

    convenience init(path: String) throws {
      let fd = open(path, O_RDONLY, 0)
      if fd < 0 {
        throw ImageSourceError.posixError(errno)
      }
      defer { close(fd) }
      let size = lseek(fd, 0, SEEK_END)
      if size < 0 {
        throw ImageSourceError.posixError(errno)
      }
      let base = mmap(nil, Int(size), PROT_READ, MAP_FILE|MAP_PRIVATE, fd, 0)
      if base == nil || base! == UnsafeRawPointer(bitPattern: -1)! {
        throw ImageSourceError.posixError(errno)
      }

      self.init(mapped: UnsafeRawBufferPointer(
                  start: base, count: Int(size)))
    }

    deinit {
      switch kind {
        case .allocated:
          mutableBytes.deallocate()
        case .mapped:
          munmap(UnsafeMutableRawPointer(mutating: bytes.baseAddress),
                 bytes.count)
        case .substorage, .unowned, .empty:
          break
      }
    }

    /// Subscripting (read-only, for subranges)
    subscript(range: Range<Int>) -> Storage {
      return Storage(parent: self, range: range)
    }

    /// Resize the buffer; only supported for allocated or empty storage
    func resize(newSize: Int) -> UnsafeMutableRawBufferPointer {
      let newBuffer = UnsafeMutableRawBufferPointer.allocate(
        byteCount: newSize,
        alignment: 0x1000
      )
      switch kind {
        case .empty:
          kind = .allocated(0)
        case let .allocated(count):
          assert(newSize >= count)

          let oldPart = UnsafeMutableRawBufferPointer(
            rebasing: newBuffer[0..<count]
          )
          oldPart.copyMemory(from: bytes)
          mutableBytes.deallocate()
          kind = .allocated(count)
        default:
          fatalError("Cannot resize immutable image source storage")
      }

      bytes = UnsafeRawBufferPointer(newBuffer)

      return newBuffer
    }

    /// Make sure the buffer has at least a certain number of bytes;
    /// only supported for allocated or empty storage.
    func requireAtLeast(byteCount: Int) -> UnsafeMutableRawBufferPointer {
      let capacity: Int
      switch kind {
        case .empty:
          capacity = 0
        case .allocated:
          capacity = bytes.count
        default:
          fatalError("Cannot resize immutable image source storage")
      }

      if capacity >= byteCount {
        return mutableBytes
      }

      let extra = byteCount - capacity

      let increment: Int
      if capacity < 1048576 {
        let roundedExtra = (extra + 0xffff) & ~0xffff
        increment = max(roundedExtra, capacity)
      } else {
        let roundedExtra = (extra + 0xfffff) & ~0xfffff
        let topBit = capacity.bitWidth - capacity.leadingZeroBitCount
        increment = max(roundedExtra, 1048576 * (topBit - 20))
      }

      return resize(newSize: capacity + increment)
    }

    /// Mark a number of bytes in the mutable buffer as in use.  This is
    /// used when passing `unusedBytes` to some other code that fills in
    /// part of the buffer.
    func used(bytes: Int) {
      guard bytes >= 0 else {
        fatalError("Bytes should not be less than zero")
      }
      guard case let .allocated(count) = kind else {
        fatalError("Cannot append to immutable image source storage")
      }
      guard mutableBytes.count - count <= bytes else {
        fatalError("Buffer overrun detected")
      }
      kind = .allocated(count + bytes)
    }

    /// Append bytes to the mutable buffer; this is only supported for
    /// allocated or empty storage.
    func append(bytes toAppend: UnsafeRawBufferPointer) {
      // Short circuit, otherwise we get in a muddle in requireAtLeast()
      if toAppend.count == 0 {
        return
      }

      let newCount = count + toAppend.count

      let mutableBytes = requireAtLeast(byteCount: newCount)

      guard case let .allocated(count) = kind else {
        fatalError("Cannot append to immutable image source storage")
      }

      let dest = UnsafeMutableRawBufferPointer(
        rebasing: mutableBytes[count..<newCount]
      )
      dest.copyMemory(from: toAppend)
      kind = .allocated(newCount)
    }
  }

  /// The storage holding the image data.
  private var storage: Storage

  /// The number of bytes of data this ImageSource holds.
  var count: Int { return storage.count }

  /// The memory holding the image data.
  var bytes: UnsafeRawBufferPointer { return storage.bytes }

  /// A mutable refernece to the image data (only for allocated storage)
  var mutableBytes: UnsafeMutableRawBufferPointer { return storage.mutableBytes }

  /// A mutable reference to unused bytes in the storage
  var unusedBytes: UnsafeMutableRawBufferPointer { return storage.unusedBytes }

  /// Says whether we are looking at a loaded (i.e. with ld.so or dyld) image.
  private(set) var isMappedImage: Bool

  /// If this ImageSource knows its path, this will be non-nil.
  private(set) var path: String?

  /// Private initialiser, not for general use
  private init(storage: Storage, isMappedImage: Bool, path: String?) {
    self.storage = storage
    self.isMappedImage = isMappedImage
    self.path = path
  }

  /// Initialise an empty storage
  init(isMappedImage: Bool, path: String? = nil) {
    self.init(storage: Storage(), isMappedImage: isMappedImage, path: path)
  }

  /// Initialise from unowned storage
  init(unowned: UnsafeRawBufferPointer, isMappedImage: Bool, path: String? = nil) {
    self.init(storage: Storage(unowned: unowned),
              isMappedImage: isMappedImage, path: path)
  }

  /// Initialise from mapped storage
  init(mapped: UnsafeRawBufferPointer, isMappedImage: Bool, path: String? = nil) {
    self.init(storage: Storage(mapped: mapped),
              isMappedImage: isMappedImage, path: path)
  }

  /// Initialise with a specified capacity
  init(capacity: Int, isMappedImage: Bool, path: String? = nil) {
    self.init(storage: Storage(capacity: capacity),
              isMappedImage: isMappedImage, path: path)
  }

  /// Initialise with a mapped file
  init(path: String) throws {
    self.init(storage: try Storage(path: path),
              isMappedImage: false, path: path)
  }

  /// Get a sub-range of this ImageSource as an ImageSource
  subscript(range: Range<Address>) -> ImageSource {
    let intRange = Int(range.lowerBound)..<Int(range.upperBound)
    return ImageSource(storage: storage[intRange],
                       isMappedImage: isMappedImage,
                       path: path)
  }

  /// Mark unused bytes in the storage as used
  func used(bytes: Int) {
    storage.used(bytes: bytes)
  }

  /// Append bytes to an empty or allocated storage
  func append(bytes toAppend: UnsafeRawBufferPointer) {
    storage.append(bytes: toAppend)
  }
}

// MemoryReader support
extension ImageSource: MemoryReader {
  public func fetch(from address: Address,
                    into buffer: UnsafeMutableRawBufferPointer) throws {
    let offset = Int(address)
    guard bytes.count >= buffer.count &&
            offset <= bytes.count - buffer.count else {
      throw ImageSourceError.outOfBoundsRead
    }
    buffer.copyMemory(from: UnsafeRawBufferPointer(
                        rebasing: bytes[offset..<offset + buffer.count]))
  }

  public func fetch<T>(from address: Address, as type: T.Type) throws -> T {
    let size = MemoryLayout<T>.size
    let offset = Int(address)
    guard offset <= bytes.count - size else {
      throw ImageSourceError.outOfBoundsRead
    }
    return bytes.loadUnaligned(fromByteOffset: offset, as: type)
  }

  public func fetchString(from address: Address) throws -> String? {
    let offset = Int(address)
    let len = strnlen(bytes.baseAddress! + offset, bytes.count - offset)
    let stringBytes = bytes[offset..<offset+len]
    return String(decoding: stringBytes, as: UTF8.self)
  }

  public func fetchString(from address: Address, length: Int) throws -> String? {
    let offset = Int(address)
    let stringBytes = bytes[offset..<offset+length]
    return String(decoding: stringBytes, as: UTF8.self)
  }
}

/// Used as a cursor by the DWARF code
struct ImageSourceCursor {
  typealias Address = ImageSource.Address
  typealias Size = ImageSource.Size

  var source: ImageSource
  var pos: Address

  init(source: ImageSource, offset: Address = 0) {
    self.source = source
    self.pos = offset
  }

  mutating func read(into buffer: UnsafeMutableRawBufferPointer) throws {
    try source.fetch(from: pos, into: buffer)
    pos += Size(buffer.count)
  }

  mutating func read<T>(into buffer: UnsafeMutableBufferPointer<T>) throws {
    try source.fetch(from: pos, into: buffer)
    pos += Size(MemoryLayout<T>.stride * buffer.count)
  }

  mutating func read<T>(into pointer: UnsafeMutablePointer<T>) throws {
    try source.fetch(from: pos, into: pointer)
    pos += Size(MemoryLayout<T>.stride)
  }

  mutating func read<T>(as type: T.Type) throws -> T {
    let result = try source.fetch(from: pos, as: type)
    pos += Size(MemoryLayout<T>.stride)
    return result
  }

  mutating func read<T>(count: Int, as type: T.Type) throws -> [T] {
    let result = try source.fetch(from: pos, count: count, as: type)
    pos += Size(MemoryLayout<T>.stride * count)
    return result
  }

  mutating func readString() throws -> String? {
    guard let result = try source.fetchString(from: pos) else {
      return nil
    }
    pos += Size(result.utf8.count + 1) // +1 for the NUL
    return result
  }

  mutating func readString(length: Int) throws -> String? {
    guard let result = try source.fetchString(from: pos, length: length) else {
      return nil
    }
    pos += Size(length)
    return result
  }
}
