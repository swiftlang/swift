//===--- MultiStreamFile.swift - PDB support for Swift --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Provides support for reading the multi-stream-file container format on
// which PDB is based.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import WinSDK
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif

internal import BacktracingImpl.OS.Libc
internal import BacktracingImpl.ImageFormats.PDB

enum MultiStreamFileError: Error {
  #if os(Windows)
  case readFailed(error: UInt32)
  #else
  case readFailed(errno: CInt)
  #endif
  case readOffEndOfStream
}

class MultiStreamFile {
  #if os(Windows)
  typealias FileHandle = HANDLE
  #else
  typealias FileHandle = CInt
  #endif

  var fileHandle: FileHandle

  struct StreamInfo {
    var size: Int
    var pages: [Int]

    var valid: Bool { return size != 0xffffffff }
  }

  enum Kind {
    case v2
    case v7
  }

  var kind: Kind
  var pageSize: Int
  var freePageMapPage: Int
  var pageCount: Int
  var streams: [StreamInfo]

  var pageCache: [Int:UnsafeMutableRawBufferPointer]

  convenience init?(path: String) {
    #if os(Windows)
    guard let handle: HANDLE =
            (path.withCString(encodedAs: UTF16.self) { lpwszPath in
               CreateFileW(lpwszPath,
                           GENERIC_READ,
                           DWORD(FILE_SHARE_READ),
                           nil,
                           DWORD(OPEN_EXISTING),
                           DWORD(FILE_ATTRIBUTE_NORMAL),
                           nil)
             })
    else {
      return nil
    }
    if handle == INVALID_HANDLE_VALUE {
      return nil
    }
    #else
    let handle: CInt = _swift_open(path, O_RDONLY, 0)
    if handle == -1 {
      return nil
    }
    #endif
    self.init(fileHandle: handle)
  }

  init?(fileHandle: FileHandle) {
    self.fileHandle = fileHandle

    // Read the start of the file to see what kind it is
    let theKind = withUnsafeTemporaryAllocation(of: CChar.self,
                                                capacity: 64) {
      (buffer: UnsafeMutableBufferPointer<CChar>) -> Kind? in

      let count = safe_read(fileHandle, 0, buffer.baseAddress!, buffer.count)

      if count < 0x1e {
        return nil
      }

      if count >= 0x2c && pdb_is_v2(buffer.baseAddress!) {
        return .v2
      }
      if pdb_is_v7(buffer.baseAddress!) {
        return .v7
      }

      return nil
    }

    guard let theKind else {
      return nil
    }

    self.kind = theKind
    self.streams = []
    self.pageCache = [:]

    var streamTable: StreamInfo

    switch kind {
      case .v2:
        let hdr =
          withUnsafeTemporaryAllocation(of: MSF_HDR.self, capacity: 1) {
            (buffer: UnsafeMutableBufferPointer<MSF_HDR>) -> MSF_HDR? in

            let count = safe_read(fileHandle, 0, buffer.baseAddress!,
                                  MemoryLayout<MSF_HDR>.size)

            if count != MemoryLayout<MSF_HDR>.size {
              return nil
            }

            return buffer[0]
          }

        guard let hdr else {
          return nil
        }

        pageSize = Int(hdr.cbPg)
        freePageMapPage = Int(hdr.pnFpm)
        pageCount = Int(hdr.pnMac)

        let streamTablePageCount = (Int(hdr.siSt.cb) + pageSize - 1) / pageSize
        let pages =
          withUnsafeTemporaryAllocation(of: PN.self,
                                        capacity: streamTablePageCount) {
            (buffer: UnsafeMutableBufferPointer<PN>) -> Array<Int>? in
            let byteCount = MemoryLayout<PN>.stride
              * Int(streamTablePageCount)
            let count = safe_read(fileHandle, MemoryLayout<MSF_HDR>.size,
                                  buffer.baseAddress!, byteCount)

            if count != byteCount {
              return nil
            }

            return buffer.map { Int($0) }
          }

        guard let pages else {
          return nil
        }

        streamTable = StreamInfo(size: Int(hdr.siSt.cb),
                                 pages: pages)
      case .v7:
        let hdr =
          withUnsafeTemporaryAllocation(of: BIGMSF_HDR.self, capacity: 1) {
            (buffer: UnsafeMutableBufferPointer<BIGMSF_HDR>) -> BIGMSF_HDR? in

            let count = safe_read(fileHandle, 0,
                                  buffer.baseAddress!,
                                  MemoryLayout<BIGMSF_HDR>.size)

            if count != MemoryLayout<BIGMSF_HDR>.size {
              return nil
            }

            return buffer[0]
          }

        guard let hdr else {
          return nil
        }

        pageSize = Int(hdr.cbPg)
        freePageMapPage = Int(hdr.pnFpm)
        pageCount = Int(hdr.pnMac)

        let streamTablePageCount = (Int(hdr.siSt.cb) + pageSize - 1) / pageSize
        let indirectTableSize = MemoryLayout<UPN>.stride * streamTablePageCount
        let indirectPageCount = (indirectTableSize + pageSize - 1) / pageSize

        // The stream table's page numbers are stored indirectly here; the
        // array in the header is an array of pages that contain the page
        // numbers, rather than an array of page numbers for the stream table.

        let indirectPages =
          withUnsafeTemporaryAllocation(of: UPN.self,
                                        capacity: indirectPageCount) {
            (buffer: UnsafeMutableBufferPointer<UPN>) -> Array<Int>? in
            let byteCount = MemoryLayout<UPN>.stride
              * Int(indirectPageCount)
            let count = safe_read(fileHandle, MemoryLayout<BIGMSF_HDR>.size,
                                  buffer.baseAddress!, byteCount)

            if count != byteCount {
              return nil
            }

            return buffer.map { Int($0) }
          }

        guard let indirectPages else {
          return nil
        }

        var pages: [Int] = []

        streamTable = StreamInfo(size: Int(hdr.siSt.cb),
                                 pages: pages)

        do {
          var remaining = streamTablePageCount
          for page in indirectPages {
            let chunkSize = min(remaining, pageSize / MemoryLayout<UPN>.stride)

            try with(page: page) {
              let pageNumbers = Span<UPN>(_bytes: $0)

              for n in 0..<chunkSize {
                pages.append(Int(pageNumbers[n]))
              }
            }

            remaining -= chunkSize
          }
        } catch {
          return nil
        }

        streamTable.pages = pages
    }

    streams.append(streamTable)

    let kStreamTable = -1

    do {
      var offset = 4

      // The stream table starts with a count of the number of streams
      let streamCount: Int
      switch kind {
        case .v2:
          streamCount = Int(
            try read(stream: kStreamTable, offset: 0, as: SN.self)
          )

          // This is followed by an array of SI_PERSIST structures, one per
          // stream
          for _ in 0..<streamCount {
            let si = try read(stream: kStreamTable, offset: offset, as: SI_PERSIST.self)

            streams.append(StreamInfo(size: Int(si.cb), pages: []))

            offset += MemoryLayout<SI_PERSIST>.stride
          }

        case .v7:
          streamCount = Int(
            try read(stream: kStreamTable, offset: 0, as: UNSN.self)
          )

          // This is followed by an array of byte counts, one per stream
          for _ in 0..<streamCount {
            let cb = try read(stream: kStreamTable, offset: offset, as: CB.self)

            streams.append(StreamInfo(size: Int(cb), pages: []))

            offset += MemoryLayout<CB>.stride
          }
      }

      for n in 1..<streams.count {
        if !streams[n].valid {
          continue
        }

        let pages = (streams[n].size + pageSize - 1) / pageSize

        if pages > 0 {
          streams[n].pages = (try read(stream: kStreamTable,
                                       offset: offset,
                                       as: PN32.self,
                                       count: pages,
                                       mustRead: true)).map { Int($0) }

          offset += pages * MemoryLayout<PN32>.stride
        }
      }

    } catch {
      return nil
    }
  }

  deinit {
    for (_, data) in pageCache {
      data.deallocate()
    }
    #if os(Windows)
    CloseHandle(fileHandle)
    #else
    close(fileHandle)
    #endif
  }

  func with<R>(
    page: Int,
    body: (RawSpan) throws -> R
  ) throws -> R {
    if let data = pageCache[page] {
      return try body(data.bytes)
    }

    let data = UnsafeMutableRawBufferPointer.allocate(byteCount: self.pageSize,
                                                      alignment: self.pageSize)
    let count = safe_read(fileHandle, page * self.pageSize,
                          data.baseAddress!, data.count)

    if count != data.count {
      #if os(Windows)
      let err = GetLastError()

      throw MultiStreamFileError.readFailed(error: UInt32(err))
      #else
      let err = errno

      throw MultiStreamFileError.readFailed(errno: err)
      #endif
    }

    pageCache[page] = data

    return try body(data.bytes)
  }

  func open(stream: Int) -> Stream? {
    if stream + 1 >= streams.count || stream < 0 {
      return nil
    }
    return Stream(parent: self, stream: stream)
  }

  struct Stream {
    private var parent: MultiStreamFile
    private var stream: Int
    private(set) var offset: Int = 0

    internal init(parent: MultiStreamFile, stream: Int) {
      self.parent = parent
      self.stream = stream
    }

    var size: Int {
      return parent.sizeOf(stream: stream)
    }

    var atEnd: Bool {
      return offset >= size
    }

    mutating func read<T>(as type: T.Type) throws -> T {
      let result = try parent.read(stream: stream, offset: offset, as: type)
      offset += MemoryLayout<T>.size
      return result
    }

    mutating func read<T>(as type: T.Type, count: Int) throws -> [T] {
      let result = try parent.read(stream: stream, offset: offset,
                                   as: type, count: count)
      offset += MemoryLayout<T>.stride * result.count
      return result
    }

    mutating func readString() throws -> String {
      var bytesRead = 0
      let result = try parent.readString(stream: stream, offset: offset,
                                         bytesRead: &bytesRead)
      offset += bytesRead
      return result
    }

    func pread<T>(from offset: Int, as type: T.Type) throws -> T {
      return try parent.read(stream: stream, offset: offset, as: type)
    }

    func pread<T>(
      from offset: Int, as type: T.Type, count: Int
    ) throws -> [T] {
      return try parent.read(stream: stream, offset: offset,
                             as: type, count: count)
    }

    func preadString(from offset: Int) throws -> String {
      var bytesRead = 0
      let result = try parent.readString(stream: stream, offset: offset,
                                         bytesRead: &bytesRead)
      return result
    }

    enum RelativeTo {
      case start
      case current
      case end
    }

    mutating func seek(
      offset: Int,
      relativeTo: RelativeTo = .start
    ) -> Int {
      var newOffset: Int
      switch relativeTo {
        case .start:
          newOffset = offset
        case .current:
          newOffset = self.offset + offset
        case .end:
          newOffset = size + offset
      }
      if newOffset < 0 {
        newOffset = 0
      } else if newOffset > size {
        newOffset = size
      }
      self.offset = newOffset
      return newOffset
    }

    mutating func align(toMultiple alignment: Int) -> Int {
      let extra = self.offset % alignment
      if extra == 0 {
        return self.offset
      }
      var newOffset = self.offset + alignment - extra
      if newOffset > size {
        newOffset = size
      }
      self.offset = newOffset
      return newOffset
    }
  }

  func sizeOf(stream: Int) -> Int {
    return streams[stream + 1].size
  }

  func read<T>(stream: Int, offset: Int, as type: T.Type) throws -> T {
    return try read(
      stream: stream, offset: offset, as: type, count: 1, mustRead: true
    )[0]
  }

  func read<T>(
    stream: Int, offset: Int, as type: T.Type, count: Int,
    mustRead: Bool = false
  ) throws -> [T] {
    let streamInfo = streams[stream + 1]
    var pageNdx = offset / pageSize
    var offsetInPage = offset - pageNdx * pageSize
    var theCount = count

    if offset > streamInfo.size {
      theCount = 0
    } else {
      let remaining = streamInfo.size - offset
      if MemoryLayout<T>.stride * count <= remaining {
        theCount = count
      } else if mustRead {
        throw MultiStreamFileError.readOffEndOfStream
      } else {
        theCount = remaining / MemoryLayout<T>.stride
      }
    }

    return try withUnsafeTemporaryAllocation(of: T.self, capacity: theCount) {
      let rawBuffer = UnsafeMutableRawBufferPointer($0)
      var bytesLeft = MemoryLayout<T>.stride * $0.count
      var offset = 0

      while bytesLeft > 0 {
        let chunkSize = min(bytesLeft, pageSize - offsetInPage)

        let page = streamInfo.pages[pageNdx]

        try with(page: page) { data in
          for n in 0..<chunkSize {
            rawBuffer[offset + n]
              = data.unsafeLoad(fromByteOffset: offsetInPage + n, as: UInt8.self)
          }
        }

        pageNdx += 1
        offsetInPage = 0
        offset += chunkSize
        bytesLeft -= chunkSize
      }

      return Array($0)
    }
  }

  func readString(stream: Int, offset: Int,
                  bytesRead: inout Int) throws -> String {
    var result = ""
    let streamInfo = streams[stream + 1]
    var pageNdx = offset / pageSize
    var offsetInPage = offset - pageNdx * pageSize

    bytesRead = 0

    if offset > streamInfo.size {
      throw MultiStreamFileError.readOffEndOfStream
    }

    while pageNdx < streamInfo.pages.count {
      let page = streamInfo.pages[pageNdx]
      var end = offsetInPage

      try with(page: page) {
        $0.withUnsafeBytes { data in
          while end < pageSize && data[end] != 0 {
            end += 1
          }

          let chunk = String(decoding: data[offsetInPage..<end],
                             as: Windows1252.self)
          result += chunk
          bytesRead += end - offsetInPage
        }
      }

      if end < pageSize {
        bytesRead += 1
        break
      }

      pageNdx += 1
      offsetInPage = 0
    }

    if pageNdx >= streamInfo.pages.count {
      throw MultiStreamFileError.readOffEndOfStream
    }

    return result
  }

  func dump(stream: Int) throws {
    let streamInfo = streams[stream + 1]
    var offset = 0

    func hex<T: FixedWidthInteger>(_ x: T, width: Int) -> String {
      let asHex = String(x, radix: 16)
      let length = asHex.count
      if length < width {
        let padding = String(repeating: "0", count: width - length)
        return padding + asHex
      }
      return asHex
    }

    while offset < streamInfo.size {
      let bytes = try read(stream: stream, offset: offset, as: UInt8.self, count: 16)

      var line = "\(hex(offset, width: 8)):"
      for byte in bytes {
        line += " \(hex(byte, width: 2))"
      }

      line += String(repeating: " ", count: 60 - line.count)

      for byte in bytes {
        if byte >= 32 && byte < 0x7f {
          line.append(Character(UnicodeScalar(byte)))
        } else {
          line += "."
        }
      }

      offset += bytes.count

      print(line)
    }
  }
}

extension MultiStreamFile: CustomStringConvertible {
  var description: String {
    return "MultiStreamFile { kind = \(kind), page size = \(pageSize), page count = \(pageCount), streams = \(streams.count - 1) }"
  }
}

#if os(Windows)
fileprivate func safe_read(
  _ handle: MultiStreamFile.FileHandle,
  _ offset: Int, _ buffer: UnsafeMutableRawPointer?, _ count: Int
) -> Int {
  var ovl = OVERLAPPED()
  ovl.Offset = DWORD(truncatingIfNeeded: offset)
  ovl.OffsetHigh = DWORD(truncatingIfNeeded: offset >> 32)

  var bytesRead = DWORD(0)
  let bRet = ReadFile(handle, buffer, DWORD(count), &bytesRead, &ovl)
  if !bRet {
    return -1
  }

  return Int(bytesRead)
}
#else
fileprivate func safe_read(
  _ handle: MultiStreamFile.FileHandle,
  _ offset: Int, _ buffer: UnsafeMutableRawPointer?, _ count: Int
) -> Int {
  while true {
    let ret = pread(handle, buffer, count, off_t(offset))
    if ret >= 0 || (ret < 0 && errno != EINTR) {
      return ret
    }
  }
}
#endif
