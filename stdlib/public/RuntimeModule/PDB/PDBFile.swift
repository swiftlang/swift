//===--- PDBFile.swift - PDB support for Swift ----------------------------===//
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
// Provides a PDBFile struct that understands how to read information from a
// Microsoft Portable Debug Format file.
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

internal import BacktracingImpl.ImageFormats.PDB

extension SC2 {
  static func fromSc(_ sc: SC) -> SC2 {
    return SC2(isect: sc.isect,
               off: sc.off,
               cb: sc.cb,
               dwCharacteristics: sc.dwCharacteristics,
               imod: sc.imod,
               dwDataCrc: sc.dwDataCrc,
               dwRelocCrc: sc.dwRelocCrc,
               isectCoff: 0)
  }
}

@_spi(PDB)
public class PDBFile {
  var msf: MultiStreamFile

  public struct Version: RawRepresentable, Comparable, Sendable {
    public let rawValue: UInt32

    public init(rawValue: UInt32) {
      self.rawValue = rawValue
    }

    public static let vc2     = Version(rawValue: 19941610)
    public static let vc4     = Version(rawValue: 19950623)
    public static let vc41    = Version(rawValue: 19950814)
    public static let vc50    = Version(rawValue: 19960307)
    public static let vc98    = Version(rawValue: 19970604)
    public static let vc70    = Version(rawValue: 20000404)
    public static let vc70dep = Version(rawValue: 19990604)
    public static let vc80    = Version(rawValue: 20030901)
    public static let vc110   = Version(rawValue: 20091201)
    public static let vc140   = Version(rawValue: 20140508)

    public static func == (lhs: Self, rhs: Self) -> Bool {
      return lhs.rawValue == rhs.rawValue
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }
  }

  public struct DebugInfoVersion: RawRepresentable, Comparable, Sendable {
    public let rawValue: UInt32

    public init(rawValue: UInt32) {
      self.rawValue = rawValue
    }

    public static let v0 = DebugInfoVersion(rawValue: 0)
    public static let v41 = DebugInfoVersion(rawValue: 930803)
    public static let v50 = DebugInfoVersion(rawValue: 19960307)
    public static let v60 = DebugInfoVersion(rawValue: 19970606)
    public static let v70 = DebugInfoVersion(rawValue: 19990903)
    public static let v110 = DebugInfoVersion(rawValue: 20091201)

    public static func == (lhs: Self, rhs: Self) -> Bool {
      return lhs.rawValue == rhs.rawValue
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }
  }

  var streamCount: Int {
    return msf.streams.count
  }

  public private(set) var version: Version
  public private(set) var signature: [UInt8]
  public private(set) var age: UInt32

  public private(set) var debugInfoVersion: DebugInfoVersion

  var globalSymbolIndexStream: Int
  var publicSymbolIndexStream: Int
  var globalSymbolStream: Int

  var extraStreams: [Int]

  struct Features: OptionSet, Sendable {
    let rawValue: Int

    init(rawValue: Int) {
      self.rawValue = rawValue
    }

    static let idStream = Features(rawValue: 1 << 0)
    static let noTypeMerge = Features(rawValue: 1 << 1)
    static let minimalDbgInfo = Features(rawValue: 1 << 2)
  }

  private(set) var features: Features

  public convenience init?(path: String) {
    guard let msf = MultiStreamFile(path: path) else {
      return nil
    }
    self.init(multiStreamFile: msf)
  }

  var streamNameTable: NameTableNI
  var modules: [Module] = []
  var segments: [PDB_OMF_SEGMAP_DESC] = []
  var sectionContributions: [SC2] = []

  var sections: [SectionInfo] = []
  var functions: [FunctionInfo] = []

  init?(multiStreamFile msf: MultiStreamFile) {
    self.msf = msf

    guard var headerStream = msf.open(stream: 1) else {
      return nil
    }

    do {
      let pdb = try headerStream.read(as: PDB_STREAM.self)

      version = Version(rawValue: pdb.impv)

      if version < .vc4 || version > .vc140 {
        // Unsupported version
        return nil
      }

      // Version 7 and above have a GUID; we represent that here as a byte array
      if version >= .vc70 {
        signature = try headerStream.read(as: UInt8.self, count: 16)
      } else {
        signature = [ UInt8(truncatingIfNeeded: pdb.sig),
                      UInt8(truncatingIfNeeded: pdb.sig >> 8),
                      UInt8(truncatingIfNeeded: pdb.sig >> 16),
                      UInt8(truncatingIfNeeded: pdb.sig >> 24) ]
      }

      // Read the name table
      guard let streamNameTable = try NameTableNI.load(from: &headerStream) else {
        return nil
      }
      self.streamNameTable = streamNameTable

      // Read feature flags
      features = []
      while !headerStream.atEnd {
        let sig = try headerStream.read(as: UInt32.self)

        if sig == Version.vc110.rawValue {
          features.insert(.idStream)
          break
        }

        if sig == Version.vc140.rawValue {
          features.insert(.idStream)
        } else if sig == 0x4d544f4e { // NOTM
          features.insert(.noTypeMerge)
        } else if sig == 0x494e494d { // MINI
          features.insert(.minimalDbgInfo)
        }
      }
    } catch {
      return nil
    }

    // Now read the debug info stream
    guard var debugInfoStream = msf.open(stream: 3) else {
      return nil
    }

    do {
      let sig = try debugInfoStream.pread(from: 0, as: UInt32.self)

      let dbiHdr: PDB_NEW_DBI_HDR
      if sig != 0xffffffff {
        // Convert the old header to the new format
        let oldDbiHdr = try debugInfoStream.read(as: PDB_DBI_HDR.self)

        dbiHdr = PDB_NEW_DBI_HDR(
          verSignature: 0xffffffff,
          verHdr: DebugInfoVersion.v0.rawValue,
          age: 1,
          snGSSyms: oldDbiHdr.snGSSyms,
          usVerAll: 0,
          snPSSyms: oldDbiHdr.snPSSyms,
          usVerPdbDllBuild: 0,
          snSymRecs: oldDbiHdr.snSymRecs,
          usVerPdbDllRBld: 0,
          cbGpModi: oldDbiHdr.cbGpModi,
          cbSC: oldDbiHdr.cbSC,
          cbSecMap: oldDbiHdr.cbSecMap,
          cbFileInfo: oldDbiHdr.cbFileInfo,
          cbTSMap: 0,
          iMFC: 0,
          cbDbgHdr: 0,
          cbECInfo: 0,
          flags: 0,
          wMachine: 0,
          rgulReserved: (0,)
        )
      } else {
        dbiHdr = try debugInfoStream.read(as: PDB_NEW_DBI_HDR.self)
      }

      age = dbiHdr.age

      if dbiHdr.cbDbgHdr > 0 {
        let numStreams = Int(dbiHdr.cbDbgHdr) / MemoryLayout<SN>.stride
        let offset = debugInfoStream.size - Int(dbiHdr.cbDbgHdr)

        extraStreams = (try debugInfoStream.pread(from: offset,
                                                  as: Int16.self,
                                                  count: numStreams)).map {
          Int($0)
        }
      } else {
        extraStreams = []
      }

      debugInfoVersion = DebugInfoVersion(rawValue: dbiHdr.verHdr)
      globalSymbolIndexStream = Int(dbiHdr.snGSSyms)
      publicSymbolIndexStream = Int(dbiHdr.snPSSyms)
      globalSymbolStream = Int(dbiHdr.snSymRecs)

      // Now read the module info substream
      if dbiHdr.cbGpModi > 0 {
        let end = debugInfoStream.offset + Int(dbiHdr.cbGpModi)
        if debugInfoVersion < .v60 {
          while debugInfoStream.offset < end
                  && end - debugInfoStream.offset >= MemoryLayout<MODI50>.size {
            let modi = try debugInfoStream.read(as: MODI50.self)
            let szModule = try debugInfoStream.readString()
            let szObjFile = try debugInfoStream.readString()

            modules.append(
              Module(
                name: szModule,
                object: szObjFile,
                streamNumber: modi.sn == SN_NIL ? nil : Int(modi.sn),
                cbSyms: Int(modi.cbSyms),
                cbLines: Int(modi.cbLines),
                cbFpo: Int(modi.cbFpo),
                cbC13Lines: 0
              )
            )

            _ = debugInfoStream.align(toMultiple: 4)
          }
        } else {
          while debugInfoStream.offset < end
                  && end - debugInfoStream.offset >= MemoryLayout<MODI60>.size {
            let modi = try debugInfoStream.read(as: MODI60.self)
            let szModule = try debugInfoStream.readString()
            let szObjFile = try debugInfoStream.readString()

            modules.append(
              Module(
                name: szModule,
                object: szObjFile,
                streamNumber: modi.sn == SN_NIL ? nil : Int(modi.sn),
                cbSyms: Int(modi.cbSyms),
                cbLines: Int(modi.cbLines),
                cbFpo: 0,
                cbC13Lines: Int(modi.cbC13Lines)
              )
            )

            _ = debugInfoStream.align(toMultiple: 4)
          }
        }
        _ = debugInfoStream.seek(offset: end)
      }

      // Read the section contribution substream
      if dbiHdr.cbSC > 0 {
        let version = try debugInfoStream.read(as: UInt32.self)

        let DBISCImpv  = UInt32(0xeffe0000) &+ 19970605
        let DBISCImpv2 = UInt32(0xeffe0000) &+ 20140516

        if version == DBISCImpv {
          let count = (Int(dbiHdr.cbSC) - 4) / MemoryLayout<SC>.stride
          sectionContributions = (try debugInfoStream.read(as: SC.self,
                                                           count: count))
            .map { SC2.fromSc($0) }
        } else if version == DBISCImpv2 {
          let count = (Int(dbiHdr.cbSC) - 4) / MemoryLayout<SC2>.stride
          sectionContributions = try debugInfoStream.read(as: SC2.self,
                                                          count: count)
        } else {
          _ = debugInfoStream.seek(offset: Int(dbiHdr.cbSC) - 4,
                                   relativeTo: .current)
        }
      }

      // Read the segment map substream
      segments = []
      if dbiHdr.cbSecMap > 0 {
        let header = try debugInfoStream.read(as: PDB_OMF_SEGMAP.self)

        segments = try debugInfoStream.read(as: PDB_OMF_SEGMAP_DESC.self,
                                            count: Int(header.cSeg))
      }

      // Read the file info substream
      if dbiHdr.cbFileInfo > 0 {
        let moduleCount = try debugInfoStream.read(as: IMOD.self)
        if moduleCount != modules.count {
          return nil
        }

        // Skip 16-bit count
        _ = debugInfoStream.seek(offset: 2, relativeTo: .current)

        // Skip the first 16-bit array
        _ = debugInfoStream.seek(offset: 2 * modules.count,
                                 relativeTo: .current)

        // Read the cref array
        let crefs = try debugInfoStream.read(as: UInt16.self,
                                             count: modules.count)

        // Compute the total of the crefs array
        let crefSum = crefs.map { Int($0) }.reduce(0, +)

        // Read the character offset array
        let ichs = try debugInfoStream.read(as: ICH.self, count: crefSum)

        let chbase = debugInfoStream.offset

        // Now extract all the strings
        var crefsDone = 0
        for ndx in 0..<modules.count {
          for nfile in 0..<crefs[ndx] {
            let iref = crefsDone + Int(nfile)
            let ich = Int(ichs[iref])
            let filename = try debugInfoStream.preadString(from: chbase + ich)

            modules[ndx].files.append(filename)
          }
          crefsDone += Int(crefs[ndx])
        }
      }
    } catch {
      print("Failed to load debug info: \(error)")
      return nil
    }

    // Read the section header stream
    guard var sectionStream = open(stream: .sectionHeader) else {
      return nil
    }

    do {
      let sectionCount = sectionStream.size
        / MemoryLayout<PDB_IMAGE_SECTION_HEADER>.stride

      let sectionArray = try sectionStream.read(as: PDB_IMAGE_SECTION_HEADER.self,
                                                count: sectionCount)
      for section in sectionArray {
        let name = withUnsafeBytes(of: section.Name) { bytes in
          if let firstNul = bytes.firstIndex(of: 0) {
            return String(decoding: bytes[0..<firstNul], as: UTF8.self)
          } else {
            return String(decoding: bytes, as: UTF8.self)
          }
        }
        sections.append(
          SectionInfo(
            name: name,
            virtualAddress: section.VirtualAddress,
            size: section.VirtualSize,
            characteristics:
              SectionCharacteristics(rawValue: section.Characteristics))
        )
      }
    } catch {
      return nil
    }

    // We scan the main symbol stream, because that contains the raw names
    // of the symbols; the module streams only contain "demangled" versions
    // which in the case of Swift don't actually match what we'd normally do.
    var rawNames : [UInt32:String] = [:]
    if var symbols = open(stream: .symbolRecords) {
      do {
        while !symbols.atEnd {
          let pos = symbols.offset

          let length = try symbols.read(as: UInt16.self)
          if length == 0 {
            break
          }

          let type = try symbols.read(as: UInt16.self)

          if type == S_PUB32 {
            let info = try symbols.read(as: PDB_CV_PUBSYM32.self)
            let section = Int(info.seg) - 1

            if section >= 0 && section < sections.count {
              let rawName = try symbols.readString()
              let address = sections[Int(info.seg) - 1].virtualAddress + info.off

              rawNames[address] = rawName
            }
          }

          _ = symbols.seek(offset: pos + Int(length) + 2)
        }
      } catch {
        print(error)
        return nil
      }
    }

    do {
      // Read the individual module streams
      for (ndx, module) in modules.enumerated() {
        guard let streamNumber = module.streamNumber else {
          continue
        }

        guard var moduleStream = open(stream: streamNumber) else {
          return nil
        }

        // Read the signature
        let signature = try moduleStream.read(as: UInt32.self)

        // Ignore anything that isn't C13 format
        if signature != CV_SIGNATURE_C13 {
          continue
        }

        // Scan the symbol records
        while moduleStream.offset < module.cbSyms {
          let pos = moduleStream.offset

          let length = try moduleStream.read(as: UInt16.self)
          if length == 0 {
            break
          }

          let type = try moduleStream.read(as: UInt16.self)

          if type == S_LPROC32 || type == S_GPROC32
               || type == S_LPROC32_ID || type == S_GPROC32_ID {
            let info = try moduleStream.read(as: PDB_CV_PROCSYM32.self)

            let name = try moduleStream.readString()
            let address = sections[Int(info.seg) - 1].virtualAddress + info.off
            let scope: FunctionInfo.Scope
              = (type == S_LPROC32 || type == S_LPROC32_ID) ? .local : .global

            functions.append(FunctionInfo(rawName: rawNames[address],
                                          name: name,
                                          address: address,
                                          length: info.len,
                                          scope: scope,
                                          moduleIndex: ndx))
          }

          _ = moduleStream.seek(offset: pos + Int(length) + 2)
        }
      }
    } catch {
      print(error)
      return nil
    }

    functions.sort { $0.address < $1.address }
  }

  enum StreamId {
    case pdbHeader
    case typeInfo
    case debugInfo
    case ipiHash
    case module(Int)
    case globalSymbolHash
    case publicSymbolHash
    case symbolRecords
    // case typeHash
    case fpo
    case exception
    case fixup
    case omapToSource
    case omapFromSource
    case sectionHeader
    case tokenRidMap
    case xdata
    case pdata
    case newFpo
    case sectionHeaderOrig
  }

  func streamNumber(from id: StreamId) -> Int? {
    switch id {
      case .pdbHeader: return 1
      case .typeInfo: return 2
      case .debugInfo: return 3
      case .ipiHash: return 4
      case .module(let n): return modules[n].streamNumber
      case .globalSymbolHash: return globalSymbolIndexStream
      case .publicSymbolHash: return publicSymbolIndexStream
      case .symbolRecords: return globalSymbolStream
                           // case .typeHash: return ??
      case .fpo:
        return has(stream: id) ? extraStreams[0] : nil
      case .exception:
        return has(stream: id) ? extraStreams[1] : nil
      case .fixup:
        return has(stream: id) ? extraStreams[2] : nil
      case .omapToSource:
        return has(stream: id) ? extraStreams[3] : nil
      case .omapFromSource:
        return has(stream: id) ? extraStreams[4] : nil
      case .sectionHeader:
        return has(stream: id) ? extraStreams[5] : nil
      case .tokenRidMap:
        return has(stream: id) ? extraStreams[6] : nil
      case .xdata:
        return has(stream: id) ? extraStreams[7] : nil
      case .pdata:
        return has(stream: id) ? extraStreams[8] : nil
      case .newFpo:
        return has(stream: id) ? extraStreams[9] : nil
      case .sectionHeaderOrig:
        return has(stream: id) ? extraStreams[10] : nil
    }
  }

  func has(stream: StreamId) -> Bool {
    switch stream {
      case .pdbHeader, .typeInfo, .debugInfo, .ipiHash: return true
      case .module(let n): return n >= 0 && n < modules.count
      case .globalSymbolHash: return globalSymbolIndexStream > 0
      case .publicSymbolHash: return publicSymbolIndexStream > 0
      case .symbolRecords: return globalSymbolStream > 0
      case .fpo: return extraStreams.count > 1 && extraStreams[0] > 0
      case .exception: return extraStreams.count > 2 && extraStreams[1] > 0
      case .fixup: return extraStreams.count > 3 && extraStreams[2] > 0
      case .omapToSource: return extraStreams.count > 4 && extraStreams[3] > 0
      case .omapFromSource: return extraStreams.count > 5 && extraStreams[4] > 0
      case .sectionHeader: return extraStreams.count > 6 && extraStreams[5] > 0
      case .tokenRidMap: return extraStreams.count > 7 && extraStreams[6] > 0
      case .xdata: return extraStreams.count > 8 && extraStreams[7] > 0
      case .pdata: return extraStreams.count > 9 && extraStreams[8] > 0
      case .newFpo: return extraStreams.count > 10 && extraStreams[9] > 0
      case .sectionHeaderOrig:
        return extraStreams.count > 11 && extraStreams[10] > 0
    }
  }

  func open(stream: Int) -> MultiStreamFile.Stream? {
    return msf.open(stream: stream)
  }

  func open(stream: StreamId) -> MultiStreamFile.Stream? {
    guard let stream = streamNumber(from: stream) else {
      return nil
    }
    return msf.open(stream: stream)
  }

  func open(stream: String) -> MultiStreamFile.Stream? {
    guard let streamNumber = streamNameTable[stream] else {
      return nil
    }
    return msf.open(stream: streamNumber)
  }

  func dump(stream: Int) throws {
    try msf.dump(stream: stream)
  }

  func dump(stream: String) throws {
    guard let streamNumber = streamNameTable[stream] else {
      return
    }
    try msf.dump(stream: streamNumber)
  }

  func dump(stream: StreamId) throws {
    try msf.dump(stream: streamNumber(from: stream)!)
  }

  func forEachDebugSection<R>(
    ofType wantedType: UInt32,
    in module: Module,
    body: (_ stream: inout MultiStreamFile.Stream, _ length: UInt32) -> R?
  ) -> R? {
    guard let streamNumber = module.streamNumber,
          var moduleStream = open(stream: streamNumber) else {
      return nil
    }

    // Find the sections
    _ = moduleStream.seek(offset: module.cbSyms + module.cbLines)

    do {
      while !moduleStream.atEnd {
        let type = try moduleStream.read(as: UInt32.self)

        if type == 0 {
          break
        }

        let length = try moduleStream.read(as: UInt32.self)
        let next = moduleStream.offset + Int(length)

        if type == wantedType {
          if let result = body(&moduleStream, length) {
            return result
          }
        }

        _ = moduleStream.seek(offset: next)
      }
    } catch {
      return nil
    }

    return nil
  }

  func lookup(file offset: UInt32, in module: Module) -> String? {
    var fileId: Int? = nil

    let ok = (forEachDebugSection(ofType: UInt32(DEBUG_S_FILECHKSMS),
                               in: module) {
      (stream: inout MultiStreamFile.Stream, length: UInt32) -> Bool? in

      if offset + 5 >= length {
        return false
      }

      _ = stream.seek(offset: Int(offset), relativeTo: .current)

      do {
        fileId = Int(try stream.read(as: UInt32.self))
        return true
      } catch {
        return false
      }
    }) ?? false

    if !ok {
      return nil
    }

    guard let fileId else {
      return nil
    }

    // Read directly from the name table (no need to load the name table
    // into memory this way).
    do {
      guard let nameStreamIndex = streamNameTable["/names"] else {
        return nil
      }
      guard var stream = msf.open(stream: nameStreamIndex) else {
        return nil
      }
      let vhdr = try stream.read(as: PDB_NMT_VHDR.self)
      if vhdr.ulHdr != 0xeffeeffe {
        // No version header
        _ = stream.seek(offset: 0)
      }

      let size = try stream.read(as: CB.self)

      if fileId >= size {
        return nil
      }

      _ = stream.seek(offset: fileId, relativeTo: .current)

      return try stream.readString()
    } catch {
      return nil
    }
  }

  public struct SourceLocation: CustomStringConvertible, Sendable {
    public struct Flags: OptionSet, Sendable {
      public var rawValue: Int

      public init(rawValue: Int) { self.rawValue = rawValue; }

      public static let noStepOnto = Flags(rawValue: 1 << 0)
      public static let noStepInto = Flags(rawValue: 1 << 1)
    }

    public var filename: String
    public var lineRange: Range<Int>?
    public var columnRange: Range<Int>?
    public var flags: Flags

    public var description: String {
      var formatted = filename

      if let lineRange {
        let firstLine = lineRange.lowerBound
        let lastLine = lineRange.upperBound
        if firstLine == lastLine {
          formatted += ":\(firstLine)"
        } else {
          formatted += ":\(firstLine)-\(lastLine)"
        }

        if let columnRange {
          let firstCol = columnRange.lowerBound
          let lastCol = columnRange.upperBound
          if firstCol == lastCol {
            formatted += ":\(firstCol)"
          } else {
            formatted += ":\(firstCol)-\(lastCol)"
          }
        }
      }

      if flags.contains(.noStepOnto) {
        formatted += " [no-step-onto]"
      }
      if flags.contains(.noStepInto) {
        formatted += " [no-step-into]"
      }

      return formatted
    }
  }

  func lookup(
    address: UInt32, in module: Module
  ) -> SourceLocation? {
    if module.cbC13Lines == 0 {
      return nil
    }

    return forEachDebugSection(ofType: UInt32(DEBUG_S_LINES),
                               in: module) {
      (stream: inout MultiStreamFile.Stream, length: UInt32) -> SourceLocation? in

      do {
        let end = stream.offset + Int(length)
        let header = try stream.read(as: PDB_CV_LINE_HEADER.self)
        let sectionBase = sections[Int(header.segCon) - 1].virtualAddress
        let baseAddress = header.offCon + sectionBase
        if address < baseAddress {
          return nil
        }
        let offset = address - baseAddress

        while stream.offset < end {
          let block = try stream.read(as: PDB_CV_LINE_BLOCK_HEADER.self)
          let count = Int(block.nLines)
          let lines = try stream.read(as: PDB_CV_LINE.self, count: count)
          var filename: String? = nil
          var lineRange: Range<Int>? = nil
          var columnRange: Range<Int>? = nil
          var lineIndex: Int? = nil
          var flags: SourceLocation.Flags = []

          for (ndx, line) in lines.enumerated() {
            if offset == line.offset
                 || (offset > line.offset
                       && ndx + 1 < lines.count
                       && offset < lines[ndx + 1].offset) {
              lineIndex = ndx
              filename = lookup(file: block.offFile, in: module)!
              let start = Int(pdb_line_start(line.lineInfo))
              let end = start + Int(pdb_line_delta(line.lineInfo))
              if start == 0xfeefee {
                flags = [.noStepOnto]
              } else if start == 0xf00f00 {
                flags = [.noStepInto]
              } else {
                lineRange = start..<end
              }
              break
            }
          }

          guard let lineIndex else {
            continue
          }

          if (header.flags & UInt16(CV_LINES_HAVE_COLUMNS)) != 0 {
            let columns = try stream.read(as: PDB_CV_COLUMN.self, count: count)
            let start = Int(columns[lineIndex].offColumnStart)
            let end = Int(columns[lineIndex].offColumnEnd)
            columnRange = start..<end
          }

          return SourceLocation(filename: filename!,
                                lineRange: lineRange,
                                columnRange: columnRange,
                                flags: flags)
        }

        return nil
      } catch {
        print("Error \(error) during lookup")
        return nil
      }
    }
  }

  public struct SymbolLookup: CustomStringConvertible {
    public var rawName: String?
    public var name: String
    public var offset: UInt32
    public var sourceLocation: SourceLocation?

    public var description: String {
      var formatted: String

      if offset == 0 {
        formatted = name
      } else {
        formatted = "\(name) + \(offset)"
      }

      if let sourceLocation {
        formatted += " (\(sourceLocation))"
      }

      return formatted
    }
  }

  public func lookupFunction(for address: UInt32) -> Int? {
    var min = 0, max = functions.count
    while min < max {
      let mid = min + (max - min) / 2

      if address >= functions[mid].address
           && (mid + 1 == functions.count
                 || address < functions[mid + 1].address) {
        return mid
      } else if address < functions[mid].address {
        max = mid
      } else {
        min = mid + 1
      }
    }
    return nil
  }

  public func lookup(address: UInt32) -> SymbolLookup? {
    guard let funcIndex = lookupFunction(for: address) else {
      return nil
    }

    let function = functions[funcIndex]
    let module = modules[function.moduleIndex]

    let sourceLocation = lookup(address: address, in: module)

    return SymbolLookup(rawName: function.rawName,
                        name: function.name,
                        offset: address - function.address,
                        sourceLocation: sourceLocation)
  }
}
