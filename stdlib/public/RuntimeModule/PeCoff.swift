//===--- PeCoff.swift - PE-COFF support for Swift -------------------------===//
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
// Defines various PE-COFF structures and provides types for working with
// PE-COFF images on disk and in memory.
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
internal import BacktracingImpl.ImageFormats.PeCoff
internal import BacktracingImpl.ImageFormats.CodeView

// .. Typealiases ..............................................................

// We define our types in the swift.runtime namespace to avoid any possibility
// of type clashes elsewhere.  That means they have verbose names, so declare
// type aliases to fix that.

typealias pe_dos_signature = swift.runtime.pe_dos_signature
typealias pe_signature = swift.runtime.pe_signature
typealias pe_machine = swift.runtime.pe_machine
typealias pe_characteristics = swift.runtime.pe_characteristics
typealias pe_optional_magic = swift.runtime.pe_optional_magic
typealias pe_subsystem = swift.runtime.pe_subsystem
typealias pe_dll_characteristics = swift.runtime.pe_dll_characteristics
typealias pe_section_characteristics = swift.runtime.pe_section_characteristics
typealias pe_symbol_type = swift.runtime.pe_symbol_type
typealias pe_symbol_class = swift.runtime.pe_symbol_class
typealias pe_debug_directory_type = swift.runtime.pe_debug_directory_type
typealias pe_debug_characteristics = swift.runtime.pe_debug_characteristics

typealias pe_dos_header = swift.runtime.pe_dos_header
typealias pe_header = swift.runtime.pe_header
typealias pe_optional_header = swift.runtime.pe_optional_header
typealias pe_data_directory_entry = swift.runtime.pe_data_directory_entry
typealias pe_windows_header32 = swift.runtime.pe_windows_header32
typealias pe_windows_header64 = swift.runtime.pe_windows_header64
typealias pe_section = swift.runtime.pe_section
typealias pe_debug_directory = swift.runtime.pe_debug_directory
typealias pe_symbol = swift.runtime.pe_symbol

// .. Byte swapping ............................................................

extension pe_dos_signature: ByteSwappable {
  var byteSwapped: Self {
    pe_dos_signature(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_signature: ByteSwappable {
  var byteSwapped: Self {
    pe_signature(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_machine: ByteSwappable {
  var byteSwapped: Self {
    pe_machine(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_optional_magic: ByteSwappable {
  var byteSwapped: Self {
    pe_optional_magic(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_subsystem: ByteSwappable {
  var byteSwapped: Self {
    pe_subsystem(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_symbol_type: ByteSwappable {
  var byteSwapped: Self {
    pe_symbol_type(rawValue: rawValue.byteSwapped)!
  }
}

extension pe_dos_header: ByteSwappable {
  var byteSwapped: Self {
    return pe_dos_header(
      e_magic: e_magic.byteSwapped,
      e_cblp: e_cblp.byteSwapped,
      e_cp: e_cp.byteSwapped,
      e_crlc: e_crlc.byteSwapped,
      e_cparhdr: e_cparhdr.byteSwapped,
      e_minalloc: e_minalloc.byteSwapped,
      e_maxalloc: e_maxalloc.byteSwapped,
      e_ss: e_ss.byteSwapped,
      e_sp: e_sp.byteSwapped,
      e_csum: e_csum.byteSwapped,
      e_ip: e_ip.byteSwapped,
      e_cs: e_cs.byteSwapped,
      e_lfarlc: e_lfarlc.byteSwapped,
      e_ovno: e_ovno.byteSwapped,
      e_res: e_res,
      e_oemid: e_oemid.byteSwapped,
      e_oeminfo: e_oeminfo.byteSwapped,
      e_res2: e_res2,
      e_lfanew: e_lfanew.byteSwapped
    )
  }
}

extension pe_header: ByteSwappable {
  var byteSwapped: Self {
    return pe_header(
      Machine: Machine.byteSwapped,
      NumberOfSections: NumberOfSections.byteSwapped,
      TimeDateStamp: TimeDateStamp.byteSwapped,
      PointerToSymbolTable: PointerToSymbolTable.byteSwapped,
      NumberOfSymbols: NumberOfSymbols.byteSwapped,
      SizeOfOptionalHeader: SizeOfOptionalHeader.byteSwapped,
      Characteristics: Characteristics.byteSwapped
    )
  }
}

extension pe_optional_header: ByteSwappable {
  var byteSwapped: Self {
    return pe_optional_header(
      Magic: Magic.byteSwapped,
      MajorLinkerVersion: MajorLinkerVersion,
      MinorLinkerVersion: MinorLinkerVersion,
      SizeOfCode: SizeOfCode.byteSwapped,
      SizeOfInitializedData: SizeOfInitializedData.byteSwapped,
      SizeOfUninitializedData: SizeOfUninitializedData.byteSwapped,
      AddressOfEntryPoint: AddressOfEntryPoint.byteSwapped,
      BaseOfCode: BaseOfCode.byteSwapped
    )
  }
}

extension pe_data_directory_entry: ByteSwappable {
  var byteSwapped: Self {
    return pe_data_directory_entry(
      VirtualAddress: VirtualAddress.byteSwapped,
      Size: Size.byteSwapped
    )
  }
}

extension pe_windows_header32: ByteSwappable {
  var byteSwapped: Self {
    return pe_windows_header32(
      ImageBase: ImageBase.byteSwapped,
      SectionAlignment: SectionAlignment.byteSwapped,
      FileAlignment: FileAlignment.byteSwapped,
      MajorOperatingSystemVersion: MajorOperatingSystemVersion.byteSwapped,
      MinorOperatingSystemVersion: MinorOperatingSystemVersion.byteSwapped,
      MajorImageVersion: MajorImageVersion.byteSwapped,
      MinorImageVersion: MinorImageVersion.byteSwapped,
      MajorSubsystemVersion: MajorSubsystemVersion.byteSwapped,
      MinorSubsystemVersion: MinorSubsystemVersion.byteSwapped,
      Win32VersionValue: Win32VersionValue.byteSwapped,
      SizeOfImage: SizeOfImage.byteSwapped,
      SizeOfHeaders: SizeOfHeaders.byteSwapped,
      CheckSum: CheckSum.byteSwapped,
      Subsystem: Subsystem.byteSwapped,
      DllCharacteristics: DllCharacteristics.byteSwapped,
      SizeOfStackReserve: SizeOfStackReserve.byteSwapped,
      SizeOfStackCommit: SizeOfStackCommit.byteSwapped,
      SizeOfHeapReserve: SizeOfHeapReserve.byteSwapped,
      SizeOfHeapCommit: SizeOfHeapCommit.byteSwapped,
      LoaderFlags: LoaderFlags.byteSwapped,
      NumberOfRvaAndSizes: NumberOfRvaAndSizes.byteSwapped
    )
  }
}

extension pe_windows_header64: ByteSwappable {
  var byteSwapped: Self {
    return pe_windows_header64(
      ImageBase: ImageBase.byteSwapped,
      SectionAlignment: SectionAlignment.byteSwapped,
      FileAlignment: FileAlignment.byteSwapped,
      MajorOperatingSystemVersion: MajorOperatingSystemVersion.byteSwapped,
      MinorOperatingSystemVersion: MinorOperatingSystemVersion.byteSwapped,
      MajorImageVersion: MajorImageVersion.byteSwapped,
      MinorImageVersion: MinorImageVersion.byteSwapped,
      MajorSubsystemVersion: MajorSubsystemVersion.byteSwapped,
      MinorSubsystemVersion: MinorSubsystemVersion.byteSwapped,
      Win32VersionValue: Win32VersionValue.byteSwapped,
      SizeOfImage: SizeOfImage.byteSwapped,
      SizeOfHeaders: SizeOfHeaders.byteSwapped,
      CheckSum: CheckSum.byteSwapped,
      Subsystem: Subsystem.byteSwapped,
      DllCharacteristics: DllCharacteristics.byteSwapped,
      SizeOfStackReserve: SizeOfStackReserve.byteSwapped,
      SizeOfStackCommit: SizeOfStackCommit.byteSwapped,
      SizeOfHeapReserve: SizeOfHeapReserve.byteSwapped,
      SizeOfHeapCommit: SizeOfHeapCommit.byteSwapped,
      LoaderFlags: LoaderFlags.byteSwapped,
      NumberOfRvaAndSizes: NumberOfRvaAndSizes.byteSwapped
    )
  }
}

extension pe_section: ByteSwappable {
  var byteSwapped: Self {
    return pe_section(
      Name: Name,
      VirtualSize: VirtualSize.byteSwapped,
      VirtualAddress: VirtualAddress.byteSwapped,
      SizeOfRawData: SizeOfRawData.byteSwapped,
      PointerToRawData: PointerToRawData.byteSwapped,
      PointerToRelocations: PointerToRelocations.byteSwapped,
      PointerToLinenumbers: PointerToLinenumbers.byteSwapped,
      NumberOfRelocations: NumberOfRelocations.byteSwapped,
      NumberOfLinenumbers: NumberOfLinenumbers.byteSwapped,
      Characteristics: Characteristics.byteSwapped
    )
  }
}

extension pe_symbol: ByteSwappable {
  var byteSwapped: Self {
    var symbol = pe_symbol()
    if LongName.Zeroes == 0 {
      symbol.LongName.Zeroes = 0
      symbol.LongName.Offset = LongName.Offset.byteSwapped
    } else {
      symbol.ShortName = ShortName
    }
    symbol.Value = Value.byteSwapped
    symbol.SectionNumber = SectionNumber.byteSwapped
    symbol.Type = Type.byteSwapped
    symbol.StorageClass = StorageClass
    symbol.NumberOfAuxSymbols = NumberOfAuxSymbols
    return symbol
  }
}

@inline(__always)
fileprivate func maybeSwap<T: ByteSwappable>(_ x: T) -> T {
  #if _endian(big)
  return x.byteSwapped
  #else
  return x
  #endif
}

@inline(__always)
fileprivate func maybeSwap<T: FixedWidthInteger>(_ x: T) -> T {
  #if _endian(big)
  return x.byteSwapped
  #else
  return x
  #endif
}

// .. PeCoffStringTable ........................................................

struct PeCoffStringTable {
  let source: ImageSource

  func getStringAt(index: Int) -> String? {
    if index < 0 || index >= source.bytes.count {
      return nil
    }

    let slice = UnsafeRawBufferPointer(rebasing: source.bytes[index...])
    var len: Int = 0
    len = strnlen(slice.baseAddress!, slice.count)
    return String(decoding: source.bytes[index..<index+len], as: UTF8.self)
  }
}

// .. PeCoffImage ..............................................................

enum PeCoffImageError: Error {
  case noDOSSignature
  case badPESignature
  case missingOptionalHeader
  case badOptionalHeader
}

enum PeImageDirectoryEntry: Int {
  case export = 0
  case `import` = 1
  case resource = 2
  case exception = 3
  case security = 4
  case baseRelocationTable = 5
  case debug = 6
  case architectureSpecific = 7
  case globalPointer = 8
  case threadLocalStorage = 9
  case loadConfiguration = 10
  case boundImports = 11
  case importAddressTable = 12
  case delayImportTable = 13
  case comRuntimeDescriptor = 14
}

struct PeVersion {
  var major: UInt16
  var minor: UInt16
}

struct PeSection {
  var name: String
  var virtualSize: UInt32
  var virtualAddress: UInt32
  var sizeOfRawData: UInt32
  var pointerToRawData: UInt32
  var pointerToRelocations: UInt32
  var pointerToLinenumbers: UInt32
  var numberOfRelocations: UInt16
  var numberOfLinenumbers: UInt16
  var characteristics: pe_section_characteristics
}

struct PeCodeview {
  var uuid: [UInt8]
  var age: UInt32
  var pdbPath: String
}

struct PeFunction {
  var name: String
  var address: ImageSource.Address
}

final class PeCoffImage {
  typealias Address = ImageSource.Address

  var baseAddress: Address
  var endAddress: Address

  var source: ImageSource

  var timestamp: UInt32
  var characteristics: pe_characteristics
  var entryPoint: UInt32
  var imageBase: UInt64
  var operatingSystemVersion: PeVersion
  var imageVersion: PeVersion
  var subsystemVersion: PeVersion
  var checksum: UInt32
  var subsystem: pe_subsystem
  var dllCharacteristics: pe_dll_characteristics
  var dataDirectory: [pe_data_directory_entry]
  var sections: [PeSection]
  var sectionMap: [String:Int]

  var codeview: PeCodeview?

  var reproHash: [UInt8]?

  var functions: [PeFunction]?

  init(source: ImageSource,
       baseAddress: Address = 0,
       endAddress: Address = 0) throws {
    // Check the DOS header first
    let dosHeader = maybeSwap(try source.fetch(from: 0, as: pe_dos_header.self))
    if dosHeader.e_magic != .PE_DOS_SIGNATURE {
      throw PeCoffImageError.noDOSSignature
    }

    // Now look for the PE signature
    var pos = Address(dosHeader.e_lfanew)
    let peSignature = maybeSwap(
      try source.fetch(from: pos, as: pe_signature.self)
    )
    if peSignature != .PE_SIGNATURE {
      throw PeCoffImageError.badPESignature
    }
    pos += 4

    // Read the PE header
    let peHeader = maybeSwap(try source.fetch(from: pos,
                                              as: pe_header.self))
    if peHeader.SizeOfOptionalHeader < MemoryLayout<pe_optional_header>.size {
      throw PeCoffImageError.missingOptionalHeader
    }
    pos += Address(MemoryLayout<pe_header>.size)

    let optionalHeaderStart = pos
    let optionalHeader = maybeSwap(try source.fetch(from: pos,
                                                    as: pe_optional_header.self))
    pos += Address(MemoryLayout<pe_optional_header>.size)

    // PE32 has an extra BaseOfData field (skip this)
    if optionalHeader.Magic == .PE_PE32 {
      pos += 4
    }

    let windowsHeader: pe_windows_header64
    if optionalHeader.Magic == .PE_PE32 {
      let windowsHeader32 = maybeSwap(
        try source.fetch(
          from: pos,
          as: pe_windows_header32.self
        )
      )
      pos += Address(MemoryLayout<pe_windows_header32>.size)

      // "Upgrade" this to the 64-bit version
      windowsHeader = pe_windows_header64(
        ImageBase: UInt64(windowsHeader32.ImageBase),
        SectionAlignment: windowsHeader32.SectionAlignment,
        FileAlignment: windowsHeader32.FileAlignment,
        MajorOperatingSystemVersion: windowsHeader32.MajorOperatingSystemVersion,
        MinorOperatingSystemVersion: windowsHeader32.MinorOperatingSystemVersion,
        MajorImageVersion: windowsHeader32.MajorImageVersion,
        MinorImageVersion: windowsHeader32.MinorImageVersion,
        MajorSubsystemVersion: windowsHeader32.MajorSubsystemVersion,
        MinorSubsystemVersion: windowsHeader32.MinorSubsystemVersion,
        Win32VersionValue: windowsHeader32.Win32VersionValue,
        SizeOfImage: windowsHeader32.SizeOfImage,
        SizeOfHeaders: windowsHeader32.SizeOfHeaders,
        CheckSum: windowsHeader32.CheckSum,
        Subsystem: windowsHeader32.Subsystem,
        DllCharacteristics: windowsHeader32.DllCharacteristics,
        SizeOfStackReserve: UInt64(windowsHeader32.SizeOfStackReserve),
        SizeOfStackCommit: UInt64(windowsHeader32.SizeOfStackCommit),
        SizeOfHeapReserve: UInt64(windowsHeader32.SizeOfHeapReserve),
        SizeOfHeapCommit: UInt64(windowsHeader32.SizeOfHeapCommit),
        LoaderFlags: windowsHeader32.LoaderFlags,
        NumberOfRvaAndSizes: windowsHeader32.NumberOfRvaAndSizes
      )
    } else if optionalHeader.Magic == .PE_PE32_PLUS {
      windowsHeader = maybeSwap(
        try source.fetch(
          from: pos,
          as: pe_windows_header64.self
        )
      )

      pos += Address(MemoryLayout<pe_windows_header64>.size)
    } else {
      throw PeCoffImageError.badOptionalHeader
    }

    // Read the data directory
    var dataDirectory = try source.fetch(
        from: pos,
        count: Int(windowsHeader.NumberOfRvaAndSizes),
        as: pe_data_directory_entry.self
    )

    #if _endian(big)
    dataDirectory.swapBytes()
    #endif

    // Now read the sections
    let sectionStart = optionalHeaderStart
      + Address(peHeader.SizeOfOptionalHeader)

    var sections = try source.fetch(
      from: sectionStart,
      count: Int(peHeader.NumberOfSections),
      as: pe_section.self)

    #if _endian(big)
    sections.swapBytes()
    #endif

    var symbols: [pe_symbol]? = nil
    var stringTable: PeCoffStringTable? = nil
    var functions: [PeFunction]? = nil

    if !source.isMappedImage && peHeader.PointerToSymbolTable != 0 {
      // For images loaded from disk, if we find there are symbols, we
      // should read them, and the corresponding string table
      symbols = try source.fetch(
        from: Address(peHeader.PointerToSymbolTable),
        count: Int(peHeader.NumberOfSymbols),
        as: pe_symbol.self
      )

      #if _endian(big)
      symbols.swapBytes()
      #endif

      let stringTableOffset = Address(peHeader.PointerToSymbolTable)
        + Address(MemoryLayout<pe_symbol>.size
                                * Int(peHeader.NumberOfSymbols))

      let stringTableSize = maybeSwap(
        try source.fetch(
          from: stringTableOffset,
          as: UInt32.self
        )
      )
      let stringTableEnd = stringTableOffset + Address(stringTableSize)
      let stringSource = source[stringTableOffset..<stringTableEnd]
      stringTable = PeCoffStringTable(source: stringSource)

      var theFunctions: [PeFunction] = []

      var skip = 0
      for symbol in symbols! {
        // Used to skip over aux symbol records
        if skip > 0 {
          skip -= 1
          continue
        }

        if symbol.NumberOfAuxSymbols > 0 {
          skip = Int(symbol.NumberOfAuxSymbols)
        }

        // We are only interested in functions
        if symbol.StorageClass != .PE_SYMBOL_CLASS_FUNCTION {
          continue
        }

        // And, at that, only those that have a section number
        if symbol.SectionNumber <= 0 {
          continue
        }

        let name: String
        if symbol.LongName.Zeroes == 0 {
          if let theName
               = stringTable!.getStringAt(index: Int(symbol.LongName.Offset)) {
            name = theName
          } else {
            continue
          }
        } else {
          var shortName = symbol.ShortName
          name = withUnsafeBytes(of: &shortName) {
            String(decoding: $0, as: UTF8.self)
          }
        }

        let sectionBase = Address(
          sections[Int(symbol.SectionNumber) - 1].VirtualAddress
        )
        let address = sectionBase + Address(symbol.Value)

        theFunctions.append(PeFunction(name: name,
                                       address: address))
      }

      theFunctions.sort { $0.address < $1.address }

      functions = theFunctions
    }

    // Fill in our member variables
    self.source = source

    self.baseAddress = baseAddress
    self.endAddress = endAddress

    self.timestamp = peHeader.TimeDateStamp
    self.characteristics = peHeader.Characteristics
    self.entryPoint = optionalHeader.AddressOfEntryPoint
    self.imageBase = windowsHeader.ImageBase
    self.operatingSystemVersion = PeVersion(
      major: windowsHeader.MajorOperatingSystemVersion,
      minor: windowsHeader.MinorOperatingSystemVersion
    )
    self.imageVersion = PeVersion(
      major: windowsHeader.MajorImageVersion,
      minor: windowsHeader.MinorImageVersion
    )
    self.subsystemVersion = PeVersion(
      major: windowsHeader.MajorSubsystemVersion,
      minor: windowsHeader.MinorSubsystemVersion
    )
    self.checksum = windowsHeader.CheckSum
    self.subsystem = windowsHeader.Subsystem
    self.dllCharacteristics = windowsHeader.DllCharacteristics
    self.dataDirectory = dataDirectory

    self.sections = []
    self.sectionMap = [:]

    self.functions = functions

    for section in sections {
      var theName = section.Name
      var name = withUnsafeBytes(of: &theName) { buffer in
        let len = strnlen(buffer.baseAddress!, buffer.count)
        return String(decoding: buffer[0..<len], as: UTF8.self)
      }

      if name.hasPrefix("/") {
        if let offset = Int(name[name.index(after: name.startIndex)...]) {
          // This is a long section name; these aren't really supposed to be
          // in executables, because the string table isn't mapped there so
          // we can't decode them unless we're looking at an image on disk

          if let stringTable {
            if let realName = stringTable.getStringAt(index: offset) {
              name = realName
            }
          }
        }
      }

      sectionMap[name] = self.sections.count

      self.sections.append(
        PeSection(
          name: name,
          virtualSize: section.VirtualSize,
          virtualAddress: section.VirtualAddress,
          sizeOfRawData: section.SizeOfRawData,
          pointerToRawData: section.PointerToRawData,
          pointerToRelocations: section.PointerToRelocations,
          pointerToLinenumbers: section.PointerToLinenumbers,
          numberOfRelocations: section.NumberOfRelocations,
          numberOfLinenumbers: section.NumberOfLinenumbers,
          characteristics: section.Characteristics
        )
      )
    }

    if dataDirectory.count > PeImageDirectoryEntry.debug.rawValue {
      let debugInfo = dataDirectory[PeImageDirectoryEntry.debug.rawValue]

      if debugInfo.VirtualAddress != 0 && debugInfo.Size != 0 {
        var pos: Address
        if source.isMappedImage {
          pos = Address(debugInfo.VirtualAddress) + self.imageBase
        } else {
          pos = Address(filePointer(from: debugInfo.VirtualAddress)!)
        }

        let end = pos + Address(debugInfo.Size)
        while pos < end {
          let entry = try source.fetch(from: pos, as: pe_debug_directory.self)

          pos += Address(MemoryLayout<pe_debug_directory>.size)

          let dataPos: Address
          if source.isMappedImage {
            dataPos = Address(entry.AddressOfRawData) + self.imageBase
          } else {
            dataPos = Address(entry.PointerToRawData)
          }

          let dataEnd = dataPos + Address(entry.SizeOfData)
          let entrySource = source[dataPos..<dataEnd]
          switch entry.Type {
            case .PE_DEBUG_TYPE_CODEVIEW:
              let magic = maybeSwap(try entrySource.fetch(from:0, as: UInt32.self))
              if magic != CV_PDB70_MAGIC || entry.SizeOfData < 24 {
                break
              }

              let uuid = try entrySource.fetch(from: 4,
                                               count: 16,
                                               as: UInt8.self)
              let age = maybeSwap(try entrySource.fetch(from: 20,
                                                        as: UInt32.self))
              let pdbFile = try entrySource.fetchString(from: 24)!

              self.codeview = PeCodeview(uuid: uuid, age: age, pdbPath: pdbFile)

            case .PE_DEBUG_TYPE_REPRO:
              if entry.SizeOfData > 4 {
                let len = maybeSwap(try entrySource.fetch(from: 0, as: UInt32.self))

                reproHash = try entrySource.fetch(from: 4,
                                                  count: Int(len),
                                                  as: UInt8.self)
              }

            default:
              break
          }
        }
      }
    }
  }

  /// Convert a virtual address into a file pointer
  func filePointer(from virtualAddress: UInt32) -> UInt32? {
    for section in sections {
      if virtualAddress >= section.virtualAddress {
        let offset = virtualAddress - section.virtualAddress
        if offset < section.virtualSize && offset < section.sizeOfRawData {
          return section.pointerToRawData + offset
        }
      }
    }

    return nil
  }

  /// Convert a file pointer into a virtual address
  func virtualAddress(from filePointer: UInt32) -> UInt32? {
    for section in sections {
      if filePointer >= section.pointerToRawData {
        let offset = filePointer - section.pointerToRawData
        if offset < section.sizeOfRawData {
          return section.virtualAddress + offset
        }
      }
    }

    return nil
  }

  /// Find the named section and return an ImageSource pointing at it.
  func getSection(_ name: String) -> ImageSource? {
    guard let ndx = sectionMap[name] else {
      return nil
    }

    let section = sections[ndx]

    if source.isMappedImage {
      let base = Address(section.virtualAddress)
      let end = base + Address(section.virtualSize)
      return source[base..<end]
    } else {
      let base = Address(section.pointerToRawData)
      let end = base + Address(min(section.virtualSize, section.sizeOfRawData))
      return source[base..<end]
    }
  }

  private lazy var dwarfReader = { [unowned self] in
    #if _endian(big)
    let shouldSwap = true
    #else
    let shouldSwap = false
    #endif
    return try? DwarfReader(source: self, shouldSwap: shouldSwap)
  }()
}

extension PeCoffImage: SymbolSource {
  func lookupSymbol(address: SymbolSource.Address) -> SymbolSource.Symbol? {
    let address = address + Address(imageBase)

    if let function = dwarfReader?.lookupFunction(at: address) {
      let offset = address - function.lowPC
      return SymbolSource.Symbol(name: function.rawName,
                                 offset: Int(offset),
                                 size: nil)
    } else if let functions {
      // If we don't have a DWARF reader, but we do have a function list,
      // try looking in the function list.

      var min = 0, max = functions.count
      while min < max {
        let mid = min + (max - min) / 2

        if address < functions[mid].address {
          max = mid
          continue
        }

        if address >= functions[mid].address {
          if mid + 1 == functions.count || address < functions[mid + 1].address {
            let offset = address - functions[mid].address
            return SymbolSource.Symbol(name: functions[mid].name,
                                       offset: Int(offset),
                                       size: nil)
          }
          min = mid + 1
        }
      }
    }

    return nil
  }

  func sourceLocation(
    for relativeAddress: SymbolSource.Address
  ) -> SymbolSource.SourceLocation? {
    let address = relativeAddress + Address(imageBase)

    guard let dwarfReader else {
      return nil
    }
    return try? dwarfReader.sourceLocation(
      for: DwarfReader<PeCoffImage>.Address(address)
    )
  }

  func inlineCallSites(
    at relativeAddress: SymbolSource.Address
  ) -> Array<SymbolSource.CallSiteInfo> {
    let address = relativeAddress + Address(imageBase)

    guard let dwarfReader else {
      return []
    }

    var result: [SymbolSource.CallSiteInfo] = []
    for site in dwarfReader.lookupInlineCallSites(
          at: DwarfReader<PeCoffImage>.Address(address)
        ) {
      result.append(SymbolSource.CallSiteInfo(rawName: site.rawName,
                                              name: site.name,
                                              location: SourceLocation(
                                                path: site.filename,
                                                line: site.line,
                                                column: site.column
                                              )))
    }

    return result
  }
}

extension PeCoffImage: DwarfSource {

  static var pathSeparator: String { "\\" }

  func getDwarfSection(_ section: DwarfSection) -> ImageSource? {
    // If linked with `link.exe`, the DWARF section names get
    // truncated; this unfortunately means that `.debug_line`
    // and `.debug_loc` get munged together.
    //
    // On the other hand, if linked with `lld`, long names are
    // used, in spite of the Microsoft documentation saying that
    // this is not valid in an `.exe` file.  We can only locate
    // the DWARF sections in that case if we're looking at the
    // *file*, because the actual name is in the COFF string
    // table, which isn't normally loaded.
    //
    // We intentionally choose to only support a small handful
    // of truncated sections here.

    switch section {
    case .debugAbbrev:
      if let abbrevs = getSection(".debug_abbrev") {
        return abbrevs
      }
      return getSection(".debug_a")
    case .debugInfo:
      if let info = getSection(".debug_info") {
        return info
      }
      return getSection(".debug_i")
    case .debugLine:
      if let line = getSection(".debug_line") {
        return line
      }
      return getSection(".debug_l")
    case .debugNames:
      if let names = getSection(".debug_names") {
        return names
      }
      return getSection(".debug_n")
    case .debugRanges:
      if let ranges = getSection(".debug_ranges") {
        return ranges
      }
      return getSection(".debug_r")
    case .debugStr:
      if let str = getSection(".debug_str") {
        return str
      }
      return getSection(".debug_s")

    case .debugAddr: return getSection(".debug_addr")
    case .debugARanges: return getSection(".debug_aranges")
    case .debugFrame: return getSection(".debug_frame")
    case .debugLineStr: return getSection(".debug_line_str")
    case .debugLoc: return getSection(".debug_loc")
    case .debugLocLists: return getSection(".debug_loclists")
    case .debugMacInfo: return getSection(".debug_macinfo")
    case .debugMacro: return getSection(".debug_macro")
    case .debugPubNames: return getSection(".debug_pubnames")
    case .debugPubTypes: return getSection(".debug_pubtypes")
    case .debugRngLists: return getSection(".debug_rnglists")
    case .debugStrOffsets: return getSection(".debug_str_offsets")
    case .debugSup: return getSection(".debug_sup")
    case .debugTypes: return getSection(".debug_types")
    case .debugCuIndex: return getSection(".debug_cu_index")
    case .debugTuIndex: return getSection(".debug_tu_index")
    }
  }

}

@_spi(SymbolLocation)
extension PeCoffImage: SymbolLocator.Image {

  public var name: String? {
    guard let path = self.path else {
      return nil
    }
    let (_, filename) = splitpath(path)
    return String(filename)
  }

  public var path: String? {
    return source.path
  }

  public var uuid: [UInt8]? {
    return codeview?.uuid
  }

  public var age: UInt32? {
    return codeview?.age
  }

}
