//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import LinuxSystemHeaders

// TODO: replace this implementation with general purpose ELF parsing support
// currently private to swift/stdlib/public/Backtrace.
class ElfFile {
  public enum ELFError: Error {
    case readFailure(_ filePath: String, offset: UInt64, size: UInt64)
    case notELF64(_ filePath: String, _ description: String = "")
    case malformedFile(_ filePath: String, _ description: String = "")
  }

  public typealias SymbolMap = [String: (start: UInt64, end: UInt64)]

  let filePath: String
  let file: FileHandle
  let ehdr: Elf64_Ehdr

  public init(filePath: String) throws {
    self.filePath = filePath

    let file = try FileHandle(forReadingFrom: URL(fileURLWithPath: filePath))
    self.file = file

    let identLen = Int(EI_NIDENT)
    file.seek(toFileOffset: 0)
    guard let identData = try file.read(upToCount: identLen), identData.count == identLen else {
      file.closeFile()
      throw ELFError.readFailure(filePath, offset: 0, size: UInt64(identLen))
    }

    let identMagic = String(bytes: identData.prefix(Int(SELFMAG)), encoding: .utf8)
    guard identMagic == ELFMAG else {
      file.closeFile()
      throw ELFError.notELF64(filePath)
    }

    let identClass = identData[Int(EI_CLASS)]
    guard identClass == ELFCLASS64 else {
      throw ELFError.notELF64(filePath, "\(identClass) != ELFCLASS64")
    }

    let ehdrSize = MemoryLayout<Elf64_Ehdr>.size
    file.seek(toFileOffset: 0)
    guard let ehdrData = try file.read(upToCount: ehdrSize), ehdrData.count == ehdrSize else {
      file.closeFile()
      throw ELFError.readFailure(filePath, offset: 0, size: UInt64(ehdrSize))
    }

    self.ehdr = ehdrData.withUnsafeBytes { $0.load(as: Elf64_Ehdr.self) }
  }

  deinit { self.file.closeFile() }

  // returns a map of symbol names to their offset range in file (+ baseAddress)
  public func loadSymbols(baseAddress: UInt64 = 0) throws -> SymbolMap {
    guard let sectionCount = UInt(exactly: self.ehdr.e_shnum) else {
      throw ELFError.malformedFile(
        self.filePath, "invalid Elf64_Ehdr.e_shnum: \(self.ehdr.e_shnum)")
    }

    var symbols: SymbolMap = [:]
    for sectionIndex in 0..<sectionCount {
      let shdr: Elf64_Shdr = try self.readShdr(index: sectionIndex)
      guard shdr.sh_type == SHT_SYMTAB || shdr.sh_type == SHT_DYNSYM else { continue }

      let sectionData: Data = try self.readSection(shdr)
      let symTable: [Elf64_Sym] = sectionData.withUnsafeBytes { Array($0.bindMemory(to: Elf64_Sym.self)) }

      guard shdr.sh_entsize == MemoryLayout<Elf64_Sym>.size else {
        throw ELFError.malformedFile(self.filePath, "invalid Elf64_Shdr.sh_entsize")
      }

      // the link field in the section header for a symbol table section refers
      // to the index of the string table section containing the symbol names
      guard let linkIndex = UInt(exactly: shdr.sh_link) else {
        throw ELFError.malformedFile(self.filePath, "invalid Elf64_Shdr.sh_link: \(shdr.sh_link)")
      }

      let shdrLink: Elf64_Shdr = try self.readShdr(index: UInt(linkIndex))
      guard shdrLink.sh_type == SHT_STRTAB else {
        throw ELFError.malformedFile(self.filePath, "linked section not SHT_STRTAB")
      }

      // load the entire contents of the string table into memory
      let strTable: Data = try self.readSection(shdrLink)

      let symCount = Int(shdr.sh_size / shdr.sh_entsize)
      for symIndex in 0..<symCount {
        let sym = symTable[symIndex]
        guard sym.st_shndx != SHN_UNDEF, sym.st_value != 0, sym.st_size != 0 else { continue }

        // sym.st_name is a byte offset into the string table
        guard let strStart = Int(exactly: sym.st_name), strStart < strTable.count else {
          throw ELFError.malformedFile(self.filePath, "invalid string table offset: \(sym.st_name)")
        }

        guard let strEnd = strTable[strStart...].firstIndex(of: 0),
          let symName = String(data: strTable[strStart..<strEnd], encoding: .utf8)
        else {
          throw ELFError.malformedFile(self.filePath, "invalid string @ offset \(strStart)")
        }

        // rebase the symbol value on the base address provided by the caller
        let symStart = sym.st_value + baseAddress
        symbols[symName] = (start: symStart, end: symStart + sym.st_size)
      }
    }

    return symbols
  }

  // reads and returns the Elf64_Shdr at the specified index
  internal func readShdr(index: UInt) throws -> Elf64_Shdr {
    guard index < self.ehdr.e_shnum else {
      throw ELFError.malformedFile(
        self.filePath, "section index \(index) >= Elf64_Ehdr.e_shnum \(self.ehdr.e_shnum))")
    }

    let shdrSize = MemoryLayout<Elf64_Shdr>.size
    guard shdrSize == self.ehdr.e_shentsize else {
      throw ELFError.malformedFile(self.filePath, "Elf64_Ehdr.e_shentsize != \(shdrSize)")
    }

    let shdrOffset: UInt64 = self.ehdr.e_shoff + UInt64(index) * UInt64(shdrSize)
    self.file.seek(toFileOffset: shdrOffset)
    guard let shdrData = try self.file.read(upToCount: shdrSize), shdrData.count == shdrSize else {
      throw ELFError.readFailure(self.filePath, offset: shdrOffset, size: UInt64(shdrSize))
    }

    return shdrData.withUnsafeBytes { $0.load(as: Elf64_Shdr.self) }
  }

  // reads and returns all data in the specified section
  internal func readSection(_ shdr: Elf64_Shdr) throws -> Data {
    guard let sectionSize = Int(exactly: shdr.sh_size) else {
      throw ELFError.malformedFile(self.filePath, "Elf64_Shdr.sh_size too large \(shdr.sh_size)")
    }

    let fileOffset = shdr.sh_offset
    self.file.seek(toFileOffset: fileOffset)
    guard let data = try self.file.read(upToCount: sectionSize), data.count == sectionSize else {
      throw ELFError.readFailure(self.filePath, offset: fileOffset, size: UInt64(sectionSize))
    }

    return data
  }
}
