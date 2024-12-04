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

class ElfFile {
  public enum Error: Swift.Error {
    case FileOpenFailure(_ filePath: String)
    case FileReadFailure(_ filePath: String, offset: UInt64, size: UInt64)
    case FileNotElfFormat(_ filePath: String)
    case MalformedElfFile(_ filePath: String, description: String = "")
  }

  public typealias SymbolMap = [String: (start: UInt64, end: UInt64)]

  let filePath: String
  let file: FileHandle
  let ehdr: ElfEhdr
  let isElf64: Bool

  public init(filePath: String) throws {
    self.filePath = filePath

    guard let file = try? FileHandle(forReadingFrom: URL(fileURLWithPath: filePath)) else {
      throw Error.FileOpenFailure(filePath)
    }
    self.file = file

    let identLen = Int(EI_NIDENT)
    file.seek(toFileOffset: 0)
    guard let identData = try file.read(upToCount: identLen), identData.count == identLen else {
      file.closeFile()
      throw Error.FileReadFailure(filePath, offset: 0, size: UInt64(identLen))
    }

    let identMagic = String(bytes: identData.prefix(Int(SELFMAG)), encoding: .utf8)
    guard identMagic == ELFMAG else {
      file.closeFile()
      throw Error.FileNotElfFormat(filePath)
    }

    let identClass = identData[Int(EI_CLASS)]
    let isElf64 = identClass == ELFCLASS64
    guard isElf64 || identClass == ELFCLASS32 else {
      file.closeFile()
      throw Error.MalformedElfFile(filePath, description: "unsupported ELFCLASS: \(identClass)")
    }
    self.isElf64 = isElf64

    let ehdrSize = isElf64 ? Elf64_Ehdr.symbolSize : Elf32_Ehdr.symbolSize
    file.seek(toFileOffset: 0)
    guard let ehdrData = try file.read(upToCount: ehdrSize), ehdrData.count == ehdrSize else {
      file.closeFile()
      throw Error.FileReadFailure(filePath, offset: 0, size: UInt64(ehdrSize))
    }

    if isElf64 {
      self.ehdr = ehdrData.withUnsafeBytes { $0.load(as: Elf64_Ehdr.self) as ElfEhdr }
    } else {
      self.ehdr = ehdrData.withUnsafeBytes { $0.load(as: Elf32_Ehdr.self) as ElfEhdr }
    }
  }

  deinit { file.closeFile() }

  // returns a map of symbol names to their offset range in file (+ baseAddress)
  public func loadSymbols(baseAddress: UInt64 = 0) throws -> SymbolMap {
    guard let sectionCount = UInt(exactly: self.ehdr.shnum) else {
      throw Error.MalformedElfFile(
        self.filePath, description: "invalid ElfEhdr.e_shnum: \(self.ehdr.shnum)")
    }

    var symbols: SymbolMap = [:]
    for sectionIndex in 0..<sectionCount {
      let shdr: ElfShdr =
        isElf64
        ? try self.readShdr(index: sectionIndex) as Elf64_Shdr
        : try self.readShdr(index: sectionIndex) as Elf32_Shdr

      guard shdr.type == SHT_SYMTAB || shdr.type == SHT_DYNSYM else { continue }

      let sectionData: Data = try self.readSection(shdr: shdr)
      let symTable: [ElfSym] =
        self.isElf64
        ? sectionData.withUnsafeBytes { Array($0.bindMemory(to: Elf64_Sym.self)) }
        : sectionData.withUnsafeBytes { Array($0.bindMemory(to: Elf32_Sym.self)) }

      guard shdr.entsize == (self.isElf64 ? Elf64_Sym.symbolSize : Elf32_Sym.symbolSize) else {
        throw Error.MalformedElfFile(self.filePath, description: "invalid ElfShdr.sh_entsize")
      }

      // the link field in the section header for a symbol table section refers
      // to the index of the string table section containing the symbol names
      guard let linkIndex = UInt(exactly: shdr.link) else {
        throw Error.MalformedElfFile(
            self.filePath, description: "invalid ElfShdr.sh_link: \(shdr.link)")
      }

      let shdrLink: ElfShdr =
        isElf64
        ? try self.readShdr(index: UInt(linkIndex)) as Elf64_Shdr
        : try self.readShdr(index: UInt(linkIndex)) as Elf32_Shdr

      guard shdrLink.type == SHT_STRTAB else {
        throw Error.MalformedElfFile(self.filePath, description: "linked section not SHT_STRTAB")
      }

      // load the entire contents of the string table into memory
      let strTable: Data = try self.readSection(shdr: shdrLink)

      let symCount = Int(shdr.size / shdr.entsize)
      for symIndex in 0..<symCount {
        let sym = symTable[symIndex]
        guard sym.shndx != SHN_UNDEF, sym.value != 0, sym.size != 0 else { continue }

        // sym.name is a byte offset into the string table
        guard let strStart = Int(exactly: sym.name), strStart < strTable.count else {
          throw Error.MalformedElfFile(
            self.filePath, description: "invalid string table offset: \(sym.name)")
        }

        guard let strEnd = strTable[strStart...].firstIndex(of: 0),
          let symName = String(data: strTable[strStart..<strEnd], encoding: .utf8)
        else {
          throw Error.MalformedElfFile(
            self.filePath, description: "invalid string @ offset \(strStart)")
        }

        // rebase the symbol value on the base address provided by the caller
        let symStart = sym.value + baseAddress
        symbols[symName] = (start: symStart, end: symStart + sym.size)
      }
    }

    return symbols
  }

  // reads and returns the Elf32_Shdr or Elf64_Shdr at the specified index
  internal func readShdr<T: ElfShdr>(index: UInt) throws -> T {
    guard index < self.ehdr.shnum else {
      throw Error.MalformedElfFile(
        self.filePath, description: "section index \(index) >= ElfEhdr.e_shnum \(self.ehdr.shnum))")
    }

    let shdrSize = T.symbolSize
    guard shdrSize == self.ehdr.shentsize else {
      throw Error.MalformedElfFile(self.filePath, description: "ElfEhdr.e_shentsize != \(shdrSize)")
    }

    let shdrOffset: UInt64 = self.ehdr.shoff + UInt64(index) * UInt64(shdrSize)
    self.file.seek(toFileOffset: shdrOffset)
    guard let shdrData = try self.file.read(upToCount: shdrSize), shdrData.count == shdrSize else {
      throw Error.FileReadFailure(self.filePath, offset: shdrOffset, size: UInt64(shdrSize))
    }

    return shdrData.withUnsafeBytes { $0.load(as: T.self) as T }
  }

  // reads and returns all data in the specified section
  internal func readSection(shdr: ElfShdr) throws -> Data {
    guard let sectionSize = Int(exactly: shdr.size) else {
      throw Error.MalformedElfFile(
        self.filePath, description: "ElfShdr.sh_size too large \(shdr.size)")
    }

    let fileOffset = shdr.offset
    self.file.seek(toFileOffset: fileOffset)
    guard let data = try self.file.read(upToCount: sectionSize), data.count == sectionSize else {
      throw Error.FileReadFailure(self.filePath, offset: fileOffset, size: UInt64(sectionSize))
    }

    return data
  }
}
