//===--- Elf.swift - ELF support for Swift --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines various ELF structures and provides types for working with ELF
// images on disk and in memory.
//
//===----------------------------------------------------------------------===//

import Swift

@_implementationOnly import OS.Libc
@_implementationOnly import ImageFormats.Elf

// .. Byte swapping ............................................................

extension Elf32_Ehdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Ehdr(
      e_ident: e_ident,
      e_type: Elf_Ehdr_Type(rawValue: e_type.rawValue.byteSwapped)!,
      e_machine: Elf_Ehdr_Machine(rawValue: e_machine.rawValue.byteSwapped)!,
      e_version: Elf_Ehdr_Version(rawValue: e_version.rawValue.byteSwapped)!,
      e_entry: e_entry.byteSwapped,
      e_phoff: e_phoff.byteSwapped,
      e_shoff: e_shoff.byteSwapped,
      e_flags: e_flags.byteSwapped,
      e_ehsize: e_ehsize.byteSwapped,
      e_phentsize: e_phentsize.byteSwapped,
      e_phnum: e_phnum.byteSwapped,
      e_shentsize: e_shentsize.byteSwapped,
      e_shnum: e_shnum.byteSwapped,
      e_shstrndx: e_shstrndx.byteSwapped
    )
  }
}

extension Elf64_Ehdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Ehdr(
      e_ident: e_ident,
      e_type: Elf_Ehdr_Type(rawValue: e_type.rawValue.byteSwapped)!,
      e_machine: Elf_Ehdr_Machine(rawValue: e_machine.rawValue.byteSwapped)!,
      e_version: Elf_Ehdr_Version(rawValue: e_version.rawValue.byteSwapped)!,
      e_entry: e_entry.byteSwapped,
      e_phoff: e_phoff.byteSwapped,
      e_shoff: e_shoff.byteSwapped,
      e_flags: e_flags.byteSwapped,
      e_ehsize: e_ehsize.byteSwapped,
      e_phentsize: e_phentsize.byteSwapped,
      e_phnum: e_phnum.byteSwapped,
      e_shentsize: e_shentsize.byteSwapped,
      e_shnum: e_shnum.byteSwapped,
      e_shstrndx: e_shstrndx.byteSwapped
    )
  }
}

extension Elf32_Shdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Shdr(
      sh_name: sh_name.byteSwapped,
      sh_type: Elf_Shdr_Type(rawValue: sh_type.rawValue.byteSwapped)!,
      sh_flags: sh_flags.byteSwapped,
      sh_addr: sh_addr.byteSwapped,
      sh_offset: sh_offset.byteSwapped,
      sh_size: sh_size.byteSwapped,
      sh_link: sh_link.byteSwapped,
      sh_info: sh_info.byteSwapped,
      sh_addralign: sh_addralign.byteSwapped,
      sh_entsize: sh_entsize.byteSwapped
    )
  }
}

extension Elf64_Shdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Shdr(
      sh_name: sh_name.byteSwapped,
      sh_type: Elf_Shdr_Type(rawValue: sh_type.rawValue.byteSwapped)!,
      sh_flags: sh_flags.byteSwapped,
      sh_addr: sh_addr.byteSwapped,
      sh_offset: sh_offset.byteSwapped,
      sh_size: sh_size.byteSwapped,
      sh_link: sh_link.byteSwapped,
      sh_info: sh_info.byteSwapped,
      sh_addralign: sh_addralign.byteSwapped,
      sh_entsize: sh_entsize.byteSwapped
    )
  }
}

extension Elf32_Chdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Chdr(
      ch_type: Elf_Chdr_Type(rawValue: ch_type.rawValue.byteSwapped)!,
      ch_size: ch_size.byteSwapped,
      ch_addralign: ch_addralign.byteSwapped
    )
  }
}

extension Elf64_Chdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Chdr(
      ch_type: Elf_Chdr_Type(rawValue: ch_type.rawValue.byteSwapped)!,
      ch_reserved: ch_reserved.byteSwapped,
      ch_size: ch_size.byteSwapped,
      ch_addralign: ch_addralign.byteSwapped
    )
  }
}

extension Elf32_Sym: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Sym(
      st_name: st_name.byteSwapped,
      st_value: st_value.byteSwapped,
      st_size: st_size.byteSwapped,
      st_info: st_info.byteSwapped,
      st_other: st_other.byteSwapped,
      st_shndx: st_shndx.byteSwapped
    )
  }
}

extension Elf64_Sym: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Sym(
      st_name: st_name.byteSwapped,
      st_info: st_info.byteSwapped,
      st_other: st_other.byteSwapped,
      st_shndx: st_shndx.byteSwapped,
      st_value: st_value.byteSwapped,
      st_size: st_size.byteSwapped
    )
  }
}

extension Elf32_Rel: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Rel(
      r_offset: r_offset.byteSwapped,
      r_info: r_info.byteSwapped
    )
  }
}

extension Elf32_Rela: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Rela(
      r_offset: r_offset.byteSwapped,
      r_info: r_info.byteSwapped,
      r_addend: r_addend.byteSwapped
    )
  }
}

extension Elf64_Rel: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Rel(
      r_offset: r_offset.byteSwapped,
      r_info: r_info.byteSwapped
    )
  }
}

extension Elf64_Rela: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Rela(
      r_offset: r_offset.byteSwapped,
      r_info: r_info.byteSwapped,
      r_addend: r_addend.byteSwapped
    )
  }
}

extension Elf32_Phdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Phdr(
      p_type: Elf_Phdr_Type(rawValue: p_type.rawValue.byteSwapped)!,
      p_offset: p_offset.byteSwapped,
      p_vaddr: p_vaddr.byteSwapped,
      p_paddr: p_paddr.byteSwapped,
      p_filesz: p_filesz.byteSwapped,
      p_memsz: p_memsz.byteSwapped,
      p_flags: p_flags.byteSwapped,
      p_align: p_align.byteSwapped
    )
  }
}

extension Elf64_Phdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Phdr(
      p_type: Elf_Phdr_Type(rawValue: p_type.rawValue.byteSwapped)!,
      p_flags: p_flags.byteSwapped,
      p_offset: p_offset.byteSwapped,
      p_vaddr: p_vaddr.byteSwapped,
      p_paddr: p_paddr.byteSwapped,
      p_filesz: p_filesz.byteSwapped,
      p_memsz: p_memsz.byteSwapped,
      p_align: p_align.byteSwapped
    )
  }
}

extension Elf32_Nhdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Nhdr(
      n_namesz: n_namesz.byteSwapped,
      n_descsz: n_descsz.byteSwapped,
      n_type: n_type.byteSwapped
    )
  }
}

extension Elf64_Nhdr: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Nhdr(
      n_namesz: n_namesz.byteSwapped,
      n_descsz: n_descsz.byteSwapped,
      n_type: n_type.byteSwapped
    )
  }
}

extension Elf32_Dyn: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Dyn(
      d_tag: d_tag.byteSwapped,
      d_un: .init(d_val: d_un.d_val.byteSwapped)
    )
  }
}

extension Elf64_Dyn: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Dyn(
      d_tag: d_tag.byteSwapped,
      d_un: .init(d_val: d_un.d_val.byteSwapped)
    )
  }
}

extension Elf32_Hash: ByteSwappable {
  var byteSwapped: Self {
    return Elf32_Hash(
      h_nbucket: h_nbucket.byteSwapped,
      h_nchain: h_nchain.byteSwapped
    )
  }
}

extension Elf64_Hash: ByteSwappable {
  var byteSwapped: Self {
    return Elf64_Hash(
      h_nbucket: h_nbucket.byteSwapped,
      h_nchain: h_nchain.byteSwapped
    )
  }
}

// .. Protocols ................................................................

internal typealias Elf_Magic = (UInt8, UInt8, UInt8, UInt8)
internal typealias Elf_Ident = (
  UInt8, UInt8, UInt8, UInt8,
  UInt8, UInt8, UInt8, UInt8,
  UInt8, UInt8, UInt8, UInt8,
  UInt8, UInt8, UInt8, UInt8
)

internal let ElfMagic: Elf_Magic = (0x7f, 0x45, 0x4c, 0x46)

internal protocol Elf_Ehdr : ByteSwappable {
  associatedtype Address: FixedWidthInteger
  associatedtype Offset: FixedWidthInteger

  init()

  var e_ident: Elf_Ident { get set }
  var ei_magic: Elf_Magic { get set }
  var ei_class: Elf_Ehdr_Class { get set }
  var ei_data: Elf_Ehdr_Data { get set }
  var ei_version: Elf_Byte { get set }
  var ei_osabi: Elf_Ehdr_OsAbi { get set }
  var ei_abiversion: Elf_Byte { get set }

  var e_type: Elf_Ehdr_Type { get set }
  var e_machine: Elf_Ehdr_Machine { get set }
  var e_version: Elf_Ehdr_Version { get set }
  var e_entry: Address { get set }
  var e_phoff: Offset { get set }
  var e_shoff: Offset { get set }
  var e_flags: Elf_Word { get set }
  var e_ehsize: Elf_Half { get set }
  var e_phentsize: Elf_Half { get set }
  var e_phnum: Elf_Half { get set }
  var e_shentsize: Elf_Half { get set }
  var e_shnum: Elf_Half { get set }
  var e_shstrndx: Elf_Half { get set }

  var shouldByteSwap: Bool { get }
}

extension Elf_Ehdr {
  var ei_magic: Elf_Magic {
    get {
      return (e_ident.0, e_ident.1, e_ident.2, e_ident.3)
    }
    set {
      e_ident.0 = newValue.0
      e_ident.1 = newValue.1
      e_ident.2 = newValue.2
      e_ident.3 = newValue.3
    }
  }
  var ei_class: Elf_Ehdr_Class {
    get {
      return Elf_Ehdr_Class(rawValue: e_ident.4)!
    }
    set {
      e_ident.4 = newValue.rawValue
    }
  }
  var ei_data: Elf_Ehdr_Data {
    get {
      return Elf_Ehdr_Data(rawValue: e_ident.5)!
    }
    set {
      e_ident.5 = newValue.rawValue
    }
  }
  var ei_version: UInt8 {
    get {
      return e_ident.6
    }
    set {
      e_ident.6 = newValue
    }
  }
  var ei_osabi: Elf_Ehdr_OsAbi {
    get {
      return Elf_Ehdr_OsAbi(rawValue: e_ident.7)!
    }
    set {
      e_ident.7 = newValue.rawValue
    }
  }
  var ei_abiversion: UInt8 {
    get {
      return e_ident.8
    }
    set {
      e_ident.8 = newValue
    }
  }
  var ei_pad: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8) {
    get {
      return (e_ident.9, e_ident.10, e_ident.11,
              e_ident.12, e_ident.13, e_ident.14,
              e_ident.15)
    }
    set {
      e_ident.9 = newValue.0
      e_ident.10 = newValue.1
      e_ident.11 = newValue.2
      e_ident.12 = newValue.3
      e_ident.13 = newValue.4
      e_ident.14 = newValue.5
      e_ident.15 = newValue.6
    }
  }

  var shouldByteSwap: Bool {
    #if _endian(big)
    return ei_data == .ELFDATA2LSB
    #else
    return ei_data == .ELFDATA2MSB
    #endif
  }
}

extension Elf32_Ehdr : Elf_Ehdr {
}

extension Elf64_Ehdr : Elf_Ehdr {
}

internal protocol Elf_Shdr : ByteSwappable {
  associatedtype Flags: FixedWidthInteger
  associatedtype Address: FixedWidthInteger
  associatedtype Offset: FixedWidthInteger
  associatedtype Size: FixedWidthInteger

  init()

  var sh_name: Elf_Word { get set }
  var sh_type: Elf_Shdr_Type { get set }
  var sh_flags: Flags { get set }
  var sh_addr: Address { get set }
  var sh_offset: Offset { get set }
  var sh_size: Size { get set }
  var sh_link: Elf_Word { get set }
  var sh_info: Elf_Word { get set }
  var sh_addralign: Size { get set }
  var sh_entsize: Size { get set }
}

extension Elf32_Shdr : Elf_Shdr {
}

extension Elf64_Shdr : Elf_Shdr {
}

internal protocol Elf_Phdr : ByteSwappable {
  associatedtype Address: FixedWidthInteger
  associatedtype Offset: FixedWidthInteger
  associatedtype Size: FixedWidthInteger

  init()

  var p_type: Elf_Phdr_Type { get set }
  var p_flags: Elf_Phdr_Flags { get set }
  var p_offset: Offset { get set }
  var p_vaddr: Address { get set }
  var p_paddr: Address { get set }
  var p_filesz: Size { get set }
  var p_memsz: Size { get set }
  var p_align: Size { get set }
}

extension Elf32_Phdr : Elf_Phdr {
}

extension Elf64_Phdr : Elf_Phdr {
}

internal protocol Elf_Nhdr : ByteSwappable {
  associatedtype Size: FixedWidthInteger
  associatedtype NoteType: FixedWidthInteger

  init()

  var n_namesz: Size { get set }
  var n_descsz: Size { get set }
  var n_type: NoteType { get set }
}

extension Elf32_Nhdr : Elf_Nhdr {
}

extension Elf64_Nhdr : Elf_Nhdr {
}

extension Elf32_Sym {
  var st_binding: Elf_Sym_Binding {
    get {
      return ELF32_ST_BIND(st_info)
    }
    set {
      st_info = ELF32_ST_INFO(newValue, ELF32_ST_TYPE(st_info))
    }
  }

  var st_type: Elf_Sym_Type {
    get {
      return ELF32_ST_TYPE(st_info)
    }
    set {
      st_info = ELF32_ST_INFO(ELF32_ST_BIND(st_info), newValue)
    }
  }

  var st_visibility: Elf_Sym_Visibility {
    get {
      return ELF32_ST_VISIBILITY(st_other)
    }
    set {
      st_other = (st_other & ~3) | newValue.rawValue
    }
  }
}

extension Elf64_Sym {
  var st_binding: Elf_Sym_Binding {
    get {
      return ELF64_ST_BIND(st_info)
    }
    set {
      st_info = ELF64_ST_INFO(newValue, ELF64_ST_TYPE(st_info))
    }
  }

  var st_type: Elf_Sym_Type {
    get {
      return ELF64_ST_TYPE(st_info)
    }
    set {
      st_info = ELF64_ST_INFO(ELF64_ST_BIND(st_info), newValue)
    }
  }

  var st_visibility: Elf_Sym_Visibility {
    get {
      return ELF64_ST_VISIBILITY(st_other)
    }
    set {
      st_other = (st_other & ~3) | newValue.rawValue
    }
  }
}

extension Elf32_Rel {
  var r_sym: Elf32_Byte {
    get {
      return ELF32_R_SYM(r_info)
    }
    set {
      r_info = ELF32_R_INFO(newValue, ELF32_R_TYPE(r_info))
    }
  }

  var r_type: Elf32_Byte {
    get {
      return ELF32_R_TYPE(r_info)
    }
    set {
      r_info = ELF32_R_INFO(ELF32_R_SYM(r_info), newValue)
    }
  }
}

extension Elf32_Rela {
  var r_sym: Elf32_Byte {
    get {
      return ELF32_R_SYM(r_info)
    }
    set {
      r_info = ELF32_R_INFO(newValue, ELF32_R_TYPE(r_info))
    }
  }

  var r_type: Elf32_Byte {
    get {
      return ELF32_R_TYPE(r_info)
    }
    set {
      r_info = ELF32_R_INFO(ELF32_R_SYM(r_info), newValue)
    }
  }
}

extension Elf64_Rel {
  var r_sym: Elf64_Word {
    get {
      return ELF64_R_SYM(r_info)
    }
    set {
      r_info = ELF64_R_INFO(newValue, ELF64_R_TYPE(r_info))
    }
  }

  var r_type: Elf64_Word {
    get {
      return ELF64_R_TYPE(r_info)
    }
    set {
      r_info = ELF64_R_INFO(ELF64_R_SYM(r_info), newValue)
    }
  }
}

extension Elf64_Rela {
  var r_sym: Elf64_Word {
    get {
      return ELF64_R_SYM(r_info)
    }
    set {
      r_info = ELF64_R_INFO(newValue, ELF64_R_TYPE(r_info))
    }
  }

  var r_type: Elf64_Word {
    get {
      return ELF64_R_TYPE(r_info)
    }
    set {
      r_info = ELF64_R_INFO(ELF64_R_SYM(r_info), newValue)
    }
  }
}

// .. Traits ...................................................................

internal protocol ElfTraits {
  associatedtype Ehdr: Elf_Ehdr
  associatedtype Phdr: Elf_Phdr
  associatedtype Shdr: Elf_Shdr
  associatedtype Nhdr: Elf_Nhdr

  static var elfClass: Elf_Ehdr_Class { get }
}

internal struct Elf32Traits: ElfTraits {
  typealias Ehdr = Elf32_Ehdr
  typealias Phdr = Elf32_Phdr
  typealias Shdr = Elf32_Shdr
  typealias Nhdr = Elf32_Nhdr

  static let elfClass: Elf_Ehdr_Class = .ELFCLASS32
}

internal struct Elf64Traits: ElfTraits {
  typealias Ehdr = Elf64_Ehdr
  typealias Phdr = Elf64_Phdr
  typealias Shdr = Elf64_Shdr
  typealias Nhdr = Elf64_Nhdr

  static let elfClass: Elf_Ehdr_Class = .ELFCLASS64
}

// .. ElfStringSection .........................................................

struct ElfStringSection {
  let bytes: [UInt8]

  func getStringAt(index: Int) -> String? {
    if index < 0 || index >= bytes.count {
      return nil
    }

    let slice = bytes[index...]
    var len: Int = 0
    slice.withUnsafeBufferPointer{ ptr in
      len = strnlen(ptr.baseAddress!, ptr.count)
    }
    return String(decoding: bytes[index..<index+len], as: UTF8.self)
  }
}

// .. ElfImage .................................................................

internal enum ElfImageError: Error {
  case notAnElfImage
  case wrongClass
  case badNoteName
  case badStringTableSectionIndex
}

internal class ElfImage<S: ImageSource, Traits: ElfTraits>: Image {
  typealias Source = S

  // This is arbitrary and it isn't in the spec
  let maxNoteNameLength = 256

  var baseAddress: S.Address
  var endAddress: S.Address

  var source: S
  var header: Traits.Ehdr
  var programHeaders: [Traits.Phdr]
  var sectionHeaders: [Traits.Shdr]?
  var shouldByteSwap: Bool { return header.shouldByteSwap }

  required init(source: S,
                       baseAddress: S.Address = 0,
                       endAddress: S.Address = 0) throws {
    self.source = source
    self.baseAddress = baseAddress
    self.endAddress = endAddress

    header = try source.fetch(from: 0, as: Traits.Ehdr.self)
    if header.ei_magic != ElfMagic {
      throw ElfImageError.notAnElfImage
    }

    if header.ei_class != Traits.elfClass {
      throw ElfImageError.wrongClass
    }

    if header.shouldByteSwap {
      header = header.byteSwapped
    }

    let byteSwap = header.shouldByteSwap
    func maybeSwap<T: ByteSwappable>(_ x: T) -> T {
      if byteSwap {
        return x.byteSwapped
      }
      return x
    }

    var phdrs: [Traits.Phdr] = []
    var phAddr = S.Address(header.e_phoff)
    for _ in 0..<header.e_phnum {
      let phdr = maybeSwap(try source.fetch(from: phAddr, as: Traits.Phdr.self))
      phdrs.append(phdr)
      phAddr += S.Address(header.e_phentsize)
    }
    programHeaders = phdrs

    if source.isMappedImage {
      sectionHeaders = nil
    } else {
      var shdrs: [Traits.Shdr] = []
      var shAddr = S.Address(header.e_shoff)
      for _ in 0..<header.e_shnum {
        let shdr = maybeSwap(try source.fetch(from: shAddr, as: Traits.Shdr.self))
        shdrs.append(shdr)
        shAddr += S.Address(header.e_shentsize)
      }
      sectionHeaders = shdrs
    }

    if header.e_shstrndx >= header.e_shnum {
      throw ElfImageError.badStringTableSectionIndex
    }
  }

  struct Note {
    let name: String
    let type: UInt32
    let desc: [UInt8]
  }

  struct Notes: Sequence {
    var image: ElfImage<S, Traits>

    struct NoteIterator: IteratorProtocol {
      var image: ElfImage<S, Traits>

      var hdrNdx = -1
      var noteAddr = S.Address()
      var noteEnd = S.Address()

      init(image: ElfImage<S, Traits>) {
        self.image = image
      }

      mutating func startHeader() {
        let ph = image.programHeaders[hdrNdx]

        if image.source.isMappedImage {
          noteAddr = S.Address(ph.p_vaddr)
          noteEnd = noteAddr + S.Address(ph.p_memsz)
        } else {
          noteAddr = S.Address(ph.p_offset)
          noteEnd = noteAddr + S.Address(ph.p_filesz)
        }
      }

      mutating func next() -> Note? {
        if hdrNdx >= image.programHeaders.count {
          return nil
        }
        while true {
          while noteAddr >= noteEnd {
            repeat {
              hdrNdx += 1
              if hdrNdx >= image.programHeaders.count {
                return nil
              }
            } while image.programHeaders[hdrNdx].p_type != .PT_NOTE
            startHeader()
          }

          do {
            let nhdr = try image.fetch(from: noteAddr, as: Traits.Nhdr.self)

            noteAddr += S.Address(MemoryLayout<Traits.Nhdr>.size)

            if noteEnd - noteAddr < nhdr.n_namesz {
              // The segment is probably corrupted
              noteAddr = noteEnd
              continue
            }

            let nameLen = nhdr.n_namesz > 0 ? nhdr.n_namesz - 1 : 0
            let nameBytes = try image.fetch(from: noteAddr,
                                            count: Int(nameLen),
                                            as: UInt8.self)
            let name = String(decoding: nameBytes, as: UTF8.self)

            noteAddr += S.Address(nhdr.n_namesz)
            if (noteAddr & 3) != 0 {
              noteAddr += 4 - (noteAddr & 3)
            }

            if noteEnd - noteAddr < nhdr.n_descsz {
              // The segment is probably corrupted
              noteAddr = noteEnd
              continue
            }

            let desc = try image.fetch(from: noteAddr,
                                       count: Int(nhdr.n_descsz),
                                       as: UInt8.self)

            noteAddr += S.Address(nhdr.n_descsz)
            if (noteAddr & 3) != 0 {
              noteAddr += 4 - (noteAddr & 3)
            }

            return Note(name: name, type: UInt32(nhdr.n_type), desc: desc)
          } catch {
            hdrNdx = image.programHeaders.count
            return nil
          }
        }
      }
    }

    func makeIterator() -> NoteIterator {
      return NoteIterator(image: image)
    }
  }

  var notes: Notes {
    return Notes(image: self)
  }

  var _uuid: [UInt8]?
  var uuid: [UInt8]? {
    if let uuid = _uuid {
      return uuid
    }

    for note in notes {
      if note.name == "GNU" && note.type == ImageFormats.NT_GNU_BUILD_ID {
        _uuid = note.desc
        return _uuid
      }
    }

    return nil
  }

  var _unwindInfo: UnwindInfo?
  var unwindInfo: UnwindInfo {
    if let unwindInfo = _unwindInfo {
      return unwindInfo
    }

    var unwindInfo = UnwindInfo()

    for phdr in programHeaders {
      if phdr.p_type == .PT_GNU_EH_FRAME {
        var ehFrameHdrRange: UnwindInfo.Range
        if source.isMappedImage {
          ehFrameHdrRange
            = UnwindInfo.Range(base: UnwindInfo.Address(phdr.p_vaddr),
                               size: UnwindInfo.Size(phdr.p_memsz))
        } else {
          ehFrameHdrRange
            = UnwindInfo.Range(base: UnwindInfo.Address(phdr.p_offset),
                               size: UnwindInfo.Size(phdr.p_filesz))
        }

        if (ehFrameHdrRange.size < MemoryLayout<EHFrameHdr>.size) {
          continue
        }

        guard let ehdr = try? fetch(from: S.Address(ehFrameHdrRange.base),
                                    as: EHFrameHdr.self) else {
          continue
        }

        if ehdr.version != 1 {
          continue
        }

        let pc = ehFrameHdrRange.base + UnwindInfo.Size(MemoryLayout<EHFrameHdr>.size)
        guard let (_, eh_frame_ptr) =
                try? source.fetchEHValue(from: S.Address(pc),
                                         with: ehdr.eh_frame_ptr_enc,
                                         pc: S.Address(pc)) else {
          continue
        }

        unwindInfo.ehFrameHdrSection = ehFrameHdrRange

        // The .eh_frame_hdr section doesn't specify the size of the
        // .eh_frame section, so we just rely on it being properly
        // terminated.  This does mean that bulk fetching the entire
        // thing isn't a good idea.
        unwindInfo.dwarfSection = UnwindInfo.Range(base: UnwindInfo.Address(eh_frame_ptr),
                                                   size: ~UnwindInfo.Size(0))
      }

      // ###TODO: Handle PT_ARM_EXIDX
    }

    if let sectionHeaders = sectionHeaders {
      let stringShdr = sectionHeaders[Int(header.e_shstrndx)]
      do {
        let bytes = try source.fetch(from: S.Address(stringShdr.sh_offset),
                                     count: Int(stringShdr.sh_size),
                                     as: UInt8.self)
        let stringSect = ElfStringSection(bytes: bytes)

        for shdr in sectionHeaders {
          guard let name = stringSect.getStringAt(index: Int(shdr.sh_name)) else {
            continue
          }

          if name == ".eh_frame" {
            unwindInfo.dwarfSection = UnwindInfo.Range(base: UnwindInfo.Address(shdr.sh_offset),
                                                       size: UnwindInfo.Size(shdr.sh_size))
          }
        }
      } catch {
      }
    }

    return unwindInfo
  }
}

internal typealias Elf32Image<S: ImageSource> = ElfImage<S, Elf32Traits>
internal typealias Elf64Image<S: ImageSource> = ElfImage<S, Elf64Traits>
