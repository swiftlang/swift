// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-as-library -Xfrontend -disable-availability-checking -g -o %t/ElfMalformed
// RUN: %target-run %t/ElfMalformed | %FileCheck %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: backtracing
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The ELF parsing code must cope with malformed input without trapping.

@_spi(ElfTest) import Runtime

struct Bytes {
  var data: [UInt8] = []

  mutating func u8(_ v: UInt8) { data.append(v) }
  mutating func u16(_ v: UInt16) { for i in 0..<2 { data.append(UInt8(truncatingIfNeeded: v >> (8 * UInt16(i)))) } }
  mutating func u32(_ v: UInt32) { for i in 0..<4 { data.append(UInt8(truncatingIfNeeded: v >> (8 * UInt32(i)))) } }
  mutating func u64(_ v: UInt64) { for i in 0..<8 { data.append(UInt8(truncatingIfNeeded: v >> (8 * UInt64(i)))) } }
  mutating func pad(to size: Int) { while data.count < size { data.append(0) } }
}

let PT_LOAD: UInt32 = 1
let PT_NOTE: UInt32 = 4
let PF_X: UInt32 = 1

struct Phdr {
  var type: UInt32
  var flags: UInt32 = 0
  var offset: UInt64 = 0
  var vaddr: UInt64 = 0
  var filesz: UInt64 = 0
  var memsz: UInt64 = 0
}

struct Shdr {
  var type: UInt32
  var offset: UInt64 = 0
  var size: UInt64 = 0
}

/// Build a little-endian 64-bit ELF file with the given headers.
func makeElf(phoff: UInt64 = 64, phentsize: UInt16 = 56,
             phnum: UInt16? = nil, phdrs: [Phdr] = [],
             shoff: UInt64? = nil, shentsize: UInt16 = 64,
             shnum: UInt16? = nil, shdrs: [Shdr] = [],
             shstrndx: UInt16 = 0, size: Int = 0) -> [UInt8] {
  // Unless told otherwise, include a null section header after the program
  // headers so that the image is otherwise valid
  var shdrs = shdrs
  var shoff = shoff ?? 64
  if shdrs.isEmpty && shnum == nil && shoff == 64 {
    shdrs = [Shdr(type: 0)]
    shoff = 256
  }

  var b = Bytes()
  b.data += [0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  b.u16(3)                              // e_type
  b.u16(62)                             // e_machine
  b.u32(1)                              // e_version
  b.u64(0)                              // e_entry
  b.u64(phoff)
  b.u64(shoff)
  b.u32(0)                              // e_flags
  b.u16(64)                             // e_ehsize
  b.u16(phentsize)
  b.u16(phnum ?? UInt16(phdrs.count))
  b.u16(shentsize)
  b.u16(shnum ?? UInt16(shdrs.count))
  b.u16(shstrndx)

  b.pad(to: Int(phoff))
  for p in phdrs {
    b.u32(p.type); b.u32(p.flags)
    b.u64(p.offset); b.u64(p.vaddr); b.u64(0)
    b.u64(p.filesz); b.u64(p.memsz); b.u64(0)
  }

  b.pad(to: Int(shoff))
  for s in shdrs {
    b.u32(0); b.u32(s.type); b.u64(0); b.u64(0)
    b.u64(s.offset); b.u64(s.size)
    b.u32(0); b.u32(0); b.u64(0); b.u64(0)
  }

  b.pad(to: size)
  return b.data
}

@main
struct ElfMalformed {
  static func check(_ name: String, _ bytes: [UInt8]) {
    print("\(name): \(testElfImage(bytes: bytes))")
  }

  static func main() {
    // The section name string table's offset + size overflows.
    // CHECK: shstrtab-overflow: image: loaded, uuid: none, info: endOfText=base+0
    check("shstrtab-overflow", makeElf(
      shoff: 64,
      shdrs: [Shdr(type: 0),
              Shdr(type: 1, offset: 0x6d00_0000_0000_0000,
                   size: 0xc000_0000_0000_0002)],
      shstrndx: 1))

    // A section's offset + size overflows.
    // CHECK: section-overflow: image: loaded, uuid: none, info: endOfText=base+0
    check("section-overflow", makeElf(
      shoff: 64,
      shdrs: [Shdr(type: 0),
              Shdr(type: 1, offset: 0, size: 64),
              Shdr(type: 1, offset: ~0, size: 2)],
      shstrndx: 1,
      size: 256))

    // A note's padding steps past the end of its segment.  The segment is
    // 12 bytes starting at an unaligned offset, holding a header with a
    // zero-length name and a one-byte descriptor.
    var note = Bytes()
    note.u32(0); note.u32(1); note.u32(3)
    var noteElf = makeElf(phdrs: [Phdr(type: PT_NOTE, offset: 121,
                                       vaddr: 121, filesz: 12, memsz: 12)],
                          size: 160)
    noteElf.replaceSubrange(121..<133, with: note.data)
    // CHECK: note-padding: image: loaded, uuid: none, info: endOfText=base+0
    check("note-padding", noteElf)

    // A note segment too small to hold a note header.
    // CHECK: short-note-segment: image: loaded, uuid: none, info: endOfText=base+0
    check("short-note-segment", makeElf(
      phdrs: [Phdr(type: PT_NOTE, offset: 1, vaddr: 1, filesz: 4, memsz: 4)],
      size: 160))

    // Segment extents that overflow are ignored.
    // CHECK: segment-overflow: image: loaded, uuid: none, info: endOfText=base+0
    check("segment-overflow", makeElf(
      phdrs: [Phdr(type: PT_LOAD, flags: PF_X,
                   vaddr: 0xffff_ffff_ffff_fff0, memsz: ~0),
              Phdr(type: PT_NOTE, vaddr: 0xffff_ffff_ffff_fff0, memsz: 0x100),
              Phdr(type: PT_NOTE, vaddr: 0x10, memsz: ~0)],
      size: 160))

    // A well-formed executable segment is still reported.
    // CHECK: good-segment: image: loaded, uuid: none, info: endOfText=base+128
    check("good-segment", makeElf(
      phdrs: [Phdr(type: PT_LOAD, flags: PF_X, vaddr: 0, memsz: 128)],
      size: 160))

    // A fixed-address (non-PIE) executable has absolute p_vaddr values, which
    // must be taken relative to the lowest PT_LOAD address.
    var buildId = Bytes()
    buildId.u32(4); buildId.u32(4); buildId.u32(3)
    buildId.data += [0x47, 0x4e, 0x55, 0, 0xde, 0xad, 0xbe, 0xef]
    var fixedElf = makeElf(
      phdrs: [Phdr(type: PT_LOAD, flags: PF_X, offset: 0,
                   vaddr: 0x400000, filesz: 320, memsz: 320),
              Phdr(type: PT_NOTE, offset: 192, vaddr: 0x4000c0,
                   filesz: 20, memsz: 20)],
      size: 320)
    fixedElf.replaceSubrange(192..<212, with: buildId.data)
    // CHECK: fixed-address: image: loaded, uuid: deadbeef, info: endOfText=base+320, info-uuid: deadbeef
    check("fixed-address", fixedElf)

    // Header entries that are too small overlap each other and are rejected.
    // CHECK: small-phentsize: image: rejected, info: none
    check("small-phentsize", makeElf(
      phoff: 0, phentsize: 1, phnum: 100, size: 160))

    // CHECK: small-shentsize: image: rejected, info: endOfText=base+0
    check("small-shentsize", makeElf(
      shoff: 0, shentsize: 0, shnum: 47104, shstrndx: 1, size: 160))
  }
}
