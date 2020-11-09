//===--- ReflectionContext.h - Swift Type Reflection Context ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements the context for reflection of values in the address space of a
// remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "llvm/BinaryFormat/COFF.h"
#include "llvm/BinaryFormat/MachO.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/Object/COFF.h"
#include "llvm/Support/Memory.h"

#include "swift/ABI/Enum.h"
#include "swift/ABI/ObjectFile.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/RuntimeInternals.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/Basic/Unreachable.h"

#include <set>
#include <unordered_map>
#include <utility>
#include <vector>

#include <inttypes.h>

namespace {

template <unsigned PointerSize> struct MachOTraits;

template <> struct MachOTraits<4> {
  using Header = const struct llvm::MachO::mach_header;
  using SegmentCmd = const struct llvm::MachO::segment_command;
  using Section = const struct llvm::MachO::section;
  static constexpr size_t MagicNumber = llvm::MachO::MH_MAGIC;
};

template <> struct MachOTraits<8> {
  using Header = const struct llvm::MachO::mach_header_64;
  using SegmentCmd = const struct llvm::MachO::segment_command_64;
  using Section = const struct llvm::MachO::section_64;
  static constexpr size_t MagicNumber = llvm::MachO::MH_MAGIC_64;
};

template <unsigned char ELFClass> struct ELFTraits;

template <> struct ELFTraits<llvm::ELF::ELFCLASS32> {
  using Header = const struct llvm::ELF::Elf32_Ehdr;
  using Section = const struct llvm::ELF::Elf32_Shdr;
  using Offset = llvm::ELF::Elf32_Off;
  using Size = llvm::ELF::Elf32_Word;
  static constexpr unsigned char ELFClass = llvm::ELF::ELFCLASS32;
};

template <> struct ELFTraits<llvm::ELF::ELFCLASS64> {
  using Header = const struct llvm::ELF::Elf64_Ehdr;
  using Section = const struct llvm::ELF::Elf64_Shdr;
  using Offset = llvm::ELF::Elf64_Off;
  using Size = llvm::ELF::Elf64_Xword;
  static constexpr unsigned char ELFClass = llvm::ELF::ELFCLASS64;
};

} // namespace

namespace swift {
namespace reflection {

using swift::remote::MemoryReader;
using swift::remote::RemoteAddress;

template <typename Runtime>
class ReflectionContext
       : public remote::MetadataReader<Runtime, TypeRefBuilder> {
  using super = remote::MetadataReader<Runtime, TypeRefBuilder>;
  using super::readMetadata;
  using super::readObjCClassName;

  std::unordered_map<typename super::StoredPointer, const TypeInfo *> Cache;

  /// All buffers we need to keep around long term. This will automatically free them
  /// when this object is destroyed.
  std::vector<MemoryReader::ReadBytesResult> savedBuffers;
  std::vector<std::tuple<RemoteAddress, RemoteAddress>> imageRanges;

public:
  using super::getBuilder;
  using super::readDemanglingForContextDescriptor;
  using super::readGenericArgFromMetadata;
  using super::readIsaMask;
  using super::readMetadataAndValueErrorExistential;
  using super::readMetadataAndValueOpaqueExistential;
  using super::readMetadataFromInstance;
  using super::readTypeFromMetadata;
  using super::stripSignedPointer;
  using typename super::StoredPointer;
  using typename super::StoredSignedPointer;
  using typename super::StoredSize;

  explicit ReflectionContext(std::shared_ptr<MemoryReader> reader)
    : super(std::move(reader), *this)
  {}

  ReflectionContext(const ReflectionContext &other) = delete;
  ReflectionContext &operator=(const ReflectionContext &other) = delete;
  
  MemoryReader &getReader() {
    return *this->Reader;
  }

  unsigned getSizeOfHeapObject() {
    // This must match sizeof(HeapObject) for the target.
    return sizeof(StoredPointer) * 2;
  }

  template <typename T> bool readMachOSections(RemoteAddress ImageStart) {
    auto Buf =
        this->getReader().readBytes(ImageStart, sizeof(typename T::Header));
    if (!Buf)
      return false;
    auto Header = reinterpret_cast<typename T::Header *>(Buf.get());
    assert(Header->magic == T::MagicNumber && "invalid MachO file");

    auto NumCommands = Header->sizeofcmds;

    // The layout of the executable is such that the commands immediately follow
    // the header.
    auto CmdStartAddress =
        RemoteAddress(ImageStart.getAddressData() + sizeof(typename T::Header));
    uint32_t SegmentCmdHdrSize = sizeof(typename T::SegmentCmd);
    uint64_t Offset = 0;

    // Find the __TEXT segment.
    typename T::SegmentCmd *Command = nullptr;
    for (unsigned I = 0; I < NumCommands; ++I) {
      auto CmdBuf = this->getReader().readBytes(
          RemoteAddress(CmdStartAddress.getAddressData() + Offset),
          SegmentCmdHdrSize);
      auto CmdHdr = reinterpret_cast<typename T::SegmentCmd *>(CmdBuf.get());
      if (strncmp(CmdHdr->segname, "__TEXT", sizeof(CmdHdr->segname)) == 0) {
        Command = CmdHdr;
        savedBuffers.push_back(std::move(CmdBuf));
        break;
      }
      Offset += CmdHdr->cmdsize;
    }

    // No __TEXT segment, bail out.
    if (!Command)
      return false;

   // Find the load command offset.
    auto loadCmdOffset = ImageStart.getAddressData() + Offset + sizeof(typename T::Header);

    // Read the load command.
    auto LoadCmdAddress = reinterpret_cast<const char *>(loadCmdOffset);
    auto LoadCmdBuf = this->getReader().readBytes(
        RemoteAddress(LoadCmdAddress), sizeof(typename T::SegmentCmd));
    auto LoadCmd = reinterpret_cast<typename T::SegmentCmd *>(LoadCmdBuf.get());

    // The sections start immediately after the load command.
    unsigned NumSect = LoadCmd->nsects;
    auto SectAddress = reinterpret_cast<const char *>(loadCmdOffset) +
                       sizeof(typename T::SegmentCmd);
    auto Sections = this->getReader().readBytes(
        RemoteAddress(SectAddress), NumSect * sizeof(typename T::Section));

    auto Slide = ImageStart.getAddressData() - Command->vmaddr;
    std::string Prefix = "__swift5";
    uint64_t RangeStart = UINT64_MAX;
    uint64_t RangeEnd = UINT64_MAX;
    auto SectionsBuf = reinterpret_cast<const char *>(Sections.get());
    for (unsigned I = 0; I < NumSect; ++I) {
      auto S = reinterpret_cast<typename T::Section *>(
          SectionsBuf + (I * sizeof(typename T::Section)));
      if (strncmp(S->sectname, Prefix.c_str(), strlen(Prefix.c_str())) != 0)
        continue;
      if (RangeStart == UINT64_MAX && RangeEnd == UINT64_MAX) {
        RangeStart = S->addr + Slide;
        RangeEnd = S->addr + S->size + Slide;
        continue;
      }
      RangeStart = std::min(RangeStart, (uint64_t)S->addr + Slide);
      RangeEnd = std::max(RangeEnd, (uint64_t)(S->addr + S->size + Slide));
      // Keep the range rounded to 8 byte alignment on both ends so we don't
      // introduce misaligned pointers mapping between local and remote
      // address space.
      RangeStart = RangeStart & ~7;
      RangeEnd = RangeEnd + 7 & ~7;      
    }
 
    if (RangeStart == UINT64_MAX && RangeEnd == UINT64_MAX)
      return false;

    auto SectBuf = this->getReader().readBytes(RemoteAddress(RangeStart),
                                               RangeEnd - RangeStart);

    auto findMachOSectionByName = [&](llvm::StringRef Name)
        -> std::pair<RemoteRef<void>, uint64_t> {
      for (unsigned I = 0; I < NumSect; ++I) {
        auto S = reinterpret_cast<typename T::Section *>(
            SectionsBuf + (I * sizeof(typename T::Section)));
        if (strncmp(S->sectname, Name.data(), strlen(Name.data())) != 0)
          continue;
        auto RemoteSecStart = S->addr + Slide;
        auto SectBufData = reinterpret_cast<const char *>(SectBuf.get());
        auto LocalSectStart =
            reinterpret_cast<const char *>(SectBufData + RemoteSecStart - RangeStart);
        
        auto StartRef = RemoteRef<void>(RemoteSecStart, LocalSectStart);
        return {StartRef, S->size};
      }
      return {nullptr, 0};
    };

    SwiftObjectFileFormatMachO ObjectFileFormat;
    auto FieldMdSec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::fieldmd));
    auto AssocTySec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::assocty));
    auto BuiltinTySec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::builtin));
    auto CaptureSec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::capture));
    auto TypeRefMdSec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::typeref));
    auto ReflStrMdSec = findMachOSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::reflstr));

    if (FieldMdSec.first == nullptr &&
        AssocTySec.first == nullptr &&
        BuiltinTySec.first == nullptr &&
        CaptureSec.first == nullptr &&
        TypeRefMdSec.first == nullptr &&
        ReflStrMdSec.first == nullptr)
      return false;

    ReflectionInfo info = {
        {FieldMdSec.first, FieldMdSec.second},
        {AssocTySec.first, AssocTySec.second},
        {BuiltinTySec.first, BuiltinTySec.second},
        {CaptureSec.first, CaptureSec.second},
        {TypeRefMdSec.first, TypeRefMdSec.second},
        {ReflStrMdSec.first, ReflStrMdSec.second}};

    this->addReflectionInfo(info);

    // Find the __DATA segment.
    for (unsigned I = 0; I < NumCommands; ++I) {
      auto CmdBuf = this->getReader().readBytes(
          RemoteAddress(CmdStartAddress.getAddressData() + Offset),
          SegmentCmdHdrSize);
      auto CmdHdr = reinterpret_cast<typename T::SegmentCmd *>(CmdBuf.get());
      if (strncmp(CmdHdr->segname, "__DATA", sizeof(CmdHdr->segname)) == 0) {
        auto DataSegmentEnd =
            ImageStart.getAddressData() + CmdHdr->vmaddr + CmdHdr->vmsize;
        assert(DataSegmentEnd > ImageStart.getAddressData() &&
               "invalid range for __DATA");
        imageRanges.push_back(
            std::make_tuple(ImageStart, RemoteAddress(DataSegmentEnd)));
        break;
      }
      Offset += CmdHdr->cmdsize;
    }

    savedBuffers.push_back(std::move(Buf));
    savedBuffers.push_back(std::move(SectBuf));
    savedBuffers.push_back(std::move(Sections));
    return true;
  }

  bool readPECOFFSections(RemoteAddress ImageStart) {
    auto DOSHdrBuf = this->getReader().readBytes(
        ImageStart, sizeof(llvm::object::dos_header));
    auto DOSHdr =
        reinterpret_cast<const llvm::object::dos_header *>(DOSHdrBuf.get());
    auto COFFFileHdrAddr = ImageStart.getAddressData() +
                           DOSHdr->AddressOfNewExeHeader +
                           sizeof(llvm::COFF::PEMagic);

    auto COFFFileHdrBuf = this->getReader().readBytes(
        RemoteAddress(COFFFileHdrAddr), sizeof(llvm::object::coff_file_header));
    auto COFFFileHdr = reinterpret_cast<const llvm::object::coff_file_header *>(
        COFFFileHdrBuf.get());

    auto SectionTableAddr = COFFFileHdrAddr +
                            sizeof(llvm::object::coff_file_header) +
                            COFFFileHdr->SizeOfOptionalHeader;
    auto SectionTableBuf = this->getReader().readBytes(
        RemoteAddress(SectionTableAddr),
        sizeof(llvm::object::coff_section) * COFFFileHdr->NumberOfSections);

    auto findCOFFSectionByName = [&](llvm::StringRef Name)
        -> std::pair<RemoteRef<void>, uint64_t> {
      for (size_t i = 0; i < COFFFileHdr->NumberOfSections; ++i) {
        const llvm::object::coff_section *COFFSec =
            reinterpret_cast<const llvm::object::coff_section *>(
                SectionTableBuf.get()) +
            i;
        llvm::StringRef SectionName =
            (COFFSec->Name[llvm::COFF::NameSize - 1] == 0)
                ? COFFSec->Name
                : llvm::StringRef(COFFSec->Name, llvm::COFF::NameSize);
        if (SectionName != Name)
          continue;
        auto Addr = ImageStart.getAddressData() + COFFSec->VirtualAddress;
        auto Buf = this->getReader().readBytes(RemoteAddress(Addr),
                                               COFFSec->VirtualSize);
        auto BufStart = Buf.get();
        savedBuffers.push_back(std::move(Buf));

        auto Begin = RemoteRef<void>(Addr, BufStart);
        auto Size = COFFSec->VirtualSize;
        
        // FIXME: This code needs to be cleaned up and updated
        // to make it work for 32 bit platforms.
        Begin = Begin.atByteOffset(8);
        Size -= 16;

        return {Begin, Size};
      }
      return {nullptr, 0};
    };

    SwiftObjectFileFormatCOFF ObjectFileFormat;
    auto FieldMdSec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::fieldmd));
    auto AssocTySec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::assocty));
    auto BuiltinTySec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::builtin));
    auto CaptureSec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::capture));
    auto TypeRefMdSec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::typeref));
    auto ReflStrMdSec = findCOFFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::reflstr));

    if (FieldMdSec.first == nullptr &&
        AssocTySec.first == nullptr &&
        BuiltinTySec.first == nullptr &&
        CaptureSec.first == nullptr &&
        TypeRefMdSec.first == nullptr &&
        ReflStrMdSec.first == nullptr)
      return false;

    ReflectionInfo Info = {
        {FieldMdSec.first, FieldMdSec.second},
        {AssocTySec.first, AssocTySec.second},
        {BuiltinTySec.first, BuiltinTySec.second},
        {CaptureSec.first, CaptureSec.second},
        {TypeRefMdSec.first, TypeRefMdSec.second},
        {ReflStrMdSec.first, ReflStrMdSec.second}};
    this->addReflectionInfo(Info);
    return true;
  }

  bool readPECOFF(RemoteAddress ImageStart) {
    auto Buf = this->getReader().readBytes(ImageStart,
                                           sizeof(llvm::object::dos_header));
    if (!Buf)
      return false;

    auto DOSHdr = reinterpret_cast<const llvm::object::dos_header *>(Buf.get());

    auto PEHeaderAddress =
        ImageStart.getAddressData() + DOSHdr->AddressOfNewExeHeader;

    Buf = this->getReader().readBytes(RemoteAddress(PEHeaderAddress),
                                      sizeof(llvm::COFF::PEMagic));
    if (!Buf)
      return false;

    if (memcmp(Buf.get(), llvm::COFF::PEMagic, sizeof(llvm::COFF::PEMagic)))
      return false;

    return readPECOFFSections(ImageStart);
  }

  template <typename T>
  bool readELFSections(RemoteAddress ImageStart,
                       llvm::Optional<llvm::sys::MemoryBlock> FileBuffer) {
    // When reading from the FileBuffer we can simply return a pointer to
    // the underlying data.
    // When reading from the process, we need to keep the memory around
    // until the end of the function, so we store it inside ReadDataBuffer.
    // We do this so in both cases we can return a simple pointer.
    std::vector<MemoryReader::ReadBytesResult> ReadDataBuffer;
    auto readData = [&](uint64_t Offset, uint64_t Size) -> const void * {
      if (FileBuffer.hasValue()) {
        auto Buffer = FileBuffer.getValue();
        if (Offset + Size > Buffer.allocatedSize())
          return nullptr;
        return (const void *)((uint64_t)Buffer.base() + Offset);
      } else {
        MemoryReader::ReadBytesResult Buf =
            this->getReader().readBytes(ImageStart + Offset, Size);
        if (!Buf)
          return nullptr;
        ReadDataBuffer.push_back(std::move(Buf));
        return ReadDataBuffer.back().get();
      }
    };

    const void *Buf = readData(0, sizeof(typename T::Header));
    if (!Buf)
      return false;
    auto Hdr = reinterpret_cast<const typename T::Header *>(Buf);
    assert(Hdr->getFileClass() == T::ELFClass && "invalid ELF file class");

    // From the header, grab information about the section header table.
    uint64_t SectionHdrAddress = Hdr->e_shoff;
    uint16_t SectionHdrNumEntries = Hdr->e_shnum;
    uint16_t SectionEntrySize = Hdr->e_shentsize;

    if (sizeof(typename T::Section) > SectionEntrySize)
      return false;
    if (SectionHdrNumEntries == 0)
      return false;

    // Collect all the section headers, we need them to look up the
    // reflection sections (by name) and the string table.
    // We read the section headers from the FileBuffer, since they are
    // not mapped in the child process.
    std::vector<const typename T::Section *> SecHdrVec;
    for (unsigned I = 0; I < SectionHdrNumEntries; ++I) {
      uint64_t Offset = SectionHdrAddress + (I * SectionEntrySize);
      auto SecBuf = readData(Offset, sizeof(typename T::Section));
      if (!SecBuf)
        return false;
      const typename T::Section *SecHdr =
          reinterpret_cast<const typename T::Section *>(SecBuf);

      SecHdrVec.push_back(SecHdr);
    }

    // This provides quick access to the section header string table index.
    // We also here handle the unlikely even where the section index overflows
    // and it's just a pointer to secondary storage (SHN_XINDEX).
    uint32_t SecIdx = Hdr->e_shstrndx;
    if (SecIdx == llvm::ELF::SHN_XINDEX) {
      assert(!SecHdrVec.empty() && "malformed ELF object");
      SecIdx = SecHdrVec[0]->sh_link;
    }

    assert(SecIdx < SecHdrVec.size() && "malformed ELF object");

    const typename T::Section *SecHdrStrTab = SecHdrVec[SecIdx];
    typename T::Offset StrTabOffset = SecHdrStrTab->sh_offset;
    typename T::Size StrTabSize = SecHdrStrTab->sh_size;

    auto StrTabBuf = readData(StrTabOffset, StrTabSize);
    if (!StrTabBuf)
      return false;
    auto StrTab = reinterpret_cast<const char *>(StrTabBuf);
    bool Error = false;
    auto findELFSectionByName =
        [&](llvm::StringRef Name) -> std::pair<RemoteRef<void>, uint64_t> {
          if (Error)
            return {nullptr, 0};
          // Now for all the sections, find their name.
          for (const typename T::Section *Hdr : SecHdrVec) {
            uint32_t Offset = Hdr->sh_name;
            const char *Start = (const char *)StrTab + Offset;
            uint64_t StringSize = strnlen(Start, StrTabSize - Offset);
            if (StringSize > StrTabSize - Offset) {
              Error = true;
              break;
            }
            std::string SecName(Start, StringSize);
            if (SecName != Name)
              continue;
            RemoteAddress SecStart =
                RemoteAddress(ImageStart.getAddressData() + Hdr->sh_addr);
            auto SecSize = Hdr->sh_size;
            MemoryReader::ReadBytesResult SecBuf;
            if (FileBuffer.hasValue()) {
              // sh_offset gives us the offset to the section in the file,
              // while sh_addr gives us the offset in the process.
              auto Offset = Hdr->sh_offset;
              if (FileBuffer->allocatedSize() < Offset + SecSize) {
                Error = true;
                break;
              }
              auto *Buf = malloc(SecSize);
              SecBuf = MemoryReader::ReadBytesResult(
                  Buf, [](const void *ptr) { free(const_cast<void *>(ptr)); });
              memcpy((void *)Buf,
                     (const void *)((uint64_t)FileBuffer->base() + Offset),
                     SecSize);
            } else {
              SecBuf = this->getReader().readBytes(SecStart, SecSize);
            }
            auto SecContents =
                RemoteRef<void>(SecStart.getAddressData(), SecBuf.get());
            savedBuffers.push_back(std::move(SecBuf));
            return {SecContents, SecSize};
          }
          return {nullptr, 0};
        };

    SwiftObjectFileFormatELF ObjectFileFormat;
    auto FieldMdSec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::fieldmd));
    auto AssocTySec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::assocty));
    auto BuiltinTySec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::builtin));
    auto CaptureSec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::capture));
    auto TypeRefMdSec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::typeref));
    auto ReflStrMdSec = findELFSectionByName(
        ObjectFileFormat.getSectionName(ReflectionSectionKind::reflstr));

    if (Error)
      return false;

    // We succeed if at least one of the sections is present in the
    // ELF executable.
    if (FieldMdSec.first == nullptr &&
        AssocTySec.first == nullptr &&
        BuiltinTySec.first == nullptr &&
        CaptureSec.first == nullptr &&
        TypeRefMdSec.first == nullptr &&
        ReflStrMdSec.first == nullptr)
      return false;

    ReflectionInfo info = {
        {FieldMdSec.first, FieldMdSec.second},
        {AssocTySec.first, AssocTySec.second},
        {BuiltinTySec.first, BuiltinTySec.second},
        {CaptureSec.first, CaptureSec.second},
        {TypeRefMdSec.first, TypeRefMdSec.second},
        {ReflStrMdSec.first, ReflStrMdSec.second}};

    this->addReflectionInfo(info);
    return true;
  }

  /// Parses metadata information from an ELF image. Because the Section
  /// Header Table maybe be missing (for example, when reading from a 
  /// process) this method optionally receives a buffer with the contents
  /// of the image's file, from where it will the necessary information.
  ///
  ///
  /// \param[in] ImageStart
  ///     A remote address pointing to the start of the image in the running
  ///     process.
  ///
  /// \param[in] FileBuffer
  ///     A buffer which contains the contents of the image's file
  ///     in disk. If missing, all the information will be read using the
  ///     instance's memory reader.
  ///
  /// \return
  ///     /b True if the metadata information was parsed successfully,
  ///     /b false otherwise.
  bool readELF(RemoteAddress ImageStart, llvm::Optional<llvm::sys::MemoryBlock> FileBuffer) {
    auto Buf =
        this->getReader().readBytes(ImageStart, sizeof(llvm::ELF::Elf64_Ehdr));

    // Read the header.
    auto Hdr = reinterpret_cast<const llvm::ELF::Elf64_Ehdr *>(Buf.get());

    if (!Hdr->checkMagic())
      return false;

    // Check if we have a ELFCLASS32 or ELFCLASS64
    unsigned char FileClass = Hdr->getFileClass();
    if (FileClass == llvm::ELF::ELFCLASS64) {
      return readELFSections<ELFTraits<llvm::ELF::ELFCLASS64>>(
          ImageStart, FileBuffer);
    } else if (FileClass == llvm::ELF::ELFCLASS32) {
      return readELFSections<ELFTraits<llvm::ELF::ELFCLASS32>>(
          ImageStart, FileBuffer);
    } else {
      return false;
    }
  }

  bool addImage(RemoteAddress ImageStart) {
    // Read the first few bytes to look for a magic header.
    auto Magic = this->getReader().readBytes(ImageStart, sizeof(uint32_t));
    if (!Magic)
      return false;
    
    uint32_t MagicWord;
    memcpy(&MagicWord, Magic.get(), sizeof(MagicWord));
    
    // 32- and 64-bit Mach-O.
    if (MagicWord == llvm::MachO::MH_MAGIC) {
      return readMachOSections<MachOTraits<4>>(ImageStart);
    }
    
    if (MagicWord == llvm::MachO::MH_MAGIC_64) {
      return readMachOSections<MachOTraits<8>>(ImageStart);
    }
    
    // PE. (This just checks for the DOS header; `readPECOFF` will further
    // validate the existence of the PE header.)
    auto MagicBytes = (const char*)Magic.get();
    if (MagicBytes[0] == 'M' && MagicBytes[1] == 'Z') {
      return readPECOFF(ImageStart);
    }


    // ELF.
    if (MagicBytes[0] == llvm::ELF::ElfMagic[0]
        && MagicBytes[1] == llvm::ELF::ElfMagic[1]
        && MagicBytes[2] == llvm::ELF::ElfMagic[2]
        && MagicBytes[3] == llvm::ELF::ElfMagic[3]) {
      return readELF(ImageStart, llvm::Optional<llvm::sys::MemoryBlock>());
    }

    // We don't recognize the format.
    return false;
  }

  void addReflectionInfo(ReflectionInfo I) {
    getBuilder().addReflectionInfo(I);
  }

  bool ownsObject(RemoteAddress ObjectAddress) {
    auto MetadataAddress = readMetadataFromInstance(ObjectAddress.getAddressData());
    if (!MetadataAddress)
      return true;
    return ownsAddress(RemoteAddress(*MetadataAddress));
  }
  
  /// Returns true if the address falls within a registered image.
  bool ownsAddressRaw(RemoteAddress Address) {
    for (auto Range : imageRanges) {
      auto Start = std::get<0>(Range);
      auto End = std::get<1>(Range);
      if (Start.getAddressData() <= Address.getAddressData()
          && Address.getAddressData() < End.getAddressData())
        return true;
    }

    return false;
  }

  /// Returns true if the address is known to the reflection context.
  /// Currently, that means that either the address falls within a registered
  /// image, or the address points to a Metadata whose type context descriptor
  /// is within a registered image.
  bool ownsAddress(RemoteAddress Address) {
    if (ownsAddressRaw(Address))
      return true;

    // This is usually called on a Metadata address which might have been
    // on the heap. Try reading it and looking up its type context descriptor
    // instead.
    if (auto Metadata = readMetadata(Address.getAddressData()))
      if (auto DescriptorAddress =
          super::readAddressOfNominalTypeDescriptor(Metadata, true))
        if (ownsAddressRaw(RemoteAddress(DescriptorAddress)))
          return true;

    return false;
  }
  
  /// Return a description of the layout of a class instance with the given
  /// metadata as its isa pointer.
  const TypeInfo *
  getMetadataTypeInfo(StoredPointer MetadataAddress,
                      remote::TypeInfoProvider *ExternalTypeInfo) {
    // See if we cached the layout already
    auto found = Cache.find(MetadataAddress);
    if (found != Cache.end())
      return found->second;

    auto &TC = getBuilder().getTypeConverter();

    const TypeInfo *TI = nullptr;

    auto TR = readTypeFromMetadata(MetadataAddress);
    auto kind = this->readKindFromMetadata(MetadataAddress);
    if (TR != nullptr && kind) {
      switch (*kind) {
      case MetadataKind::Class: {
        // Figure out where the stored properties of this class begin
        // by looking at the size of the superclass
        auto start =
            this->readInstanceStartAndAlignmentFromClassMetadata(MetadataAddress);

        // Perform layout
        if (start)
          TI = TC.getClassInstanceTypeInfo(TR, *start, ExternalTypeInfo);

        break;
      }
      default:
        break;
      }
    }

    // Cache the result for future lookups
    Cache[MetadataAddress] = TI;
    return TI;
  }

  /// Return a description of the layout of a class instance with the given
  /// metadata as its isa pointer.
  const TypeInfo *
  getInstanceTypeInfo(StoredPointer ObjectAddress,
                      remote::TypeInfoProvider *ExternalTypeInfo) {
    auto MetadataAddress = readMetadataFromInstance(ObjectAddress);
    if (!MetadataAddress)
      return nullptr;

    auto kind = this->readKindFromMetadata(*MetadataAddress);
    if (!kind)
      return nullptr;

    switch (*kind) {
    case MetadataKind::Class:
      return getMetadataTypeInfo(*MetadataAddress, ExternalTypeInfo);

    case MetadataKind::HeapLocalVariable: {
      auto CDAddr = this->readCaptureDescriptorFromMetadata(*MetadataAddress);
      if (!CDAddr)
        return nullptr;
      if (!CDAddr->isResolved())
        return nullptr;

      // FIXME: Non-generic SIL boxes also use the HeapLocalVariable metadata
      // kind, but with a null capture descriptor right now (see
      // FixedBoxTypeInfoBase::allocate).
      //
      // Non-generic SIL boxes share metadata among types with compatible
      // layout, but we need some way to get an outgoing pointer map for them.
      auto CD = getBuilder().getCaptureDescriptor(
                                CDAddr->getResolvedAddress().getAddressData());
      if (CD == nullptr)
        return nullptr;

      auto Info = getBuilder().getClosureContextInfo(CD);

      return getClosureContextInfo(ObjectAddress, Info, ExternalTypeInfo);
    }

    case MetadataKind::HeapGenericLocalVariable: {
      // Generic SIL @box type - there is always an instantiated metadata
      // pointer for the boxed type.
      if (auto Meta = readMetadata(*MetadataAddress)) {
        auto GenericHeapMeta =
          cast<TargetGenericBoxHeapMetadata<Runtime>>(Meta.getLocalBuffer());
        return getMetadataTypeInfo(GenericHeapMeta->BoxedType,
                                   ExternalTypeInfo);
      }
      return nullptr;
    }

    case MetadataKind::ErrorObject:
      // Error boxed existential on non-Objective-C runtime target
      return nullptr;

    default:
      return nullptr;
    }
  }

  bool projectExistential(RemoteAddress ExistentialAddress,
                          const TypeRef *ExistentialTR,
                          const TypeRef **OutInstanceTR,
                          RemoteAddress *OutInstanceAddress,
                          remote::TypeInfoProvider *ExternalTypeInfo) {
    if (ExistentialTR == nullptr)
      return false;

    auto ExistentialTI = getTypeInfo(ExistentialTR, ExternalTypeInfo);
    if (ExistentialTI == nullptr)
      return false;

    auto ExistentialRecordTI = dyn_cast<const RecordTypeInfo>(ExistentialTI);
    if (ExistentialRecordTI == nullptr)
      return false;

    switch (ExistentialRecordTI->getRecordKind()) {
    // Class existentials have trivial layout.
    // It is itself the pointer to the instance followed by the witness tables.
    case RecordKind::ClassExistential:
      // This is just AnyObject.
      *OutInstanceTR = ExistentialRecordTI->getFields()[0].TR;
      *OutInstanceAddress = ExistentialAddress;
      return true;

    case RecordKind::OpaqueExistential: {
      auto OptMetaAndValue =
          readMetadataAndValueOpaqueExistential(ExistentialAddress);
      if (!OptMetaAndValue)
        return false;

      auto InstanceTR = readTypeFromMetadata(
          OptMetaAndValue->MetadataAddress.getAddressData());
      if (!InstanceTR)
        return false;

      *OutInstanceTR = InstanceTR;
      *OutInstanceAddress = OptMetaAndValue->PayloadAddress;
      return true;
    }
    case RecordKind::ErrorExistential: {
      auto OptMetaAndValue =
          readMetadataAndValueErrorExistential(ExistentialAddress);
      if (!OptMetaAndValue)
        return false;

      // FIXME: Check third value, 'IsBridgedError'

      auto InstanceTR = readTypeFromMetadata(
          OptMetaAndValue->MetadataAddress.getAddressData());
      if (!InstanceTR)
        return false;

      *OutInstanceTR = InstanceTR;
      *OutInstanceAddress = OptMetaAndValue->PayloadAddress;
      return true;
    }
    default:
      return false;
    }
  }

  /// Projects the value of an enum.
  ///
  /// Takes the address and typeref for an enum and determines the
  /// index of the currently-selected case within the enum.
  /// You can use this index with `swift_reflection_childOfTypeRef`
  /// to get detailed information about the specific case.
  ///
  /// Returns true if the enum case could be successfully determined.  In
  /// particular, note that this code may return false for valid in-memory data
  /// if the compiler used a strategy we do not yet understand.
  bool projectEnumValue(RemoteAddress EnumAddress, const TypeRef *EnumTR,
                        int *CaseIndex,
                        remote::TypeInfoProvider *ExternalTypeInfo) {
    // Get the TypeInfo and sanity-check it
    if (EnumTR == nullptr) {
      return false;
    }
    auto TI = getTypeInfo(EnumTR, ExternalTypeInfo);
    if (TI == nullptr) {
      return false;
    }
    auto EnumTI = dyn_cast<const EnumTypeInfo>(TI);
    if (EnumTI == nullptr){
      return false;
    }
    return EnumTI->projectEnumValue(getReader(), EnumAddress, CaseIndex);
  }

  /// Return a description of the layout of a value with the given type.
  const TypeInfo *getTypeInfo(const TypeRef *TR,
                              remote::TypeInfoProvider *ExternalTypeInfo) {
    if (TR == nullptr) {
      return nullptr;
    } else {
      return getBuilder().getTypeConverter().getTypeInfo(TR, ExternalTypeInfo);
    }
  }

  /// Iterate the protocol conformance cache tree rooted at NodePtr, calling
  /// Call with the type and protocol in each node.
  void iterateConformanceTree(StoredPointer NodePtr,
    std::function<void(StoredPointer Type, StoredPointer Proto)> Call) {
    if (!NodePtr)
      return;
    auto NodeBytes = getReader().readBytes(RemoteAddress(NodePtr),
                                           sizeof(ConformanceNode<Runtime>));
    auto NodeData =
      reinterpret_cast<const ConformanceNode<Runtime> *>(NodeBytes.get());
    if (!NodeData)
      return;
    Call(NodeData->Type, NodeData->Proto);
    iterateConformanceTree(NodeData->Left, Call);
    iterateConformanceTree(NodeData->Right, Call);
  }

  void IterateConformanceTable(
      RemoteAddress ConformancesPtr,
      std::function<void(StoredPointer Type, StoredPointer Proto)> Call) {
    auto MapBytes = getReader().readBytes(RemoteAddress(ConformancesPtr),
                                          sizeof(ConcurrentHashMap<Runtime>));
    auto MapData =
        reinterpret_cast<const ConcurrentHashMap<Runtime> *>(MapBytes.get());
    if (!MapData)
      return;

    auto Count = MapData->ElementCount;
    auto Size = Count * sizeof(ConformanceCacheEntry<Runtime>);

    auto ElementsBytes =
        getReader().readBytes(RemoteAddress(MapData->Elements), Size);
    auto ElementsData =
        reinterpret_cast<const ConformanceCacheEntry<Runtime> *>(
            ElementsBytes.get());
    if (!ElementsData)
      return;

    for (StoredSize i = 0; i < Count; i++) {
      auto &Element = ElementsData[i];
      Call(Element.Type, Element.Proto);
    }
  }

  /// Iterate the protocol conformance cache in the target process, calling Call
  /// with the type and protocol of each conformance. Returns None on success,
  /// and a string describing the error on failure.
  llvm::Optional<std::string> iterateConformances(
    std::function<void(StoredPointer Type, StoredPointer Proto)> Call) {
    std::string ConformancesPointerName =
        "_swift_debug_protocolConformanceStatePointer";
    auto ConformancesAddrAddr =
      getReader().getSymbolAddress(ConformancesPointerName);
    if (!ConformancesAddrAddr)
      return "unable to look up debug variable " + ConformancesPointerName;

    auto ConformancesAddr =
      getReader().readPointer(ConformancesAddrAddr, sizeof(StoredPointer));
    if (!ConformancesAddr)
      return "unable to read value of " + ConformancesPointerName;

    auto Root = getReader().readPointer(ConformancesAddr->getResolvedAddress(),
                                        sizeof(StoredPointer));
    auto ReaderCount = Root->getResolvedAddress().getAddressData();

    // ReaderCount will be the root pointer if the conformance cache is a
    // ConcurrentMap. It's very unlikely that there would ever be more readers
    // than the least valid pointer value, so compare with that to distinguish.
    // TODO: once the old conformance cache is gone for good, remove that code.
    uint64_t LeastValidPointerValue;
    if (!getReader().queryDataLayout(
            DataLayoutQueryType::DLQ_GetLeastValidPointerValue, nullptr,
            &LeastValidPointerValue)) {
      return std::string("unable to query least valid pointer value");
    }

    if (ReaderCount < LeastValidPointerValue)
      IterateConformanceTable(ConformancesAddr->getResolvedAddress(), Call);
    else {
      // The old code has the root address at this location.
      auto RootAddr = ReaderCount;
      iterateConformanceTree(RootAddr, Call);
    }
    return llvm::None;
  }
  
  /// Fetch the metadata pointer from a metadata allocation, or 0 if this
  /// allocation's tag is not handled or an error occurred.
  StoredPointer allocationMetadataPointer(
    MetadataAllocation<Runtime> Allocation) {
    if (Allocation.Tag == GenericMetadataCacheTag) {
        struct GenericMetadataCacheEntry {
          StoredPointer Left, Right;
          StoredPointer LockedStorage;
          uint8_t LockedStorageKind;
          uint8_t TrackingInfo;
          uint16_t NumKeyParameters;
          uint16_t NumWitnessTables;
          uint32_t Hash;
          StoredPointer Value;
        };
        auto AllocationBytes =
          getReader().readBytes(RemoteAddress(Allocation.Ptr),
                                              Allocation.Size);
        auto Entry = reinterpret_cast<const GenericMetadataCacheEntry *>(
          AllocationBytes.get());
        if (!Entry)
          return 0;
        return Entry->Value;
    }
    return 0;
  }

  /// Get the name of a metadata tag, if known.
  llvm::Optional<std::string> metadataAllocationTagName(int Tag) {
    switch (Tag) {
#define TAG(name, value)                                                       \
  case value:                                                                  \
    return std::string(#name);
#include "../../../stdlib/public/runtime/MetadataAllocatorTags.def"
    default:
      return llvm::None;
    }
  }

  llvm::Optional<MetadataCacheNode<Runtime>>
  metadataAllocationCacheNode(MetadataAllocation<Runtime> Allocation) {
    switch (Allocation.Tag) {
    case BoxesTag:
    case ObjCClassWrappersTag:
    case FunctionTypesTag:
    case MetatypeTypesTag:
    case ExistentialMetatypeValueWitnessTablesTag:
    case ExistentialMetatypesTag:
    case ExistentialTypesTag:
    case OpaqueExistentialValueWitnessTablesTag:
    case ClassExistentialValueWitnessTablesTag:
    case ForeignWitnessTablesTag:
    case TupleCacheTag:
    case GenericMetadataCacheTag:
    case ForeignMetadataCacheTag:
    case GenericWitnessTableCacheTag: {
      auto NodeBytes = getReader().readBytes(
          RemoteAddress(Allocation.Ptr), sizeof(MetadataCacheNode<Runtime>));
      auto Node =
          reinterpret_cast<const MetadataCacheNode<Runtime> *>(NodeBytes.get());
      if (!Node)
        return llvm::None;
      return *Node;
    }
    default:
      return llvm::None;
    }
  }

  /// Iterate the metadata allocations in the target process, calling Call with
  /// each allocation found. Returns None on success, and a string describing
  /// the error on failure.
  llvm::Optional<std::string> iterateMetadataAllocations(
    std::function<void (MetadataAllocation<Runtime>)> Call) {
    std::string IterationEnabledName =
        "_swift_debug_metadataAllocationIterationEnabled";
    std::string AllocationPoolPointerName =
        "_swift_debug_allocationPoolPointer";

    auto IterationEnabledAddr =
      getReader().getSymbolAddress(IterationEnabledName);
    if (!IterationEnabledAddr)
      return "unable to look up debug variable " + IterationEnabledName;
    char IterationEnabled;
    if (!getReader().readInteger(IterationEnabledAddr, &IterationEnabled))
      return "failed to read value of " + IterationEnabledName;
    if (!IterationEnabled)
      return std::string("remote process does not have metadata allocation "
                         "iteration enabled");

    auto AllocationPoolAddrAddr =
      getReader().getSymbolAddress(AllocationPoolPointerName);
    if (!AllocationPoolAddrAddr)
      return "unable to look up debug variable " + AllocationPoolPointerName;
    auto AllocationPoolAddr =
      getReader().readPointer(AllocationPoolAddrAddr, sizeof(StoredPointer));
    if (!AllocationPoolAddr)
      return "failed to read value of " + AllocationPoolPointerName;
    
    struct PoolRange {
      StoredPointer Begin;
      StoredSize Remaining;
    };
    struct PoolTrailer {
      StoredPointer PrevTrailer;
      StoredSize PoolSize;
    };
    struct alignas(StoredPointer) AllocationHeader {
      uint16_t Size;
      uint16_t Tag;
    };

    auto PoolBytes = getReader()
      .readBytes(AllocationPoolAddr->getResolvedAddress(), sizeof(PoolRange));
    auto Pool = reinterpret_cast<const PoolRange *>(PoolBytes.get());
    if (!Pool)
      return std::string("failure reading allocation pool contents");

    auto TrailerPtr = Pool->Begin + Pool->Remaining;
    while (TrailerPtr) {
      auto TrailerBytes = getReader()
        .readBytes(RemoteAddress(TrailerPtr), sizeof(PoolTrailer));
      auto Trailer = reinterpret_cast<const PoolTrailer *>(TrailerBytes.get());
      if (!Trailer)
        break;
      auto PoolStart = TrailerPtr - Trailer->PoolSize;
      auto PoolBytes = getReader()
        .readBytes(RemoteAddress(PoolStart), Trailer->PoolSize);
      auto PoolPtr = (const char *)PoolBytes.get();
      if (!PoolPtr)
        break;

      uintptr_t Offset = 0;
      while (Offset < Trailer->PoolSize) {
        auto AllocationPtr = PoolPtr + Offset;
        auto Header = (const AllocationHeader *)AllocationPtr;
        if (Header->Size == 0)
          break;
        auto RemoteAddr = PoolStart + Offset + sizeof(AllocationHeader);
        MetadataAllocation<Runtime> Allocation;
        Allocation.Tag = Header->Tag;
        Allocation.Ptr = RemoteAddr;
        Allocation.Size = Header->Size;
        Call(Allocation);
        
        Offset += sizeof(AllocationHeader) + Header->Size;
      }
      
      TrailerPtr = Trailer->PrevTrailer;
    }
    return llvm::None;
  }

  llvm::Optional<std::string> iterateMetadataAllocationBacktraces(
      std::function<void(StoredPointer, uint32_t, const StoredPointer *)>
          Call) {
    std::string BacktraceListName =
        "_swift_debug_metadataAllocationBacktraceList";

    auto BacktraceListAddr = getReader().getSymbolAddress(BacktraceListName);
    if (!BacktraceListAddr)
      return "unable to look up debug variable " + BacktraceListName;
    auto BacktraceListNextPtr =
        getReader().readPointer(BacktraceListAddr, sizeof(StoredPointer));
    if (!BacktraceListNextPtr)
      return llvm::None;

    auto BacktraceListNext = BacktraceListNextPtr->getResolvedAddress();
    while (BacktraceListNext) {
      auto HeaderBytes = getReader().readBytes(
          RemoteAddress(BacktraceListNext),
          sizeof(MetadataAllocationBacktraceHeader<Runtime>));
      auto HeaderPtr =
          reinterpret_cast<const MetadataAllocationBacktraceHeader<Runtime> *>(
              HeaderBytes.get());
      if (HeaderPtr == nullptr) {
        // FIXME: std::stringstream would be better, but LLVM's standard library
        // introduces a vtable and we don't want that.
        char result[128];
        std::snprintf(result, sizeof(result),
            "unable to read Next pointer %#" PRIx64,
            BacktraceListNext.getAddressData());
        return std::string(result);
      }
      auto BacktraceAddrPtr =
          BacktraceListNext +
          sizeof(MetadataAllocationBacktraceHeader<Runtime>);
      auto BacktraceBytes =
          getReader().readBytes(RemoteAddress(BacktraceAddrPtr),
                                HeaderPtr->Count * sizeof(StoredPointer));
      auto BacktracePtr =
          reinterpret_cast<const StoredPointer *>(BacktraceBytes.get());

      Call(HeaderPtr->Allocation, HeaderPtr->Count, BacktracePtr);

      BacktraceListNext = RemoteAddress(HeaderPtr->Next);
    }
    return llvm::None;
  }

private:
  const TypeInfo *
  getClosureContextInfo(StoredPointer Context, const ClosureContextInfo &Info,
                        remote::TypeInfoProvider *ExternalTypeInfo) {
    RecordTypeInfoBuilder Builder(getBuilder().getTypeConverter(),
                                  RecordKind::ClosureContext);

    auto Metadata = readMetadataFromInstance(Context);
    if (!Metadata)
      return nullptr;

    // Calculate the offset of the first capture.
    // See GenHeap.cpp, buildPrivateMetadata().
    auto OffsetToFirstCapture =
        this->readOffsetToFirstCaptureFromMetadata(*Metadata);
    if (!OffsetToFirstCapture)
      return nullptr;

    // Initialize the builder.
    Builder.addField(*OffsetToFirstCapture,
                     /*alignment=*/sizeof(StoredPointer),
                     /*numExtraInhabitants=*/0,
                     /*bitwiseTakable=*/true);

    // Skip the closure's necessary bindings struct, if it's present.
    auto SizeOfNecessaryBindings = Info.NumBindings * sizeof(StoredPointer);
    Builder.addField(/*size=*/SizeOfNecessaryBindings,
                     /*alignment=*/sizeof(StoredPointer),
                     /*numExtraInhabitants=*/0,
                     /*bitwiseTakable=*/true);

    // FIXME: should be unordered_set but I'm too lazy to write a hash
    // functor
    std::set<std::pair<const TypeRef *, const MetadataSource *>> Done;
    GenericArgumentMap Subs;

    ArrayRef<const TypeRef *> CaptureTypes = Info.CaptureTypes;

    // Closure context element layout depends on the layout of the
    // captured types, but captured types might depend on element
    // layout (of previous elements). Use an iterative approach to
    // solve the problem.
    while (!CaptureTypes.empty()) {
      const TypeRef *OrigCaptureTR = CaptureTypes[0];

      // If we failed to demangle the capture type, we cannot proceed.
      if (OrigCaptureTR == nullptr)
        return nullptr;

      const TypeRef *SubstCaptureTR = nullptr;

      // If we have enough substitutions to make this captured value's
      // type concrete, or we know it's size anyway (because it is a
      // class reference or metatype, for example), go ahead and add
      // it to the layout.
      if (OrigCaptureTR->isConcreteAfterSubstitutions(Subs))
        SubstCaptureTR = OrigCaptureTR->subst(getBuilder(), Subs);
      else if (getBuilder().getTypeConverter().hasFixedSize(OrigCaptureTR))
        SubstCaptureTR = OrigCaptureTR;

      if (SubstCaptureTR != nullptr) {
        Builder.addField("", SubstCaptureTR, ExternalTypeInfo);
        if (Builder.isInvalid())
          return nullptr;

        // Try the next capture type.
        CaptureTypes = CaptureTypes.slice(1);
        continue;
      }

      // Ok, we do not have enough substitutions yet. Perhaps we have
      // enough elements figured out that we can pick off some
      // metadata sources though, and use those to derive some new
      // substitutions.
      bool Progress = false;
      for (auto Source : Info.MetadataSources) {
        // Don't read a source more than once.
        if (Done.count(Source))
          continue;

        // If we don't have enough information to read this source
        // (because it is fulfilled by metadata from a capture at
        // at unknown offset), keep going.
        if (!isMetadataSourceReady(Source.second, Builder))
          continue;

        auto Metadata = readMetadataSource(Context, Source.second, Builder);
        if (!Metadata)
          return nullptr;

        auto *SubstTR = readTypeFromMetadata(*Metadata);
        if (SubstTR == nullptr)
          return nullptr;

        if (!TypeRef::deriveSubstitutions(Subs, Source.first, SubstTR))
          return nullptr;

        Done.insert(Source);
        Progress = true;
      }

      // If we failed to make any forward progress above, we're stuck
      // and cannot close out this layout.
      if (!Progress)
        return nullptr;
    }

    // Ok, we have a complete picture now.
    return Builder.build();
  }

  /// Checks if we have enough information to read the given metadata
  /// source.
  ///
  /// \param Builder Used to obtain offsets of elements known so far.
  bool isMetadataSourceReady(const MetadataSource *MS,
                             const RecordTypeInfoBuilder &Builder) {
    switch (MS->getKind()) {
    case MetadataSourceKind::ClosureBinding:
      return true;
    case MetadataSourceKind::ReferenceCapture: {
      unsigned Index = cast<ReferenceCaptureMetadataSource>(MS)->getIndex();
      return Index < Builder.getNumFields();
    }
    case MetadataSourceKind::MetadataCapture: {
      unsigned Index = cast<MetadataCaptureMetadataSource>(MS)->getIndex();
      return Index < Builder.getNumFields();
    }
    case MetadataSourceKind::GenericArgument: {
      auto Base = cast<GenericArgumentMetadataSource>(MS)->getSource();
      return isMetadataSourceReady(Base, Builder);
    }
    case MetadataSourceKind::Self:
    case MetadataSourceKind::SelfWitnessTable:
      return true;
    }

    swift_unreachable("Unhandled MetadataSourceKind in switch.");
  }

  /// Read metadata for a captured generic type from a closure context.
  ///
  /// \param Context The closure context in the remote process.
  ///
  /// \param MS The metadata source, which must be "ready" as per the
  /// above.
  ///
  /// \param Builder Used to obtain offsets of elements known so far.
  llvm::Optional<StoredPointer>
  readMetadataSource(StoredPointer Context,
                     const MetadataSource *MS,
                     const RecordTypeInfoBuilder &Builder) {
    switch (MS->getKind()) {
    case MetadataSourceKind::ClosureBinding: {
      unsigned Index = cast<ClosureBindingMetadataSource>(MS)->getIndex();

      // Skip the context's HeapObject header
      // (one word each for isa pointer and reference counts).
      //
      // Metadata and conformance tables are stored consecutively after
      // the heap object header, in the 'necessary bindings' area.
      //
      // We should only have the index of a type metadata record here.
      unsigned Offset = getSizeOfHeapObject() +
                        sizeof(StoredPointer) * Index;

      StoredPointer MetadataAddress;
      if (!getReader().readInteger(RemoteAddress(Context + Offset),
                                   &MetadataAddress))
        break;

      return MetadataAddress;
    }
    case MetadataSourceKind::ReferenceCapture: {
      unsigned Index = cast<ReferenceCaptureMetadataSource>(MS)->getIndex();

      // We should already have enough type information to know the offset
      // of this capture in the context.
      unsigned CaptureOffset = Builder.getFieldOffset(Index);

      StoredPointer CaptureAddress;
      if (!getReader().readInteger(RemoteAddress(Context + CaptureOffset),
                                   &CaptureAddress))
        break;

      // Read the requested capture's isa pointer.
      return readMetadataFromInstance(CaptureAddress);
    }
    case MetadataSourceKind::MetadataCapture: {
      unsigned Index = cast<MetadataCaptureMetadataSource>(MS)->getIndex();

      // We should already have enough type information to know the offset
      // of this capture in the context.
      unsigned CaptureOffset = Builder.getFieldOffset(Index);

      StoredPointer CaptureAddress;
      if (!getReader().readInteger(RemoteAddress(Context + CaptureOffset),
                                   &CaptureAddress))
        break;

      return CaptureAddress;
    }
    case MetadataSourceKind::GenericArgument: {
      auto *GAMS = cast<GenericArgumentMetadataSource>(MS);
      auto Base = readMetadataSource(Context, GAMS->getSource(), Builder);
      if (!Base)
        break;

      unsigned Index = GAMS->getIndex();
      auto Arg = readGenericArgFromMetadata(*Base, Index);
      if (!Arg)
        break;

      return *Arg;
    }
    case MetadataSourceKind::Self:
    case MetadataSourceKind::SelfWitnessTable:
      break;
    }

    return llvm::None;
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
