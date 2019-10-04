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

#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/Runtime/Unreachable.h"

#include <iostream>
#include <set>
#include <vector>
#include <unordered_map>
#include <utility>

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
  using typename super::StoredPointer;

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

    auto findMachOSectionByName = [&](std::string Name)
        -> std::pair<RemoteRef<void>, uint64_t> {
      for (unsigned I = 0; I < NumSect; ++I) {
        auto S = reinterpret_cast<typename T::Section *>(
            SectionsBuf + (I * sizeof(typename T::Section)));
        if (strncmp(S->sectname, Name.c_str(), strlen(Name.c_str())) != 0)
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

    auto FieldMdSec = findMachOSectionByName("__swift5_fieldmd");
    auto AssocTySec = findMachOSectionByName("__swift5_assocty");
    auto BuiltinTySec = findMachOSectionByName("__swift5_builtin");
    auto CaptureSec = findMachOSectionByName("__swift5_capture");
    auto TypeRefMdSec = findMachOSectionByName("__swift5_typeref");
    auto ReflStrMdSec = findMachOSectionByName("__swift5_reflstr");

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
        if (SectionName != ".sw5cptr" && SectionName != ".sw5bltn") {
          Begin = Begin.atByteOffset(8);
          Size -= 16;
        }

        return {Begin, Size};
      }
      return {nullptr, 0};
    };

    auto CaptureSec = findCOFFSectionByName(".sw5cptr");
    auto TypeRefMdSec = findCOFFSectionByName(".sw5tyrf");
    auto FieldMdSec = findCOFFSectionByName(".sw5flmd");
    auto AssocTySec = findCOFFSectionByName(".sw5asty");
    auto BuiltinTySec = findCOFFSectionByName(".sw5bltn");
    auto ReflStrMdSec = findCOFFSectionByName(".sw5rfst");

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

  template <typename T> bool readELFSections(RemoteAddress ImageStart) {
    auto Buf =
        this->getReader().readBytes(ImageStart, sizeof(typename T::Header));

    auto Hdr = reinterpret_cast<const typename T::Header *>(Buf.get());
    assert(Hdr->getFileClass() == T::ELFClass && "invalid ELF file class");

    // From the header, grab informations about the section header table.
    auto SectionHdrAddress = ImageStart.getAddressData() + Hdr->e_shoff;
    auto SectionHdrNumEntries = Hdr->e_shnum;
    auto SectionEntrySize = Hdr->e_shentsize;

    // Collect all the section headers, we need them to look up the
    // reflection sections (by name) and the string table.
    std::vector<const typename T::Section *> SecHdrVec;
    for (unsigned I = 0; I < SectionHdrNumEntries; ++I) {
      auto SecBuf = this->getReader().readBytes(
          RemoteAddress(SectionHdrAddress + (I * SectionEntrySize)),
          SectionEntrySize);
      if (!SecBuf)
        return false;
      auto SecHdr =
          reinterpret_cast<const typename T::Section *>(SecBuf.get());
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

    auto StrTabStart =
        RemoteAddress(ImageStart.getAddressData() + StrTabOffset);
    auto StrTabBuf = this->getReader().readBytes(StrTabStart, StrTabSize);
    auto StrTab = reinterpret_cast<const char *>(StrTabBuf.get());

    auto findELFSectionByName = [&](std::string Name)
        -> std::pair<RemoteRef<void>, uint64_t> {
      // Now for all the sections, find their name.
      for (const typename T::Section *Hdr : SecHdrVec) {
        uint32_t Offset = Hdr->sh_name;
        auto SecName = std::string(StrTab + Offset);
        if (SecName != Name)
          continue;
        auto SecStart =
            RemoteAddress(ImageStart.getAddressData() + Hdr->sh_addr);
        auto SecSize = Hdr->sh_size;
        auto SecBuf = this->getReader().readBytes(SecStart, SecSize);
        auto SecContents = RemoteRef<void>(SecStart.getAddressData(),
                                           SecBuf.get());
        savedBuffers.push_back(std::move(SecBuf));
        return {SecContents, SecSize};
      }
      return {nullptr, 0};
    };

    auto FieldMdSec = findELFSectionByName("swift5_fieldmd");
    auto AssocTySec = findELFSectionByName("swift5_assocty");
    auto BuiltinTySec = findELFSectionByName("swift5_builtin");
    auto CaptureSec = findELFSectionByName("swift5_capture");
    auto TypeRefMdSec = findELFSectionByName("swift5_typeref");
    auto ReflStrMdSec = findELFSectionByName("swift5_reflstr");

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

    savedBuffers.push_back(std::move(Buf));
    return true;
  }
         
  bool readELF(RemoteAddress ImageStart) {
    auto Buf =
        this->getReader().readBytes(ImageStart, sizeof(llvm::ELF::Elf64_Ehdr));

    // Read the header.
    auto Hdr = reinterpret_cast<const llvm::ELF::Elf64_Ehdr *>(Buf.get());

    if (!Hdr->checkMagic())
      return false;

    // Check if we have a ELFCLASS32 or ELFCLASS64
    unsigned char FileClass = Hdr->getFileClass();
    if (FileClass == llvm::ELF::ELFCLASS64) {
      return readELFSections<ELFTraits<llvm::ELF::ELFCLASS64>>(ImageStart);
    } else if (FileClass == llvm::ELF::ELFCLASS32) {
      return readELFSections<ELFTraits<llvm::ELF::ELFCLASS32>>(ImageStart);
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
      return readELF(ImageStart);
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
  bool ownsAddress(RemoteAddress Address) {
    for (auto Range : imageRanges) {
      auto Start = std::get<0>(Range);
      auto End = std::get<1>(Range);
      if (Start.getAddressData() <= Address.getAddressData()
          && Address.getAddressData() < End.getAddressData())
        return true;
    }
  
    return false;
  }
  
  /// Return a description of the layout of a class instance with the given
  /// metadata as its isa pointer.
  const TypeInfo *getMetadataTypeInfo(StoredPointer MetadataAddress) {
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
          TI = TC.getClassInstanceTypeInfo(TR, *start);

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
  const TypeInfo *getInstanceTypeInfo(StoredPointer ObjectAddress) {
    auto MetadataAddress = readMetadataFromInstance(ObjectAddress);
    if (!MetadataAddress)
      return nullptr;

    auto kind = this->readKindFromMetadata(*MetadataAddress);
    if (!kind)
      return nullptr;

    switch (*kind) {
    case MetadataKind::Class:
      return getMetadataTypeInfo(*MetadataAddress);

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

      return getClosureContextInfo(ObjectAddress, Info);
    }

    case MetadataKind::HeapGenericLocalVariable: {
      // Generic SIL @box type - there is always an instantiated metadata
      // pointer for the boxed type.
      if (auto Meta = readMetadata(*MetadataAddress)) {
        auto GenericHeapMeta =
          cast<TargetGenericBoxHeapMetadata<Runtime>>(Meta.getLocalBuffer());
        return getMetadataTypeInfo(GenericHeapMeta->BoxedType);
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

  bool
  projectExistential(RemoteAddress ExistentialAddress,
                     const TypeRef *ExistentialTR,
                     const TypeRef **OutInstanceTR,
                     RemoteAddress *OutInstanceAddress) {
    if (ExistentialTR == nullptr)
      return false;

    auto ExistentialTI = getTypeInfo(ExistentialTR);
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

  /// Return a description of the layout of a value with the given type.
  const TypeInfo *getTypeInfo(const TypeRef *TR) {
    return getBuilder().getTypeConverter().getTypeInfo(TR);
  }

private:
  const TypeInfo *getClosureContextInfo(StoredPointer Context,
                                        const ClosureContextInfo &Info) {
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
        Builder.addField("", SubstCaptureTR);
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

    swift_runtime_unreachable("Unhandled MetadataSourceKind in switch.");
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
