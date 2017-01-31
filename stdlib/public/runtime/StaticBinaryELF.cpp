//===--- StaticBinaryELF.cpp ----------------------------------------------===//
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
// Parse a static ELF binary to implement lookupSymbol() address lookup.
//
//===----------------------------------------------------------------------===//

#if defined(__ELF__) && defined(__linux__)

#include "ImageInspection.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "swift/Basic/Lazy.h"
#include <cassert>
#include <string>
#include <vector>
#include <elf.h>
#include <fcntl.h>
#include <pthread.h>
#include <unistd.h>
#include <linux/limits.h>
#include <sys/auxv.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>

using namespace std;
using namespace llvm;

#ifdef __LP64__
#define ELFCLASS ELFCLASS64
typedef Elf64_Ehdr Elf_Ehdr;
typedef Elf64_Shdr Elf_Shdr;
typedef Elf64_Phdr Elf_Phdr;
typedef Elf64_Addr Elf_Addr;
typedef Elf64_Word Elf_Word;
typedef Elf64_Sym Elf_Sym;
typedef Elf64_Section Elf_Section;
#define ELF_ST_TYPE(x) ELF64_ST_TYPE(x)
#else
#define ELFCLASS ELFCLASS32
typedef Elf32_Ehdr Elf_Ehdr;
typedef Elf32_Shdr Elf_Shdr;
typedef Elf32_Phdr Elf_Phdr;
typedef Elf32_Addr Elf_Addr;
typedef Elf32_Word Elf_Word;
typedef Elf32_Sym Elf_Sym;
typedef Elf32_Section Elf_Section;
#define ELF_ST_TYPE(x) ELF32_ST_TYPE(x)
#endif

extern const Elf_Ehdr elfHeader asm("__ehdr_start");

class StaticBinaryELF {

private:
  // mmap a section of a file that might not be page aligned.
  class Mapping {
  public:
    void *mapping;
    size_t mapLength;
    off_t diff;

    Mapping(int fd, size_t fileSize, off_t offset, size_t length) {
      if (fd < 0 || offset + length > fileSize) {
        mapping = nullptr;
        mapLength = 0;
        diff = 0;
        return;
      }
      long pageSize = sysconf(_SC_PAGESIZE);
      long pageMask = ~(pageSize - 1);

      off_t alignedOffset = offset & pageMask;
      diff = (offset - alignedOffset);
      mapLength = diff + length;
      mapping = mmap(nullptr, mapLength, PROT_READ, MAP_PRIVATE, fd,
                     alignedOffset);
      if (mapping == MAP_FAILED) {
        mapping = nullptr;
        mapLength = 0;
        diff = 0;
      }
    }

    template<typename T>
    ArrayRef<T> data() {
      size_t elements = (mapLength - diff) / sizeof(T);
      const T *data = reinterpret_cast<T *>(reinterpret_cast<char *>(mapping)
                                            + diff);
      return ArrayRef<T>(data, elements);
    }

    ~Mapping() {
      if (mapping && mapLength > 0) {
        munmap(mapping, mapLength);
      }
    }
  };

  string fullPathName;
  const Elf_Phdr *programHeaders = nullptr;
  Optional<Mapping> symbolTable;
  Optional<Mapping> stringTable;

public:
  StaticBinaryELF() {
    getExecutablePathName();

    programHeaders = reinterpret_cast<const Elf_Phdr *>(getauxval(AT_PHDR));
    if (programHeaders == nullptr) {
      return;
    }
    // If an interpreter is set in the program headers then this is a
    // dynamic executable and therefore not valid.
    for (size_t idx = 0; idx < elfHeader.e_phnum; idx++) {
      if (programHeaders[idx].p_type == PT_INTERP) {
        programHeaders = nullptr;
        return;
      }
    }

    if (!fullPathName.empty()) {
      mmapExecutable();
    }
  }

  const char *getPathName() {
    return fullPathName.empty() ? nullptr : fullPathName.c_str();
  }

  void *getSectionLoadAddress(const void *addr) {
    if (programHeaders) {
      auto searchAddr = reinterpret_cast<Elf_Addr>(addr);

      for (size_t idx = 0; idx < elfHeader.e_phnum; idx++) {
        auto header = &programHeaders[idx];
        if (header->p_type == PT_LOAD && searchAddr >= header->p_vaddr
            && searchAddr <= (header->p_vaddr + header->p_memsz)) {
          return reinterpret_cast<void *>(header->p_vaddr);
        }
      }
    }
    return nullptr;
  }

  // Lookup a function symbol by address.
  const Elf_Sym *findSymbol(const void *addr) {
    if (symbolTable.hasValue()) {
      auto searchAddr = reinterpret_cast<Elf_Addr>(addr);
      auto symbols = symbolTable->data<Elf_Sym>();

      for (size_t idx = 0; idx < symbols.size(); idx++) {
        auto symbol = &symbols[idx];
        if (ELF_ST_TYPE(symbol->st_info) == STT_FUNC
            && searchAddr >= symbol->st_value
            && searchAddr < (symbol->st_value + symbol->st_size)) {
          return symbol;
        }
      }
    }
    return nullptr;
  }

  const char *symbolName(const Elf_Sym *symbol) {
    if (stringTable.hasValue()) {
      auto strings = stringTable->data<char>();
      if (symbol->st_name < strings.size()) {
        return &strings[symbol->st_name];
      }
    }
    return nullptr;
  }

private:
  // This is Linux specific - find the full path of the executable
  // by looking in /proc/self/maps for a mapping holding the current
  // address space. For a static binary it should only be mapping one
  // file anyway. Don't use /proc/self/exe as the symlink will be removed
  // if the main thread terminates - see proc(5).
  void getExecutablePathName() {
    uintptr_t address = (uintptr_t)&elfHeader;

    FILE *fp = fopen("/proc/self/maps", "r");
    if (!fp) {
      perror("Unable to open /proc/self/maps");
    } else {
      char *line = nullptr;
      size_t size = 0;
      // Format is: addrLo-addrHi perms offset dev inode pathname.
      // If the executable has been deleted the last column will be '(deleted)'.
      StringRef deleted = StringRef("(deleted)");

      while(getdelim(&line, &size, '\n', fp) != -1) {
        StringRef entry = StringRef(line).rsplit('\n').first;
        auto addrRange = entry.split(' ').first.split('-');
        unsigned long long low = strtoull(addrRange.first.str().c_str(),
                                          nullptr, 16);
        if (low == 0 || low > UINTPTR_MAX || address < (uintptr_t)low) {
          continue;
        }

        unsigned long long high = strtoull(addrRange.second.str().c_str(),
                                           nullptr, 16);
        if (high == 0 || high > UINTPTR_MAX || address > (uintptr_t)high) {
          continue;
        }

        auto fname = entry.split('/').second;
        if (fname.empty() || fname.endswith(deleted)) {
          continue;
        }

        fullPathName = "/" + fname.str();
        break;
      }
      if (line) {
        free(line);
      }
      fclose(fp);
    }
  }


  // Parse the ELF binary using mmap for the section headers, symbol table and
  // string table.
  void mmapExecutable() {
    struct stat buf;
    int fd = open(fullPathName.c_str(), O_RDONLY);
    if (fd < 0) {
      return;
    }

    if (fstat(fd, &buf) != 0) {
      close(fd);
      return;
    }

    // Map in the section headers.
    size_t sectionHeadersSize = (elfHeader.e_shentsize * elfHeader.e_shnum);
    Mapping sectionHeaders = Mapping(fd, buf.st_size, elfHeader.e_shoff,
                                     sectionHeadersSize);
    if (sectionHeaders.mapping) {
      auto headers = sectionHeaders.data<Elf_Shdr>();
      auto section = findSectionHeader(headers, SHT_SYMTAB);
      if (section) {
        assert(section->sh_entsize == sizeof(Elf_Sym));
        symbolTable.emplace(fd, buf.st_size, section->sh_offset,
                            section->sh_size);
        if (symbolTable->mapping == nullptr) {
          symbolTable.reset();
        }
      }
      section = findSectionHeader(headers, SHT_STRTAB);
      if (section) {
        stringTable.emplace(fd, buf.st_size, section->sh_offset,
                            section->sh_size);
        if (stringTable->mapping == nullptr) {
          stringTable.reset();
        }
      }
    }
    close(fd);
    return;
  }

  // Find the section header of a specified type in the section headers table.
  const Elf_Shdr *findSectionHeader(ArrayRef<Elf_Shdr> headers,
                                    Elf_Word sectionType) {
    assert(elfHeader.e_shnum == headers.size());
    for (size_t idx = 0; idx < headers.size(); idx++) {
      if (idx == elfHeader.e_shstrndx) {
        continue;
      }
      auto header = &headers[idx];
      if (header->sh_type == sectionType) {
        if (header->sh_entsize > 0 && header->sh_size % header->sh_entsize) {
          fprintf(stderr,
                  "section size is not a multiple of entrysize (%ld/%ld)\n",
                  header->sh_size, header->sh_entsize);
          return nullptr;
        }
        return header;
      }
    }
    return nullptr;
  }
};


static swift::Lazy<StaticBinaryELF> TheBinary;

int
swift::lookupSymbol(const void *address, SymbolInfo *info) {
  // The pointers returned point into the mmap()'d binary so keep the
  // object once instantiated.
  auto &binary = TheBinary.get();

  info->fileName = binary.getPathName();
  info->baseAddress = binary.getSectionLoadAddress(address);

  auto symbol = binary.findSymbol(address);
  if (symbol != nullptr) {
    info->symbolAddress = reinterpret_cast<void *>(symbol->st_value);
    info->symbolName = binary.symbolName(symbol);
  } else {
    info->symbolAddress = nullptr;
    info->symbolName = nullptr;
  }

  return 1;
}

#endif // defined(__ELF__) && defined(__linux__)
