//===--- StaticBinaryELF.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
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

#if defined(__ELF__)

#include "ImageInspection.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Object/ELF.h"
#include "swift/Basic/Lazy.h"
#include <cassert>
#include <string>
#include <vector>
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
using elfType = object::ELFType<support::native, (__POINTER_WIDTH__ == 64)>;

extern const elfType::Ehdr elfHeader asm("__ehdr_start");

// Create strong linkage to pthread_self, pthread_once and pthread_key_create
// as they are normally weak-linked and used to detect the presence of pthreads.
// Without this the calls just jump to 0x0.
__attribute__((__visibility__("hidden")))
 pthread_t (*__strong_pthread_self)(void) = pthread_self;

__attribute__((__visibility__("hidden")))
int (*__strong_pthread_once)(pthread_once_t *, void (*)(void)) = pthread_once;

__attribute__((__visibility__("hidden")))
int (*__strong_pthread_key_create)(pthread_key_t *, void (*)(void *)) = pthread_key_create;

class StaticBinaryELF {
private:
  // mmap a section of a file that might not be page aligned.
  class Mapping {
  public:
    void *mapping;
    size_t mapLength;
    off_t diff;

    Mapping(int fd, size_t fileSize, off_t offset, size_t length) {
      assert(fd >= 0 && offset + length <= fileSize);

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
  const elfType::Phdr *programHeaders = nullptr;
  Optional<Mapping> symbolTable;
  Optional<Mapping> stringTable;

public:
  StaticBinaryELF() {
    getExecutablePathName();

    programHeaders = reinterpret_cast<const elfType::Phdr *>(getauxval(AT_PHDR));
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
      auto searchAddr = reinterpret_cast<uintptr_t>(addr);

      for (size_t idx = 0; idx < elfHeader.e_phnum; idx++) {
        auto header = &programHeaders[idx];
        if (header->p_type == PT_LOAD && searchAddr >= header->p_vaddr
            && searchAddr <= (header->p_vaddr + header->p_memsz)) {
					uintptr_t ptr = header->p_vaddr;
          return reinterpret_cast<void *>(ptr);
        }
      }
    }
    return nullptr;
  }

  // Lookup a function symbol by address.
  const elfType::Sym *findSymbol(const void *addr) {
    if (symbolTable.hasValue()) {
      auto searchAddr = reinterpret_cast<uintptr_t>(addr);
      auto symbols = symbolTable->data<elfType::Sym>();
      const elfType::Sym *bestMatch = nullptr;
      unsigned long bestDistance = ULONG_MAX;

      for (size_t idx = 0; idx < symbols.size(); idx++) {
        auto symbol = &symbols[idx];
        if (symbol->getType() == STT_FUNC
            && searchAddr >= symbol->getValue()) {

          auto tmpDistance = searchAddr - symbol->getValue();
          if (tmpDistance < symbol->st_size) {
            return symbol;
          }
          // The searchAddress is past the end of this symbol's region, keep
          // track of which symbol end address the searchAddress is closest to.
          tmpDistance -= symbol->st_size;
          if (tmpDistance < bestDistance) {
            bestMatch = symbol;
            tmpDistance = bestDistance;
          }
        }
      }
      return bestMatch;
    }
    return nullptr;
  }

  const char *symbolName(const elfType::Sym *symbol) {
    if (stringTable.hasValue()) {
      auto strings = stringTable->data<char>();
      if (symbol->st_name < strings.size()) {
        return &strings[symbol->st_name];
      }
    }
    return nullptr;
  }

private:
#if defined(__linux__)
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

      while (getdelim(&line, &size, '\n', fp) != -1) {
        StringRef entry = StringRef(line).drop_back();

        auto indexOfDash = entry.find('-');
        auto indexOfSpace = entry.find(' ');
        auto addrLow = entry.substr(0, indexOfDash);
        auto addrHigh = entry.substr(indexOfDash + 1, indexOfSpace);
        unsigned long long low = strtoull(addrLow.str().c_str(),
                                          nullptr, 16);
        if (low == 0 || low > UINTPTR_MAX || address < (uintptr_t)low) {
          continue;
        }

        unsigned long long high = strtoull(addrHigh.str().c_str(),
                                           nullptr, 16);
        if (high == 0 || high > UINTPTR_MAX || address > (uintptr_t)high) {
          continue;
        }

        auto fname = entry.substr(entry.find('/'));
        if (fname.empty() || fname.endswith(deleted)) {
          continue;
        }

        fullPathName = fname.str();
        break;
      }
      if (line) {
        free(line);
      }
      fclose(fp);
    }
  }
#else
#error("To support -static-executable on this ELF platform implement getExecutablePathName()")
#endif

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
      auto headers = sectionHeaders.data<elfType::Shdr>();
      auto section = findSectionHeader(headers, elfType::Word(SHT_SYMTAB));
      if (section) {
        assert(section->sh_entsize == sizeof(elfType::Sym));
        symbolTable.emplace(fd, buf.st_size, section->sh_offset,
                            section->sh_size);
        if (symbolTable->mapping == nullptr) {
          symbolTable.reset();
        }
      }
      section = findSectionHeader(headers, elfType::Word(SHT_STRTAB));
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
  const elfType::Shdr *findSectionHeader(ArrayRef<elfType::Shdr> headers,
                                         elfType::Word sectionType) {
    assert(elfHeader.e_shnum == headers.size());
    for (size_t idx = 0; idx < headers.size(); idx++) {
      if (idx == elfHeader.e_shstrndx) {
        continue;
      }
      auto header = &headers[idx];
      if (header->sh_type == sectionType) {
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
		auto address = reinterpret_cast<uintptr_t>(symbol->getValue());
    info->symbolAddress = reinterpret_cast<void *>(address);
    info->symbolName.reset(binary.symbolName(symbol));
  } else {
    info->symbolAddress = nullptr;
    info->symbolName = nullptr;
  }

  return 1;
}

#endif // defined(__ELF__)
