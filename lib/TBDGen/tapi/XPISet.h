//===- tapi/Core/XPISet.h - TAPI XPI Set ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the XPI Set - A set of API, SPI, etc
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_XPISET_H
#define TAPI_CORE_XPISET_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "AvailabilityInfo.h"
#include "STLExtras.h"
#include "XPI.h"
#include "Defines.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/Allocator.h"
#include <stddef.h>
#include <unordered_map>

namespace clang {
class PresumedLoc;
} // namespace clang

TAPI_NAMESPACE_INTERNAL_BEGIN

class XPISet {
private:
  llvm::BumpPtrAllocator allocator;

  StringRef copyString(StringRef string) {
    if (string.empty())
      return {};

    void *ptr = allocator.Allocate(string.size(), 1);
    memcpy(ptr, string.data(), string.size());
    return StringRef(reinterpret_cast<const char *>(ptr), string.size());
  }

public:
  struct SymbolsMapKey {
    XPIKind kind;
    StringRef name;

    SymbolsMapKey(XPIKind kind, StringRef name) : kind(kind), name(name) {}
  };

  struct SymbolsMapKeyHash {
    std::size_t operator()(const SymbolsMapKey &key) const {
      return llvm::hash_combine(key.kind, key.name);
    }
  };

  struct SymbolsMapKeyEqual {
    bool operator()(const SymbolsMapKey &lhs, const SymbolsMapKey &rhs) const {
      return std::tie(lhs.kind, lhs.name) == std::tie(rhs.kind, rhs.name);
    }
  };

  struct SelectorsMapKey {
    StringRef containerName;
    StringRef selectorName;
    union {
      unsigned raw;
      struct {
        unsigned isInstanceMethod : 1;
        unsigned containerIsProtocol : 1;
      } bits;
    };

    SelectorsMapKey(StringRef containerName, StringRef selectorName,
                    bool isInstanceMethod, bool containerIsProtocol)
        : containerName(containerName), selectorName(selectorName), raw(0) {
      bits.isInstanceMethod = isInstanceMethod;
      bits.containerIsProtocol = containerIsProtocol;
    }
  };

  struct SelectorsMapKeyHash {
    std::size_t operator()(const SelectorsMapKey &key) const {
      return llvm::hash_combine(key.containerName, key.selectorName, key.raw);
    }
  };

  struct SelectorsMapKeyEqual {
    bool operator()(const SelectorsMapKey &lhs,
                    const SelectorsMapKey &rhs) const {
      return std::tie(lhs.containerName, lhs.selectorName, lhs.raw) ==
             std::tie(rhs.containerName, rhs.selectorName, rhs.raw);
    }
  };

  struct CategoriesMapKey {
    StringRef containerName;
    StringRef categoryName;

    CategoriesMapKey(StringRef containerName, StringRef categoryName)
        : containerName(containerName), categoryName(categoryName) {}
  };

  struct CategoriesMapKeyHash {
    std::size_t operator()(const CategoriesMapKey &key) const {
      return llvm::hash_combine(key.containerName, key.categoryName);
    }
  };

  struct CategoriesMapKeyEqual {
    bool operator()(const CategoriesMapKey &lhs,
                    const CategoriesMapKey &rhs) const {
      return std::tie(lhs.containerName, lhs.categoryName) ==
             std::tie(rhs.containerName, rhs.categoryName);
    }
  };

  struct ProtocolsMapKeyHash {
    std::size_t operator()(const StringRef &key) const {
      return llvm::hash_value(key);
    }
  };

  struct ProtocolsMapKeyEqual {
    bool operator()(const StringRef &lhs, const StringRef &rhs) const {
      return lhs == rhs;
    }
  };

  using SymbolsMapType =
      std::unordered_map<SymbolsMapKey, XPI *, SymbolsMapKeyHash,
                         SymbolsMapKeyEqual>;
  using SelectorsMapType =
      std::unordered_map<SelectorsMapKey, ObjCSelector *, SelectorsMapKeyHash,
                         SelectorsMapKeyEqual>;
  using CategoriesMapType =
      std::unordered_map<CategoriesMapKey, ObjCCategory *, CategoriesMapKeyHash,
                         CategoriesMapKeyEqual>;
  using ProtocolsMapType =
      std::unordered_map<StringRef, ObjCProtocol *, ProtocolsMapKeyHash>;
  SymbolsMapType _symbols;
  SelectorsMapType _selectors;
  CategoriesMapType _categories;
  ProtocolsMapType _protocols;

public:
  XPISet() = default;

  GlobalSymbol *addGlobalSymbol(StringRef name, clang::PresumedLoc loc,
                                XPIAccess access, Architecture arch,
                                const AvailabilityInfo &info,
                                bool isWeakDefined = false);
  ObjCClass *addObjCClass(StringRef name, clang::PresumedLoc loc,
                          XPIAccess access, Architecture arch,
                          const AvailabilityInfo &info,
                          ObjCClass *superClass = nullptr);
  ObjCClassEHType *addObjCClassEHType(StringRef name, clang::PresumedLoc loc,
                                      XPIAccess access, Architecture arch,
                                      const AvailabilityInfo &info);
  ObjCInstanceVariable *addObjCInstanceVariable(StringRef name,
                                                clang::PresumedLoc loc,
                                                XPIAccess access,
                                                Architecture arch,
                                                const AvailabilityInfo &info);
  ObjCSelector *addObjCSelector(ObjCContainer *container, StringRef name,
                                bool isInstanceMethod, bool isDynamic,
                                clang::PresumedLoc loc, XPIAccess access,
                                Architecture arch, const AvailabilityInfo &info,
                                bool isDerivedFromProtocol = false);
  ObjCCategory *addObjCCategory(ObjCClass *baseClass, StringRef name,
                                clang::PresumedLoc loc, XPIAccess access,
                                Architecture arch,
                                const AvailabilityInfo &info);
  ObjCProtocol *addObjCProtocol(StringRef name, clang::PresumedLoc loc,
                                XPIAccess access, Architecture arch,
                                const AvailabilityInfo info);

  GlobalSymbol *addGlobalSymbol(StringRef name, ArchitectureSet archs,
                                SymbolFlags flags, XPIAccess access);
  ObjCClass *addObjCClass(StringRef name, ArchitectureSet archs,
                          XPIAccess access, ObjCClass *superClass = nullptr);
  ObjCClassEHType *addObjCClassEHType(StringRef name, ArchitectureSet archs,
                                      XPIAccess access);
  ObjCInstanceVariable *addObjCInstanceVariable(StringRef name,
                                                ArchitectureSet archs,
                                                XPIAccess access);
  ObjCSelector *addObjCSelector(ObjCContainer *container, StringRef name,
                                ArchitectureSet archs, bool isInstanceMethod,
                                bool isDynamic, XPIAccess access);
  ObjCCategory *addObjCCategory(ObjCClass *baseClass, StringRef name,
                                ArchitectureSet archs, XPIAccess access);
  ObjCProtocol *addObjCProtocol(StringRef name, ArchitectureSet archs,
                                XPIAccess access);

  const XPI *findSymbol(const XPI &) const;
  const XPI *findSymbol(XPIKind kind, StringRef name) const;
  bool removeSymbol(XPIKind, StringRef name);
  const ObjCSelector *findSelector(const SelectorsMapKey &) const;
  const ObjCCategory *findCategory(const CategoriesMapKey &) const;
  const ObjCCategory *findCategory(const ObjCCategory *) const;
  const ObjCProtocol *findProtocol(StringRef) const;

  struct const_symbol_iterator
      : public llvm::iterator_adaptor_base<
            const_symbol_iterator, SymbolsMapType::const_iterator,
            std::forward_iterator_tag, const XPI *, ptrdiff_t, const XPI *,
            const XPI *> {
    const_symbol_iterator() = default;

    template <typename U>
    const_symbol_iterator(U &&u)
        : iterator_adaptor_base(std::forward<U &&>(u)) {}

    reference operator*() const { return I->second; }
    pointer operator->() const { return I->second; }
  };
  using const_symbol_range = llvm::iterator_range<const_symbol_iterator>;

  // Custom iterator to return only exported symbols.
  struct const_export_iterator
      : public llvm::iterator_adaptor_base<
            const_export_iterator, const_symbol_iterator,
            std::forward_iterator_tag, const XPI *> {
    const_symbol_iterator _end;

    const_export_iterator() = default;
    template <typename U>
    const_export_iterator(U &&it, U &&end)
        : iterator_adaptor_base(std::forward<U &&>(it)),
          _end(std::forward<U &&>(end)) {
      while (I != _end && !I->isExportedSymbol())
        ++I;
    }

    const_export_iterator &operator++() {
      do {
        ++I;
      } while (I != _end && !I->isExportedSymbol());
      return *this;
    }

    const_export_iterator operator++(int) {
      const_export_iterator tmp(*this);
      do {
        ++I;
      } while (I != _end && !I->isExportedSymbol());
      return tmp;
    }
  };
  using const_export_range = llvm::iterator_range<const_export_iterator>;

  const_symbol_range symbols() const {
    return {_symbols.begin(), _symbols.end()};
  }

  const_export_range exports() const {
    return {const_export_iterator(_symbols.begin(), _symbols.end()),
            const_export_iterator(_symbols.end(), _symbols.end())};
  }

  using const_selector_range =
      llvm::iterator_range<SelectorsMapType::const_iterator>;
  const_selector_range selectors() const { return _selectors; }

  using const_category_range =
      llvm::iterator_range<CategoriesMapType::const_iterator>;
  const_category_range categories() const { return _categories; }

  using const_protocol_range =
      llvm::iterator_range<ProtocolsMapType::const_iterator>;
  const_protocol_range protocols() const { return _protocols; }

  void *Allocate(size_t Size, unsigned Align = 8) {
    return allocator.Allocate(Size, Align);
  }
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_XPISET_H
