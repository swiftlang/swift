//===- tapi/Core/XPI.h - TAPI XPI -------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Defines XPI - API, SPI, etc
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_XPI_H
#define TAPI_CORE_XPI_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "AvailabilityInfo.h"
#include "LLVM.h"
#include "STLExtras.h"
#include "Defines.h"
#include "Symbol.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/iterator.h"
#include "llvm/Support/Allocator.h"
#include <map>
#include <utility>

TAPI_NAMESPACE_INTERNAL_BEGIN

class ObjCClass;
class ObjCContainer;
class XPISet;

using SymbolFlags = tapi::v1::SymbolFlags;

/// Helper method to create the symbol flags from the XPI flags.
inline SymbolFlags operator|=(SymbolFlags &lhs,
                              const SymbolFlags &rhs) noexcept {
  lhs = static_cast<SymbolFlags>(static_cast<unsigned>(lhs) |
                                 static_cast<unsigned>(rhs));
  return lhs;
}

/// The different XPI kinds.
enum class XPIKind : unsigned {
  GlobalSymbol,
  ObjectiveCClass,
  ObjectiveCClassEHType,
  ObjectiveCInstanceVariable,
  ObjCSelector,
  ObjCCategory,
  ObjCProtocol,
};

/// The XPI access permissions/visibility.
enum class XPIAccess : unsigned {
  Unknown,
  Exported,
  Public,
  Private,
  Project,
  Internal,
};

class XPI {
protected:
  /// Construct an XPI - the constructor should only be called by a
  /// sub-class.
  XPI(XPIKind kind, StringRef name, XPIAccess access,
      SymbolFlags flags = SymbolFlags::None)
      : _name(name), _kind(kind), _access(access), _flags(flags) {}

  /// Construct an XPI - the constructor should only be called by a
  /// sub-class.
  XPI(XPIKind kind, StringRef name, XPIAccess access, Architecture arch,
      AvailabilityInfo &info)
      : XPI(kind, name, access) {
    addAvailabilityInfo(arch, info);

    if (!info._unavailable)
      _archs.set(arch);
  }

public:
  bool isExportedSymbol() const;
  StringRef getName() const { return _name; }
  XPIKind getKind() const { return _kind; }
  XPIAccess getAccess() const { return _access; }
  void setAccess(XPIAccess access) { _access = access; }
  bool updateAccess(XPIAccess access) {
    if (access == XPIAccess::Unknown)
      return true;

    if (getAccess() == XPIAccess::Unknown) {
      setAccess(access);
      return true;
    }

    // XPIAccess Public and Private are for header declaration only.
    // It is fine to re-declare the public XPI in the private header again and
    // the final XPIAccess type should be public.
    if (getAccess() == XPIAccess::Public && access == XPIAccess::Private)
      return true;
    if (getAccess() == XPIAccess::Private && access == XPIAccess::Public) {
      setAccess(access);
      return true;
    }

    return getAccess() == access;
  }

  bool isWeakDefined() const {
    return (_flags & SymbolFlags::WeakDefined) == SymbolFlags::WeakDefined;
  }
  bool isWeakReferenced() const {
    return (_flags & SymbolFlags::WeakReferenced) ==
           SymbolFlags::WeakReferenced;
  }
  bool isThreadLocalValue() const {
    return (_flags & SymbolFlags::ThreadLocalValue) ==
           SymbolFlags::ThreadLocalValue;
  }

  SymbolFlags getSymbolFlags() const { return _flags; }

  void addAvailabilityInfo(Architecture arch,
                           const AvailabilityInfo info = AvailabilityInfo(),
                           bool NoOverwrite = false) {
    auto it =
        find_if(_availability,
                [arch](const std::pair<Architecture, AvailabilityInfo> &avail) {
                  return arch == avail.first;
                });
    if (it != _availability.end()) {
      if (NoOverwrite && !it->second.isDefault())
        it->second = info;
      if (!info._unavailable && info._obsoleted.empty()) {
        it->second._unavailable = false;
        _archs.set(arch);
      }
      return;
    }

    _availability.emplace_back(arch, info);
    if (!info._unavailable && info._obsoleted.empty())
      _archs.set(arch);
  }

  const llvm::SmallVectorImpl<std::pair<Architecture, AvailabilityInfo>> &
  getAvailabilityInfo() const {
    return _availability;
  }

  llvm::Optional<AvailabilityInfo>
  getAvailabilityInfo(Architecture arch) const {
    auto it =
        find_if(_availability,
                [arch](const std::pair<Architecture, AvailabilityInfo> &avail) {
                  return arch == avail.first;
                });
    if (it != _availability.end())
      return it->second;

    return llvm::None;
  }

  ArchitectureSet getArchitectures() const { return _archs; }

  bool hasArch(Architecture arch) const { return _archs.has(arch); }

  bool isAvailable() const { return _archs.count() != 0; }
  bool isUnavailable() const { return _archs.count() == 0; }
  bool isObsolete() const {
    for (const auto &avail : _availability)
      if (avail.second._obsoleted.empty())
        return false;
    return true;
  }
  std::string getPrettyName(bool demangle = false) const;
  std::string getAnnotatedName(bool demangle = false) const;
  void print(raw_ostream &os) const;

  bool operator<(const XPI &other) const {
    return std::tie(_kind, _name) < std::tie(other._kind, other._name);
  }

private:
  llvm::SmallVector<std::pair<Architecture, AvailabilityInfo>, 4>
      _availability{};
  StringRef _name;
  ArchitectureSet _archs{};

protected:
  /// The kind of xpi.
  XPIKind _kind;

  /// The access permission/visibility of this xpi.
  XPIAccess _access;

  /// Hoisted GlobalSymbol flags.
  SymbolFlags _flags;
};

inline raw_ostream &operator<<(raw_ostream &os, const XPI &xpi) {
  xpi.print(os);
  return os;
}

class GlobalSymbol : public XPI {
private:
  GlobalSymbol(StringRef name, XPIAccess access, SymbolFlags flags)
      : XPI(XPIKind::GlobalSymbol, name, access, flags) {}

public:
  static GlobalSymbol *create(llvm::BumpPtrAllocator &A, StringRef name,
                              XPIAccess access,
                              SymbolFlags flags = SymbolFlags::None);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::GlobalSymbol;
  }
};

class ObjCClassEHType : public XPI {
private:
  ObjCClassEHType(StringRef name, XPIAccess access)
      : XPI(XPIKind::ObjectiveCClassEHType, name, access) {}

public:
  static ObjCClassEHType *create(llvm::BumpPtrAllocator &A, StringRef name,
                                 XPIAccess access);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjectiveCClassEHType;
  }
};

class ObjCInstanceVariable : public XPI {
private:
  ObjCInstanceVariable(StringRef name, XPIAccess access)
      : XPI(XPIKind::ObjectiveCInstanceVariable, name, access) {}

public:
  static ObjCInstanceVariable *create(llvm::BumpPtrAllocator &A, StringRef name,
                                      XPIAccess access);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjectiveCInstanceVariable;
  }
};

class ObjCSelector : public XPI {
private:
  bool _isInstanceMethod;
  bool _isDynamic;
  bool _isDerivedFromProtocol;

  ObjCSelector(StringRef name, bool isInstanceMethod, bool isDynamic,
               XPIAccess access, bool isDerivedFromProtocol)
      : XPI(XPIKind::ObjCSelector, name, access),
        _isInstanceMethod(isInstanceMethod), _isDynamic(isDynamic),
        _isDerivedFromProtocol(isDerivedFromProtocol) {}

public:
  static ObjCSelector *create(llvm::BumpPtrAllocator &A, StringRef name,
                              bool isInstanceMethod, bool isDynamic,
                              XPIAccess access,
                              bool isDerivedFromProtocol = false);

  bool isInstanceMethod() const { return _isInstanceMethod; }
  bool isDynamic() const { return _isDynamic; }
  bool isDerivedFromProtocol() const { return _isDerivedFromProtocol; }

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjCSelector;
  }
};

class ObjCContainer : public XPI {
public:
  ObjCContainer(XPIKind kind, StringRef name, XPIAccess access)
      : XPI(kind, name, access) {}

  static bool classof(const XPI *xpi) {
    auto K = xpi->getKind();
    return K == XPIKind::ObjCProtocol || K == XPIKind::ObjectiveCClass ||
           K == XPIKind::ObjCCategory;
  }

  void addSelector(const ObjCSelector *selector);
  const ObjCSelector *findSelector(StringRef name,
                                   bool isInstanceMethod = false) const;
  using const_selector_range = llvm::iterator_range<
      SmallVectorImpl<const ObjCSelector *>::const_iterator>;
  const_selector_range selectors() const { return _selectors; }

private:
  llvm::SmallVector<const ObjCSelector *, 8> _selectors;
};

class ObjCProtocol : public ObjCContainer {
private:
  ObjCProtocol(StringRef name, XPIAccess access)
      : ObjCContainer(XPIKind::ObjCProtocol, name, access) {}

public:
  static ObjCProtocol *create(llvm::BumpPtrAllocator &A, StringRef name,
                              XPIAccess access);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjCProtocol;
  }
};

class ObjCCategory : public ObjCContainer {
private:
  ObjCCategory(ObjCClass *baseClass, StringRef name, XPIAccess access)
      : ObjCContainer(XPIKind::ObjCCategory, name, access),
        _baseClass(baseClass) {}

public:
  static ObjCCategory *create(llvm::BumpPtrAllocator &A, ObjCClass *baseClass,
                              StringRef name, XPIAccess access);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjCCategory;
  }

  const ObjCClass *getBaseClass() const { return _baseClass; }

private:
  const ObjCClass *_baseClass;
};

class ObjCClass : public ObjCContainer {
private:
  ObjCClass(StringRef name, XPIAccess access)
      : ObjCContainer(XPIKind::ObjectiveCClass, name, access),
        _superClass(nullptr) {}

public:
  static ObjCClass *create(llvm::BumpPtrAllocator &A, StringRef name,
                           XPIAccess access);

  static bool classof(const XPI *xpi) {
    return xpi->getKind() == XPIKind::ObjectiveCClass;
  }

  bool updateSuperClass(ObjCClass *superClass) {
    if (superClass == nullptr)
      return true;

    if (_superClass == nullptr) {
      _superClass = superClass;
      return true;
    }

    return _superClass == superClass;
  }

  const ObjCClass *getSuperClass() const { return _superClass; }

  void addCategory(const ObjCCategory *category);
  using const_category_range = llvm::iterator_range<
      llvm::SmallVectorImpl<const ObjCCategory *>::const_iterator>;
  const_category_range categories() const { return _categories; }

private:
  llvm::SmallVector<const ObjCCategory *, 4> _categories;
  const ObjCClass *_superClass;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_XPI_H
