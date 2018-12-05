//===- lib/Core/XPISet.cpp - TAPI XPI Set -----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the XPI set
///
//===----------------------------------------------------------------------===//

#include "XPISet.h"
#include "Defines.h"
#include "clang/Basic/SourceLocation.h"

using namespace llvm;
using clang::PresumedLoc;

TAPI_NAMESPACE_INTERNAL_BEGIN

GlobalSymbol *XPISet::addGlobalSymbol(StringRef name, PresumedLoc /*loc*/,
                                      XPIAccess access, Architecture arch,
                                      const AvailabilityInfo &info,
                                      bool isWeakDefined) {
  name = copyString(name);
  GlobalSymbol *globalSymbol;
  auto result =
      _symbols.emplace(std::piecewise_construct,
                       std::forward_as_tuple(XPIKind::GlobalSymbol, name),
                       std::forward_as_tuple(nullptr));
  if (result.second) {
    globalSymbol = GlobalSymbol::create(allocator, name, access,
                                        isWeakDefined ? SymbolFlags::WeakDefined
                                                      : SymbolFlags::None);
    result.first->second = globalSymbol;
  } else {
    globalSymbol = cast<GlobalSymbol>(result.first->second);
    assert(globalSymbol->isWeakDefined() == isWeakDefined &&
           "Weak defined not equal");
  }

  auto success = globalSymbol->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  globalSymbol->addAvailabilityInfo(arch, info);

  return globalSymbol;
}

ObjCClass *XPISet::addObjCClass(StringRef name, PresumedLoc /*loc*/,
                                XPIAccess access, Architecture arch,
                                const AvailabilityInfo &info,
                                ObjCClass *superClass) {
  name = copyString(name);
  ObjCClass *objcClass = nullptr;
  auto result =
      _symbols.emplace(std::piecewise_construct,
                       std::forward_as_tuple(XPIKind::ObjectiveCClass, name),
                       std::forward_as_tuple(nullptr));
  if (result.second) {
    objcClass = ObjCClass::create(allocator, name, access);
    result.first->second = objcClass;
  } else {
    objcClass = cast<ObjCClass>(result.first->second);
  }

  auto success = objcClass->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  success = objcClass->updateSuperClass(superClass);
  assert(success && "super class is not equal");
  (void)success;
  objcClass->addAvailabilityInfo(arch, info);

  return objcClass;
}

ObjCClassEHType *XPISet::addObjCClassEHType(StringRef name,
                                            clang::PresumedLoc /*loc*/,
                                            XPIAccess access, Architecture arch,
                                            const AvailabilityInfo &info) {
  name = copyString(name);
  ObjCClassEHType *objcClassEH = nullptr;
  auto result = _symbols.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(XPIKind::ObjectiveCClassEHType, name),
      std::forward_as_tuple(nullptr));
  if (result.second) {
    objcClassEH = ObjCClassEHType::create(allocator, name, access);
    result.first->second = objcClassEH;
  } else {
    objcClassEH = cast<ObjCClassEHType>(result.first->second);
  }

  auto success = objcClassEH->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  objcClassEH->addAvailabilityInfo(arch, info);

  return objcClassEH;
}

ObjCInstanceVariable *
XPISet::addObjCInstanceVariable(StringRef name, PresumedLoc /*loc*/,
                                XPIAccess access, Architecture arch,
                                const AvailabilityInfo &info) {
  name = copyString(name);
  ObjCInstanceVariable *objcInstanceVariable = nullptr;
  auto result = _symbols.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(XPIKind::ObjectiveCInstanceVariable, name),
      std::forward_as_tuple(nullptr));

  if (result.second) {
    objcInstanceVariable =
        ObjCInstanceVariable::create(allocator, name, access);
    result.first->second = objcInstanceVariable;
  } else {
    objcInstanceVariable = cast<ObjCInstanceVariable>(result.first->second);
  }

  auto success = objcInstanceVariable->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  objcInstanceVariable->addAvailabilityInfo(arch, info);

  return objcInstanceVariable;
}

ObjCSelector *XPISet::addObjCSelector(ObjCContainer *container, StringRef name,
                                      bool isInstanceMethod, bool isDynamic,
                                      clang::PresumedLoc /*loc*/,
                                      XPIAccess access, Architecture arch,
                                      const AvailabilityInfo &info,
                                      bool isDerivedFromProtocol) {
  const auto *objcClass = container;
  bool isCategory = false;
  if (auto *category = dyn_cast<ObjCCategory>(container)) {
    objcClass = category->getBaseClass();
    isCategory = true;
  }

  name = copyString(name);
  auto result = _selectors.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(objcClass->getName(), name, isInstanceMethod,
                            isa<ObjCProtocol>(objcClass)),
      std::forward_as_tuple(nullptr));
  ObjCSelector *objcSelector = nullptr;
  if (result.second) {
    objcSelector =
        ObjCSelector::create(allocator, name, isInstanceMethod, isDynamic,
                             access, isDerivedFromProtocol);
    result.first->second = objcSelector;
  } else {
    objcSelector = cast<ObjCSelector>(result.first->second);
  }

  auto success = objcSelector->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  objcSelector->addAvailabilityInfo(arch, info, isCategory);

  // Record a reference in the container (class, category, or protocol).
  container->addSelector(objcSelector);

  return objcSelector;
}

ObjCCategory *XPISet::addObjCCategory(ObjCClass *baseClass, StringRef name,
                                      PresumedLoc /*loc*/, XPIAccess access,
                                      Architecture arch,
                                      const AvailabilityInfo &info) {
  name = copyString(name);
  ObjCCategory *objcCategory = nullptr;
  auto result =
      _categories.emplace(std::piecewise_construct,
                          std::forward_as_tuple(baseClass->getName(), name),
                          std::forward_as_tuple(nullptr));

  if (result.second) {
    objcCategory = ObjCCategory::create(allocator, baseClass, name, access);
    result.first->second = objcCategory;
  } else {
    objcCategory = cast<ObjCCategory>(result.first->second);
  }

  auto success = objcCategory->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  objcCategory->addAvailabilityInfo(arch, info);

  // Record a reference in the base class.
  baseClass->addCategory(objcCategory);

  return objcCategory;
}

ObjCProtocol *XPISet::addObjCProtocol(StringRef name, PresumedLoc /*loc*/,
                                      XPIAccess access, Architecture arch,
                                      const AvailabilityInfo info) {
  name = copyString(name);
  ObjCProtocol *objcProtocol = nullptr;
  auto result =
      _protocols.emplace(std::piecewise_construct, std::forward_as_tuple(name),
                         std::forward_as_tuple(nullptr));

  if (result.second) {
    objcProtocol = ObjCProtocol::create(allocator, name, access);
    result.first->second = objcProtocol;
  } else {
    objcProtocol = cast<ObjCProtocol>(result.first->second);
  }

  auto success = objcProtocol->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  objcProtocol->addAvailabilityInfo(arch, info);

  return objcProtocol;
}

GlobalSymbol *XPISet::addGlobalSymbol(StringRef name, ArchitectureSet archs,
                                      SymbolFlags flags, XPIAccess access) {
  name = copyString(name);
  GlobalSymbol *globalSymbol;
  auto result =
      _symbols.emplace(std::piecewise_construct,
                       std::forward_as_tuple(XPIKind::GlobalSymbol, name),
                       std::forward_as_tuple(nullptr));
  if (result.second) {
    globalSymbol = GlobalSymbol::create(allocator, name, access, flags);
    result.first->second = globalSymbol;
  } else {
    globalSymbol = cast<GlobalSymbol>(result.first->second);
    assert(globalSymbol->getSymbolFlags() == flags && "flags are not equal");
  }

  auto success = globalSymbol->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    globalSymbol->addAvailabilityInfo(arch);

  return globalSymbol;
}


ObjCClass *XPISet::addObjCClass(StringRef name, ArchitectureSet archs,
                                XPIAccess access, ObjCClass *superClass) {
  name = copyString(name);
  ObjCClass *objcClass = nullptr;
  auto result =
      _symbols.emplace(std::piecewise_construct,
                       std::forward_as_tuple(XPIKind::ObjectiveCClass, name),
                       std::forward_as_tuple(nullptr));
  if (result.second) {
    objcClass = ObjCClass::create(allocator, name, access);
    result.first->second = objcClass;
  } else {
    objcClass = cast<ObjCClass>(result.first->second);
  }

  auto success = objcClass->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;
  success = objcClass->updateSuperClass(superClass);
  assert(success && "super class is not equal");
  (void)success;

  for (auto arch : archs)
    objcClass->addAvailabilityInfo(arch);

  return objcClass;
}

ObjCClassEHType *XPISet::addObjCClassEHType(StringRef name,
                                            ArchitectureSet archs,
                                            XPIAccess access) {
  name = copyString(name);
  ObjCClassEHType *objCClassEH = nullptr;
  auto result = _symbols.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(XPIKind::ObjectiveCClassEHType, name),
      std::forward_as_tuple(nullptr));
  if (result.second) {
    objCClassEH = ObjCClassEHType::create(allocator, name, access);
    result.first->second = objCClassEH;
  } else {
    objCClassEH = cast<ObjCClassEHType>(result.first->second);
  }

  auto success = objCClassEH->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    objCClassEH->addAvailabilityInfo(arch);

  return objCClassEH;
}

ObjCInstanceVariable *XPISet::addObjCInstanceVariable(StringRef name,
                                                      ArchitectureSet archs,
                                                      XPIAccess access) {
  name = copyString(name);
  ObjCInstanceVariable *objcInstanceVariable = nullptr;
  auto result = _symbols.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(XPIKind::ObjectiveCInstanceVariable, name),
      std::forward_as_tuple(nullptr));

  if (result.second) {
    objcInstanceVariable =
        ObjCInstanceVariable::create(allocator, name, access);
    result.first->second = objcInstanceVariable;
  } else {
    objcInstanceVariable = cast<ObjCInstanceVariable>(result.first->second);
  }

  auto success = objcInstanceVariable->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    objcInstanceVariable->addAvailabilityInfo(arch);

  return objcInstanceVariable;
}

ObjCSelector *XPISet::addObjCSelector(ObjCContainer *container, StringRef name,
                                      ArchitectureSet archs,
                                      bool isInstanceMethod, bool isDynamic,
                                      XPIAccess access) {
  const auto *objcClass = container;
  if (auto *category = dyn_cast<ObjCCategory>(container))
    objcClass = category->getBaseClass();

  name = copyString(name);
  auto result = _selectors.emplace(
      std::piecewise_construct,
      std::forward_as_tuple(objcClass->getName(), name, isInstanceMethod,
                            isa<ObjCProtocol>(objcClass)),
      std::forward_as_tuple(nullptr));
  ObjCSelector *objcSelector = nullptr;
  if (result.second) {
    objcSelector = ObjCSelector::create(allocator, name, isInstanceMethod,
                                        isDynamic, access);
    result.first->second = objcSelector;
  } else {
    objcSelector = cast<ObjCSelector>(result.first->second);
  }

  auto success = objcSelector->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    objcSelector->addAvailabilityInfo(arch);

  // Record a reference in the container (class, category, or protocol).
  container->addSelector(objcSelector);

  return objcSelector;
}

ObjCCategory *XPISet::addObjCCategory(ObjCClass *baseClass, StringRef name,
                                      ArchitectureSet archs, XPIAccess access) {
  name = copyString(name);
  ObjCCategory *objcCategory = nullptr;
  auto result =
      _categories.emplace(std::piecewise_construct,
                          std::forward_as_tuple(baseClass->getName(), name),
                          std::forward_as_tuple(nullptr));

  if (result.second) {
    objcCategory = ObjCCategory::create(allocator, baseClass, name, access);
    result.first->second = objcCategory;
  } else {
    objcCategory = cast<ObjCCategory>(result.first->second);
  }

  auto success = objcCategory->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    objcCategory->addAvailabilityInfo(arch);

  // Record a reference in the base class.
  baseClass->addCategory(objcCategory);

  return objcCategory;
}

ObjCProtocol *XPISet::addObjCProtocol(StringRef name, ArchitectureSet archs,
                                      XPIAccess access) {
  name = copyString(name);
  ObjCProtocol *objcProtocol = nullptr;
  auto result =
      _protocols.emplace(std::piecewise_construct, std::forward_as_tuple(name),
                         std::forward_as_tuple(nullptr));

  if (result.second) {
    objcProtocol = ObjCProtocol::create(allocator, name, access);
    result.first->second = objcProtocol;
  } else {
    objcProtocol = cast<ObjCProtocol>(result.first->second);
  }

  auto success = objcProtocol->updateAccess(access);
  assert(success && "Access is not equal");
  (void)success;

  for (auto arch : archs)
    objcProtocol->addAvailabilityInfo(arch);

  return objcProtocol;
}

const XPI *XPISet::findSymbol(XPIKind kind, StringRef name) const {
  assert((kind == XPIKind::GlobalSymbol || kind == XPIKind::ObjectiveCClass ||
          kind == XPIKind::ObjectiveCClassEHType ||
          kind == XPIKind::ObjectiveCInstanceVariable) &&
         "Not a symbol kind");
  auto it = _symbols.find({kind, name});
  if (it != _symbols.end())
    return it->second;
  return nullptr;
}

const XPI *XPISet::findSymbol(const XPI &xpi) const {
  return findSymbol(xpi.getKind(), xpi.getName());
}

bool XPISet::removeSymbol(XPIKind kind, StringRef name) {
  assert((kind == XPIKind::GlobalSymbol || kind == XPIKind::ObjectiveCClass ||
          kind == XPIKind::ObjectiveCClassEHType ||
          kind == XPIKind::ObjectiveCInstanceVariable) &&
         "Not a symbol kind");
  auto it = _symbols.find({kind, name});
  if (it == _symbols.end())
    return false;

  _symbols.erase(it);
  return true;
}

const ObjCSelector *XPISet::findSelector(const SelectorsMapKey &key) const {
  auto it = _selectors.find(key);
  if (it != _selectors.end())
    return it->second;

  return nullptr;
}

const ObjCCategory *XPISet::findCategory(const CategoriesMapKey &key) const {
  auto it = _categories.find(key);
  if (it != _categories.end())
    return it->second;

  return nullptr;
}

const ObjCCategory *XPISet::findCategory(const ObjCCategory *category) const {
  return findCategory(CategoriesMapKey{category->getBaseClass()->getName(),
                                       category->getName()});
}

const ObjCProtocol *XPISet::findProtocol(StringRef key) const {
  auto it = _protocols.find(key);
  if (it != _protocols.end())
    return it->second;

  return nullptr;
}

TAPI_NAMESPACE_INTERNAL_END
