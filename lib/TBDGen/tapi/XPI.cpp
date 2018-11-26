//===- lib/Core/XPI.cpp - TAPI XPI ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Implements the XPI set
///
//===----------------------------------------------------------------------===//

#include "XPI.h"
#include "Defines.h"
#include "llvm/Demangle/Demangle.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

bool XPI::isExportedSymbol() const {
  switch (getKind()) {
  case XPIKind::GlobalSymbol:
  case XPIKind::ObjectiveCClass:
  case XPIKind::ObjectiveCClassEHType:
  case XPIKind::ObjectiveCInstanceVariable:
    break;
  case XPIKind::ObjCCategory:
  case XPIKind::ObjCProtocol:
  case XPIKind::ObjCSelector:
    return false;
  }

  switch (getAccess()) {
  case XPIAccess::Exported:
  case XPIAccess::Public:
  case XPIAccess::Private:
  case XPIAccess::Project:
    break;
  case XPIAccess::Internal:
  case XPIAccess::Unknown:
    return false;
  }

  return isAvailable();
}

std::string XPI::getPrettyName(bool demangle) const {
  if (!demangle)
    return _name;

  if (demangle && _name.startswith("__Z")) {
    int status = 0;
    char *demangledName = itaniumDemangle(_name.substr(1).str().c_str(),
                                          nullptr, nullptr, &status);
    if (status == 0) {
      std::string result = demangledName;
      free(demangledName);
      return result;
    }
  }

  if (_name[0] == '_')
    return _name.substr(1);

  return _name;
}

std::string XPI::getAnnotatedName(bool demangle) const {
  std::string name;
  if (isWeakDefined())
    name += "(weak-def) ";
  if (isWeakReferenced())
    name += "(weak-ref) ";
  if (isThreadLocalValue())
    name += "(tlv) ";
  switch (getKind()) {
  case XPIKind::GlobalSymbol:
    return name + getPrettyName(demangle);
  case XPIKind::ObjectiveCClass:
    return name + "(ObjC Class) " + _name.str();
  case XPIKind::ObjectiveCClassEHType:
    return name + "(ObjC Class EH) " + _name.str();
  case XPIKind::ObjectiveCInstanceVariable:
    return name + "(ObjC IVar) " + _name.str();
  case XPIKind::ObjCSelector: {
    auto selector = cast<ObjCSelector>(this);
    if (selector->isInstanceMethod())
      return name + "(ObjC Instance Method) " + _name.str();

    return name + "(ObjC Class Method) " + _name.str();
  }
  case XPIKind::ObjCCategory:
    return name + "(ObjC Category) " + _name.str();
  case XPIKind::ObjCProtocol:
    return name + "(ObjC Protocol) " + _name.str();
  }
  llvm_unreachable("unknown kind");
}

void XPI::print(raw_ostream &os) const {
  switch (getAccess()) {
  case XPIAccess::Unknown:
    os << "(unknown) ";
    break;
  case XPIAccess::Exported:
    os << "(exported) ";
    break;
  case XPIAccess::Public:
    os << "(public) ";
    break;
  case XPIAccess::Private:
    os << "(private) ";
    break;
  case XPIAccess::Project:
    os << "(project) ";
    break;
  case XPIAccess::Internal:
    os << "(internal) ";
    break;
  }
  os << getAnnotatedName();
  for (const auto &avail : _availability)
    os << " [" << avail.first << ": " << avail.second << "]";
}

GlobalSymbol *GlobalSymbol::create(BumpPtrAllocator &A, StringRef name,
                                   XPIAccess access, SymbolFlags flags) {
  return new (A) GlobalSymbol(name, access, flags);
}

ObjCInstanceVariable *ObjCInstanceVariable::create(BumpPtrAllocator &A,
                                                   StringRef name,
                                                   XPIAccess access) {
  return new (A) ObjCInstanceVariable(name, access);
}

ObjCProtocol *ObjCProtocol::create(BumpPtrAllocator &A, StringRef name,
                                   XPIAccess access) {
  return new (A) ObjCProtocol(name, access);
}

void ObjCContainer::addSelector(const ObjCSelector *selector) {
  auto it = find(_selectors, selector);
  if (it != _selectors.end())
    return;

  auto insertAt =
      lower_bound(_selectors, selector,
                  [](const ObjCSelector *selector, const ObjCSelector *value) {
                    return static_cast<int>(selector->isInstanceMethod()) <
                               static_cast<int>(value->isInstanceMethod()) ||
                           selector->getName() < value->getName();
                  });

  _selectors.insert(insertAt, selector);
}

const ObjCSelector *ObjCContainer::findSelector(StringRef name,
                                                bool isInstanceMethod) const {
  auto it = find_if(_selectors,
                    [&name, isInstanceMethod](const ObjCSelector *selector) {
                      return selector->isInstanceMethod() == isInstanceMethod &&
                             selector->getName() == name;
                    });

  if (it != _selectors.end())
    return *it;

  return nullptr;
}

ObjCClass *ObjCClass::create(BumpPtrAllocator &A, StringRef name,
                             XPIAccess access) {
  return new (A) ObjCClass(name, access);
}

void ObjCClass::addCategory(const ObjCCategory *category) {
  auto it = find(_categories, category);
  if (it != _categories.end())
    return;

  auto insertAt =
      lower_bound(_categories, category,
                  [](const ObjCCategory *category, const ObjCCategory *value) {
                    return category->getName() < value->getName();
                  });
  _categories.insert(insertAt, category);
}

ObjCClassEHType *ObjCClassEHType::create(BumpPtrAllocator &A, StringRef name,
                                         XPIAccess access) {
  return new (A) ObjCClassEHType(name, access);
}

ObjCCategory *ObjCCategory::create(BumpPtrAllocator &A, ObjCClass *baseClass,
                                   StringRef name, XPIAccess access) {
  return new (A) ObjCCategory(baseClass, name, access);
}

ObjCSelector *ObjCSelector::create(BumpPtrAllocator &A, StringRef name,
                                   bool isInstanceMethod, bool isDynamic,
                                   XPIAccess access,
                                   bool isDerivedFromProtocol) {
  return new (A) ObjCSelector(name, isInstanceMethod, isDynamic, access,
                              isDerivedFromProtocol);
}

TAPI_NAMESPACE_INTERNAL_END
