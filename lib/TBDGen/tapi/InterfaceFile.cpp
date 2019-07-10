//===- lib/Core/InterfaceFile.cpp - Interface File --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Implements the Interface File
///
//===----------------------------------------------------------------------===//

#include "InterfaceFile.h"
#include "ExtendedInterfaceFile.h"
#include "TapiError.h"
#include "XPI.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

static SymbolKind convertXPIKindToSymbolKind(XPIKind kind) {
  switch (kind) {
  default:
    llvm_unreachable("unexpected XPI kind");
  case XPIKind::GlobalSymbol:
    return SymbolKind::GlobalSymbol;
  case XPIKind::ObjectiveCClass:
    return SymbolKind::ObjectiveCClass;
  case XPIKind::ObjectiveCClassEHType:
    return SymbolKind::ObjectiveCClassEHType;
  case XPIKind::ObjectiveCInstanceVariable:
    return SymbolKind::ObjectiveCInstanceVariable;
  }
}

InterfaceFile::InterfaceFile(ExtendedInterfaceFile &&other)
    : InterfaceFileBase(std::forward<InterfaceFileBase>(other)) {
  setKind(File::Kind::InterfaceFile);
  for (const auto *symbol : other.exports())
    addSymbolImpl(convertXPIKindToSymbolKind(symbol->getKind()),
                  symbol->getName(), symbol->getArchitectures(),
                  symbol->getSymbolFlags());

  for (const auto *symbol : other.undefineds())
    addUndefinedSymbolImpl(convertXPIKindToSymbolKind(symbol->getKind()),
                           symbol->getName(), symbol->getArchitectures(),
                           symbol->getSymbolFlags());
}

void InterfaceFile::addSymbolImpl(SymbolKind kind, StringRef name,
                                  ArchitectureSet archs, SymbolFlags flags,
                                  bool copyStrings) {
  if (copyStrings)
    name = copyString(name);
  _symbols.emplace_back(new (allocator) Symbol{kind, name, archs, flags});
}

void InterfaceFile::addSymbol(SymbolKind kind, StringRef name,
                              ArchitectureSet archs, SymbolFlags flags,
                              bool copyStrings) {
  auto it = find_if(_symbols, [kind, name](const Symbol *symbol) {
    return (symbol->getKind() == kind) && (symbol->getName() == name);
  });

  if (it != _symbols.end()) {
    (*it)->setArchitectures(archs);
    return;
  }

  addSymbolImpl(kind, name, archs, flags, copyStrings);
}

void InterfaceFile::addUndefinedSymbolImpl(SymbolKind kind, StringRef name,
                                           ArchitectureSet archs,
                                           SymbolFlags flags,
                                           bool copyStrings) {
  if (copyStrings)
    name = copyString(name);
  _undefineds.emplace_back(new (allocator) Symbol{kind, name, archs, flags});
}

void InterfaceFile::addUndefinedSymbol(SymbolKind kind, StringRef name,
                                       ArchitectureSet archs, SymbolFlags flags,
                                       bool copyStrings) {
  auto it = find_if(_undefineds, [kind, name](const Symbol *symbol) {
    return (symbol->getKind() == kind) && (symbol->getName() == name);
  });

  if (it != _undefineds.end()) {
    (*it)->setArchitectures(archs);
    return;
  }

  addUndefinedSymbolImpl(kind, name, archs, flags, copyStrings);
}

bool InterfaceFile::contains(SymbolKind kind, StringRef name,
                             Symbol const **result) const {
  auto it = find_if(_symbols, [kind, name](const Symbol *symbol) {
    return (symbol->getKind() == kind) && (symbol->getName() == name);
  });

  if (it != _symbols.end()) {
    if (result)
      *result = *it;
    return true;
  }

  return false;
}

Expected<std::unique_ptr<InterfaceFile>>
InterfaceFile::extract(Architecture arch) const {
  if (!_architectures.has(arch)) {
    return make_error<StringError>("file doesn't have architecture '" +
                                       getArchName(arch) + "'",
                                   inconvertibleErrorCode());
  }

  std::unique_ptr<InterfaceFile> interface(new InterfaceFile());
  interface->setFileType(getFileType());
  interface->setPath(getPath());
  interface->setPlatform(getPlatform());
  interface->setArch(arch);
  interface->setInstallName(getInstallName());
  interface->setCurrentVersion(getCurrentVersion());
  interface->setCompatibilityVersion(getCompatibilityVersion());
  interface->setSwiftABIVersion(getSwiftABIVersion());
  interface->setTwoLevelNamespace(isTwoLevelNamespace());
  interface->setApplicationExtensionSafe(isApplicationExtensionSafe());
  interface->setInstallAPI(isInstallAPI());
  interface->setObjCConstraint(getObjCConstraint());
  interface->setParentUmbrella(getParentUmbrella());

  for (const auto &lib : allowableClients())
    if (lib.hasArchitecture(arch))
      interface->addAllowableClient(lib.getInstallName(), arch);

  for (const auto &lib : reexportedLibraries())
    if (lib.hasArchitecture(arch))
      interface->addReexportedLibrary(lib.getInstallName(), arch);

  for (const auto &uuid : uuids())
    if (uuid.first == arch)
      interface->addUUID(arch, uuid.second);

  for (const auto *symbol : symbols())
    if (symbol->getArchitectures().has(arch))
      interface->addSymbol(symbol->getKind(), symbol->getName(), arch,
                           symbol->getFlags());

  for (const auto *symbol : undefineds())
    if (symbol->getArchitectures().has(arch))
      interface->addUndefinedSymbol(symbol->getKind(), symbol->getName(), arch,
                                    symbol->getFlags());

  return std::move(interface);
}

Expected<std::unique_ptr<InterfaceFile>>
InterfaceFile::remove(Architecture arch) const {
  if (_architectures == arch)
    return make_error<StringError>("cannot remove last architecture slice '" +
                                       getArchName(arch) + "'",
                                   inconvertibleErrorCode());

  if (!_architectures.has(arch))
    return make_error<TapiError>(TapiErrorCode::NoSuchArchitecture);

  std::unique_ptr<InterfaceFile> interface(new InterfaceFile());
  interface->setFileType(getFileType());
  interface->setPath(getPath());
  interface->setPlatform(getPlatform());
  ArchitectureSet archs = getArchitectures();
  archs.clear(arch);
  interface->setArchitectures(archs);
  interface->setInstallName(getInstallName());
  interface->setCurrentVersion(getCurrentVersion());
  interface->setCompatibilityVersion(getCompatibilityVersion());
  interface->setSwiftABIVersion(getSwiftABIVersion());
  interface->setTwoLevelNamespace(isTwoLevelNamespace());
  interface->setApplicationExtensionSafe(isApplicationExtensionSafe());
  interface->setInstallAPI(isInstallAPI());
  interface->setObjCConstraint(getObjCConstraint());
  interface->setParentUmbrella(getParentUmbrella());

  for (const auto &lib : allowableClients()) {
    auto archs = lib.getArchitectures();
    archs.clear(arch);
    if (archs.empty())
      continue;
    interface->addAllowableClient(lib.getInstallName(), archs);
  }

  for (const auto &lib : reexportedLibraries()) {
    auto archs = lib.getArchitectures();
    archs.clear(arch);
    if (archs.empty())
      continue;
    interface->addReexportedLibrary(lib.getInstallName(), archs);
  }

  for (const auto &uuid : uuids())
    if (uuid.first != arch)
      interface->addUUID(uuid.first, uuid.second);

  for (const auto *symbol : symbols()) {
    auto archs = symbol->getArchitectures();
    archs.clear(arch);
    if (archs.empty())
      continue;
    interface->addSymbol(symbol->getKind(), symbol->getName(), archs,
                         symbol->getFlags());
  }

  for (const auto *symbol : undefineds()) {
    auto archs = symbol->getArchitectures();
    archs.clear(arch);
    if (archs.empty())
      continue;
    interface->addUndefinedSymbol(symbol->getKind(), symbol->getName(), archs,
                                  symbol->getFlags());
  }

  return std::move(interface);
}

Expected<std::unique_ptr<InterfaceFile>>
InterfaceFile::merge(const InterfaceFile *otherInterface,
                     bool allowArchitectureMerges) const {
  // Verify files can be merged.
  if (getFileType() != otherInterface->getFileType()) {
    return make_error<StringError>("file types do not match",
                                   inconvertibleErrorCode());
  }

  if (!allowArchitectureMerges) {
    if ((getArchitectures() & otherInterface->getArchitectures()) !=
        Architecture::unknown) {
      return make_error<StringError>("architectures overlap",
                                     inconvertibleErrorCode());
    }
  }

  if (getPlatform() != otherInterface->getPlatform()) {
    return make_error<StringError>("platforms do not match",
                                   inconvertibleErrorCode());
  }

  if (getInstallName() != otherInterface->getInstallName()) {
    return make_error<StringError>("install names do not match",
                                   inconvertibleErrorCode());
  }

  if (getCurrentVersion() != otherInterface->getCurrentVersion()) {
    return make_error<StringError>("current versions do not match",
                                   inconvertibleErrorCode());
  }

  if (getCompatibilityVersion() != otherInterface->getCompatibilityVersion()) {
    return make_error<StringError>("compatibility versions do not match",
                                   inconvertibleErrorCode());
  }

  if ((getSwiftABIVersion() != 0) &&
      (otherInterface->getSwiftABIVersion() != 0) &&
      (getSwiftABIVersion() != otherInterface->getSwiftABIVersion())) {
    return make_error<StringError>("swift ABI versions do not match",
                                   inconvertibleErrorCode());
  }

  if (isTwoLevelNamespace() != otherInterface->isTwoLevelNamespace()) {
    return make_error<StringError>("two level namespace flags do not match",
                                   inconvertibleErrorCode());
  }

  if (isApplicationExtensionSafe() !=
      otherInterface->isApplicationExtensionSafe()) {
    return make_error<StringError>(
        "application extension safe flags do not match",
        inconvertibleErrorCode());
  }

  if (isInstallAPI() != otherInterface->isInstallAPI()) {
    return make_error<StringError>("installapi flags do not match",
                                   inconvertibleErrorCode());
  }

  if ((getObjCConstraint() != ObjCConstraint::None) &&
      (otherInterface->getObjCConstraint() != ObjCConstraint::None) &&
      (getObjCConstraint() != otherInterface->getObjCConstraint())) {
    return make_error<StringError>("installapi flags do not match",
                                   inconvertibleErrorCode());
  }

  if (getParentUmbrella() != otherInterface->getParentUmbrella()) {
    return make_error<StringError>("parent umbrellas do not match",
                                   inconvertibleErrorCode());
  }

  std::unique_ptr<InterfaceFile> interface(new InterfaceFile());
  interface->setFileType(getFileType());
  interface->setPath(getPath());
  interface->setPlatform(getPlatform());
  interface->setInstallName(getInstallName());
  interface->setCurrentVersion(getCurrentVersion());
  interface->setCompatibilityVersion(getCompatibilityVersion());

  if (getSwiftABIVersion() == 0)
    interface->setSwiftABIVersion(otherInterface->getSwiftABIVersion());
  else
    interface->setSwiftABIVersion(getSwiftABIVersion());

  interface->setTwoLevelNamespace(isTwoLevelNamespace());
  interface->setApplicationExtensionSafe(isApplicationExtensionSafe());
  interface->setInstallAPI(isInstallAPI());

  if (getObjCConstraint() == ObjCConstraint::None)
    interface->setObjCConstraint(otherInterface->getObjCConstraint());
  else
    interface->setObjCConstraint(getObjCConstraint());

  interface->setParentUmbrella(getParentUmbrella());

  interface->setArchitectures(getArchitectures() |
                              otherInterface->getArchitectures());

  for (const auto &lib : allowableClients())
    interface->addAllowableClient(lib.getInstallName(), lib.getArchitectures());

  for (const auto &lib : otherInterface->allowableClients())
    interface->addAllowableClient(lib.getInstallName(), lib.getArchitectures());

  for (const auto &lib : reexportedLibraries())
    interface->addReexportedLibrary(lib.getInstallName(),
                                    lib.getArchitectures());

  for (const auto &lib : otherInterface->reexportedLibraries())
    interface->addReexportedLibrary(lib.getInstallName(),
                                    lib.getArchitectures());

  for (const auto &uuid : uuids())
    interface->addUUID(uuid.first, uuid.second);

  for (const auto &uuid : otherInterface->uuids())
    interface->addUUID(uuid.first, uuid.second);

  for (const auto *symbol : symbols())
    interface->addSymbol(symbol->getKind(), symbol->getName(),
                         symbol->getArchitectures(), symbol->getFlags());

  for (const auto *symbol : otherInterface->symbols())
    interface->addSymbol(symbol->getKind(), symbol->getName(),
                         symbol->getArchitectures(), symbol->getFlags());

  for (const auto *symbol : undefineds())
    interface->addUndefinedSymbol(symbol->getKind(), symbol->getName(),
                                  symbol->getArchitectures(),
                                  symbol->getFlags());

  for (const auto *symbol : otherInterface->undefineds())
    interface->addUndefinedSymbol(symbol->getKind(), symbol->getName(),
                                  symbol->getArchitectures(),
                                  symbol->getFlags());

  return std::move(interface);
}

void InterfaceFile::printSymbolsForArch(Architecture arch) const {
  std::vector<std::string> exports;
  for (const auto *symbol : this->symbols()) {
    if (!symbol->getArchitectures().has(arch))
      continue;

    switch (symbol->getKind()) {
    case SymbolKind::GlobalSymbol:
      exports.emplace_back(symbol->getName());
      break;
    case SymbolKind::ObjectiveCClass:
      if (getPlatform() == Platform::macOS && arch == Architecture::i386) {
        exports.emplace_back(".objc_class_name_" + symbol->getName().str());
      } else {
        exports.emplace_back("_OBJC_CLASS_$_" + symbol->getName().str());
        exports.emplace_back("_OBJC_METACLASS_$_" + symbol->getName().str());
      }
      break;
    case SymbolKind::ObjectiveCClassEHType:
      exports.emplace_back("_OBJC_EHTYPE_$_" + symbol->getName().str());
      break;
    case SymbolKind::ObjectiveCInstanceVariable:
      exports.emplace_back("_OBJC_IVAR_$_" + symbol->getName().str());
      break;
    }
  }

  sort(exports);
  for (const auto &symbol : exports)
    outs() << symbol << "\n";
}

void InterfaceFile::printSymbols(ArchitectureSet archs) const {
  if (archs.empty())
    archs = getArchitectures();

  if (getArchitectures().contains(archs)) {
    bool firstItr = true;
    for (auto arch : getArchitectures()) {
      if (!archs.has(arch))
        continue;

      if (firstItr)
        firstItr = false;
      else
        outs() << "\n";
      if (archs.count() > 1)
        outs() << getPath() << " (for architecture " << arch << "):\n";
      printSymbolsForArch(arch);
    }
  } else {
    outs() << "file: " << getPath()
           << " does not contain architecture: " << archs << "\n";
  }
}

TAPI_NAMESPACE_INTERNAL_END
