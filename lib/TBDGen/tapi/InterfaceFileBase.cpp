//===- lib/Core/InterfaceFileBase.cpp - Interface File Base -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Implements the Interface File Base
///
//===----------------------------------------------------------------------===//

#include "InterfaceFileBase.h"
#include "STLExtras.h"
#include "clang/Basic/Diagnostic.h"
#include <iomanip>
#include <sstream>

using namespace llvm;

TAPI_NAMESPACE_INTERNAL_BEGIN

raw_ostream &operator<<(raw_ostream &os, const InterfaceFileRef &ref) {
  os << ref.getInstallName() << ref.getArchitectures();
  return os;
}

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    const InterfaceFileRef &ref) {
  auto str = ref.getInstallName().str() + " [ " +
             std::string(ref.getArchitectures()) + " ]";
  db.AddString(str);
  return db;
}

namespace {
template <typename C>
typename C::iterator addEntry(C &container, StringRef installName) {
  auto it = lower_bound(container, installName,
                        [](const InterfaceFileRef &lhs, const StringRef &rhs) {
                          return lhs.getInstallName() < rhs;
                        });
  if ((it != std::end(container)) && !(installName < it->getInstallName()))
    return it;

  return container.emplace(it, installName);
}
} // end anonymous namespace.

void InterfaceFileBase::addAllowableClient(StringRef installName,
                                           ArchitectureSet archs) {
  auto client = addEntry(_allowableClients, installName);
  client->setArchitectures(archs);
}

void InterfaceFileBase::addReexportedLibrary(StringRef installName,
                                             ArchitectureSet archs) {
  auto lib = addEntry(_reexportedLibraries, installName);
  lib->setArchitectures(archs);
}

bool InterfaceFileBase::removeReexportedLibrary(StringRef installName) {
  auto it = lower_bound(_reexportedLibraries, installName,
                        [](const InterfaceFileRef &lhs, const StringRef &rhs) {
                          return lhs.getInstallName() < rhs;
                        });

  if ((it == _reexportedLibraries.end()) ||
      (installName < it->getInstallName()))
    return false;

  _reexportedLibraries.erase(it);
  return true;
}

void InterfaceFileBase::addUUID(Architecture arch, StringRef uuid) {
  auto it = lower_bound(_uuids, arch,
                        [](const std::pair<Architecture, std::string> &lhs,
                           Architecture rhs) { return lhs.first < rhs; });

  if ((it != _uuids.end()) && !(arch < it->first)) {
    it->second = uuid;
    return;
  }

  _uuids.emplace(it, arch, uuid);
  return;
}

void InterfaceFileBase::addUUID(uint8_t uuid[16], Architecture arch) {
  std::stringstream stream;
  for (unsigned i = 0; i < 16; ++i) {
    if (i == 4 || i == 6 || i == 8 || i == 10)
      stream << '-';
    stream << std::setfill('0') << std::setw(2) << std::uppercase << std::hex
           << static_cast<int>(uuid[i]);
  }
  addUUID(arch, stream.str());
}

bool InterfaceFileBase::convertTo(FileType fileType) {
  switch (fileType) {
  default:
    return false;
  case FileType::TBD_V1:
  case FileType::TBD_V2:
  case FileType::TBD_V3:
  case FileType::API_V1:
  case FileType::SPI_V1:
    break;
  }

  if ((fileType == FileType::TBD_V1) &&
      (!isTwoLevelNamespace() || !isApplicationExtensionSafe()))
    return false;

  setFileType(fileType);

  return true;
}

void InterfaceFileBase::inlineFramework(
    std::shared_ptr<InterfaceFileBase> framework) {
  auto addFramework = [&](std::shared_ptr<InterfaceFileBase> &&framework) {
    auto it =
        lower_bound(_documents, framework->getInstallName(),
                    [](std::shared_ptr<File> &lhs, const std::string &rhs) {
                      return std::static_pointer_cast<InterfaceFileBase>(lhs)
                                 ->getInstallName() < rhs;
                    });

    if ((it != _documents.end()) &&
        !(framework->getInstallName() <
          std::static_pointer_cast<InterfaceFileBase>(*it)->getInstallName()))
      return;

    _documents.emplace(it, std::move(framework));
  };
  for (auto &doc : framework->_documents)
    addFramework(std::static_pointer_cast<InterfaceFileBase>(doc));

  framework->_documents.clear();
  addFramework(std::move(framework));
}

TAPI_NAMESPACE_INTERNAL_END
