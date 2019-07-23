//===- tapi/Core/IntefaceFileBase.h - TAPI Interface File Base --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Abstract base class for interface files.
///
//===----------------------------------------------------------------------===//

#ifndef TAPI_CORE_INTERFACE_FILE_BASE_H
#define TAPI_CORE_INTERFACE_FILE_BASE_H

#include "Architecture.h"
#include "ArchitectureSet.h"
#include "ArchitectureSupport.h"
#include "File.h"
#include "Platform.h"
#include "STLExtras.h"
#include "Defines.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
namespace yaml {
template <typename T> struct MappingTraits;
}
} // namespace llvm

TAPI_NAMESPACE_INTERNAL_BEGIN

class InterfaceFileRef {
public:
  InterfaceFileRef() = default;

  InterfaceFileRef(StringRef installName) : _installName(installName) {}

  InterfaceFileRef(StringRef installName, ArchitectureSet archs)
      : _installName(installName), _architectures(archs) {}

  StringRef getInstallName() const { return _installName; };
  void setArchitectures(ArchitectureSet archs) { _architectures |= archs; }
  ArchitectureSet getArchitectures() const { return _architectures; }
  bool hasArchitecture(Architecture arch) const {
    return _architectures.has(arch);
  }
  void clearArchitectures() { _architectures = Architecture::unknown; }

  bool operator==(const InterfaceFileRef &o) const {
    return std::tie(_installName, _architectures) ==
           std::tie(o._installName, o._architectures);
  }

  bool operator!=(const InterfaceFileRef &o) const {
    return std::tie(_installName, _architectures) !=
           std::tie(o._installName, o._architectures);
  }

  bool operator<(const InterfaceFileRef &o) const {
    return std::tie(_installName, _architectures) <
           std::tie(o._installName, o._architectures);
  }

private:
  std::string _installName;
  ArchitectureSet _architectures;

  template <typename T> friend struct llvm::yaml::MappingTraits;
};

raw_ostream &operator<<(raw_ostream &os, const InterfaceFileRef &ref);

const DiagnosticBuilder &operator<<(const DiagnosticBuilder &db,
                                    const InterfaceFileRef &ref);

class InterfaceFileBase : public File {
public:
  static bool classof(const File *file) {
    return (file->kind() == File::Kind::InterfaceFileBase) ||
           (file->kind() == File::Kind::InterfaceFile) ||
           (file->kind() == File::Kind::ExtendedInterfaceFile);
  }

  void setPlatform(Platform platform) {
    _platform = platform;
  }
  Platform getPlatform() const { return _platform; }

  void setArchitectures(ArchitectureSet archs) { _architectures |= archs; }
  void setArch(Architecture arch) { _architectures.set(arch); }
  ArchitectureSet getArchitectures() const { return _architectures; }
  void clearArchitectures() { _architectures = Architecture::unknown; }

  void setInstallName(StringRef installName) { _installName = installName; }
  StringRef getInstallName() const { return _installName; }

  void setCurrentVersion(PackedVersion version) { _currentVersion = version; }
  PackedVersion getCurrentVersion() const { return _currentVersion; }

  void setCompatibilityVersion(PackedVersion version) {
    _compatibilityVersion = version;
  }
  PackedVersion getCompatibilityVersion() const {
    return _compatibilityVersion;
  }

  void setSwiftABIVersion(uint8_t version) { _swiftABIVersion = version; }
  uint8_t getSwiftABIVersion() const { return _swiftABIVersion; }

  void setTwoLevelNamespace(bool v = true) { _isTwoLevelNamespace = v; }
  bool isTwoLevelNamespace() const { return _isTwoLevelNamespace; }

  void setApplicationExtensionSafe(bool v = true) { _isAppExtensionSafe = v; }
  bool isApplicationExtensionSafe() const { return _isAppExtensionSafe; }

  void setObjCConstraint(ObjCConstraint constraint) {
    _objcConstraint = constraint;
  }
  ObjCConstraint getObjCConstraint() const { return _objcConstraint; }

  void setInstallAPI(bool v = true) { _isInstallAPI = v; }
  bool isInstallAPI() const { return _isInstallAPI; }

  void setParentUmbrella(StringRef parent) { _parentUmbrella = parent; }
  StringRef getParentUmbrella() const { return _parentUmbrella; }

  void addAllowableClient(StringRef installName, ArchitectureSet archs);
  const std::vector<InterfaceFileRef> &allowableClients() const {
    return _allowableClients;
  }

  void addReexportedLibrary(StringRef installName, ArchitectureSet archs);
  bool removeReexportedLibrary(StringRef installName);
  const std::vector<InterfaceFileRef> &reexportedLibraries() const {
    return _reexportedLibraries;
  }

  void addUUID(Architecture arch, StringRef uuid);
  void addUUID(uint8_t uuid[16], Architecture arch);
  const std::vector<std::pair<Architecture, std::string>> &uuids() const {
    return _uuids;
  }
  void clearUUIDs() { _uuids.clear(); }

  bool convertTo(FileType fileType);

  void inlineFramework(std::shared_ptr<InterfaceFileBase> framework);

protected:
  InterfaceFileBase(File::Kind kind) : File(kind) {}
  InterfaceFileBase(InterfaceFileBase &&) = default;

  Platform _platform = Platform::unknown;
  ArchitectureSet _architectures;
  std::string _installName;
  PackedVersion _currentVersion;
  PackedVersion _compatibilityVersion;
  uint8_t _swiftABIVersion = 0;
  bool _isTwoLevelNamespace = false;
  bool _isAppExtensionSafe = false;
  bool _isInstallAPI = false;
  ObjCConstraint _objcConstraint = ObjCConstraint::None;
  std::string _parentUmbrella;
  std::vector<InterfaceFileRef> _allowableClients;
  std::vector<InterfaceFileRef> _reexportedLibraries;
  std::vector<std::pair<Architecture, std::string>> _uuids;
};

TAPI_NAMESPACE_INTERNAL_END

#endif // TAPI_CORE_INTERFACE_FILE_BASE_H
