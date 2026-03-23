//===--- ModuleImportExtractor.cpp - Extract imports from .swiftmodule ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Serialization/ModuleImportExtractor.h"
#include "ModuleFileSharedCore.h"
#include "swift/Basic/PathRemapper.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;

int swift::extractModuleImports(StringRef modulePath,
                                llvm::raw_ostream &out) {
  auto bufOrErr = llvm::MemoryBuffer::getFile(modulePath);
  if (auto EC = bufOrErr.getError()) {
    llvm::errs() << "error: could not open module file '" << modulePath
                 << "': " << EC.message() << '\n';
    return 1;
  }

  // Load the module without an ASTContext. We pass no interface paths, no
  // doc/source-info buffers, no SDK constraint, and no target triple so that
  // loading succeeds regardless of build configuration.
  std::shared_ptr<const ModuleFileSharedCore> loadedModule;
  PathObfuscator pathRecoverer;
  auto loadInfo = ModuleFileSharedCore::load(
      /*moduleInterfacePath=*/"", /*moduleInterfaceSourcePath=*/"",
      std::move(bufOrErr.get()), /*moduleDocInputBuffer=*/nullptr,
      /*moduleSourceInfoInputBuffer=*/nullptr, /*isFramework=*/false,
      /*requiredSDK=*/"", /*target=*/std::nullopt, pathRecoverer,
      loadedModule);

  // Accept Valid and some "soft" mismatches -- we only need the dependency
  // table, which is available whenever the control block parses.
  switch (loadInfo.status) {
  case serialization::Status::Valid:
  case serialization::Status::RevisionIncompatible:
  case serialization::Status::ChannelIncompatible:
  case serialization::Status::TargetIncompatible:
  case serialization::Status::TargetTooNew:
  case serialization::Status::SDKMismatch:
    break;
  default:
    llvm::errs() << "error: failed to load module '" << modulePath
                 << "': " << serialization::StatusToString(loadInfo.status)
                 << '\n';
    return 1;
  }

  out << "// Imports extracted from module: " << loadedModule->getName()
      << '\n';

  for (auto &dep : loadedModule->getDependencies()) {
    // Emit SPI group attributes before the import.
    StringRef spiGroups = dep.RawSPIs;
    while (!spiGroups.empty()) {
      auto split = spiGroups.split('\0');
      StringRef group = split.first;
      if (!group.empty())
        out << "@_spi(" << group << ") ";
      spiGroups = split.second;
    }

    // Emit attribute/access-level prefix based on the import control kind.
    if (dep.isExported()) {
      out << "@_exported ";
    } else if (dep.isImplementationOnly()) {
      out << "@_implementationOnly ";
    } else if (dep.isSPIOnly()) {
      out << "@_spiOnly ";
    } else if (dep.isPackageOnly()) {
      out << "package ";
    } else if (dep.isInternalOrBelow()) {
      out << "internal ";
    }
    // Default imports get no prefix.

    out << "import " << dep.getPrettyPrintedPath();

    if (dep.isScoped())
      out << " /* scoped */";

    if (dep.isHeader())
      out << " /* bridging header */";

    out << '\n';
  }

  return 0;
}
