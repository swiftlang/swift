//===------CASOutputBackends.h-- ---------------------------------*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_CASOUTPUTBACKENDS_H
#define SWIFT_FRONTEND_CASOUTPUTBACKENDS_H

#include "swift/Frontend/FrontendInputsAndOutputs.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/CAS/ActionCache.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/VirtualOutputBackends.h"
#include "llvm/Support/VirtualOutputFile.h"

namespace swift::cas {

/// Write the data of the CAS object \p Object to the file at \p Path,
/// honoring \p Config.
///
/// When possible, this uses \c ObjectStore::exportDataToFile, which can avoid
/// copying the data (e.g. by cloning the file from the CAS storage). For
/// configurations that it doesn't support (e.g. writing to stdout or appending)
/// the data is written through \p FallbackBackend instead.
llvm::Error exportCASObjectToFile(const llvm::cas::ObjectProxy &Object,
                                  llvm::StringRef Path,
                                  llvm::vfs::OutputConfig Config,
                                  llvm::vfs::OutputBackend &FallbackBackend);

/// An output backend that stores the compiler outputs into the CAS and
/// associates them with the compile job cache key.
///
/// If a \c DiskBackend is provided, the outputs are also written to disk: the
/// outputs that are stored directly into the CAS are exported from the CAS,
/// and the rest of the outputs are written through \c DiskBackend.
class SwiftCASOutputBackend : public llvm::vfs::OutputBackend {
  void anchor() override;

protected:
  llvm::IntrusiveRefCntPtr<OutputBackend> cloneImpl() const override;

  llvm::Expected<std::unique_ptr<llvm::vfs::OutputFileImpl>>
  createFileImpl(llvm::StringRef ResolvedPath,
                 std::optional<llvm::vfs::OutputConfig> Config) override;

  virtual llvm::Error storeImpl(llvm::StringRef Path, llvm::StringRef Bytes,
                                unsigned InputIndex, file_types::ID OutputKind);

private:
  file_types::ID getOutputFileType(llvm::StringRef Path) const;

  /// Return true if the file type is stored into CAS Backend directly.
  static bool isStoredDirectly(file_types::ID Kind);

public:
  SwiftCASOutputBackend(
      llvm::cas::ObjectStore &CAS, llvm::cas::ActionCache &Cache,
      llvm::cas::ObjectRef BaseKey,
      const FrontendInputsAndOutputs &InputsAndOutputs,
      const FrontendOptions &Opts, FrontendOptions::ActionType Action,
      bool WriteOutputHashXAttr,
      llvm::IntrusiveRefCntPtr<llvm::vfs::OutputBackend> DiskBackend = nullptr);
  ~SwiftCASOutputBackend();

  llvm::Error storeCachedDiagnostics(unsigned InputIndex,
                                     llvm::StringRef Bytes);

  llvm::Error storeSupplementaryOutputFile(StringRef OutputFilename,
                                           llvm::StringRef Bytes);

  /// Store the MCCAS CASID \p ID as the object file output for the input
  /// that corresponds to the \p OutputFilename
  llvm::Error storeMCCASObjectID(StringRef OutputFilename, llvm::cas::CASID ID);

private:
  class Implementation;
  Implementation &Impl;
};

} // end namespace swift::cas

#endif
