//===--- CASOutputBackends.cpp ----------------------------------*- C++ -*-===//
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

#include "swift/Frontend/CASOutputBackends.h"

#include "swift/Basic/FileTypes.h"
#include "swift/Frontend/CachingUtils.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/CompileJobCacheResult.h"
#include "clang/Frontend/CompileJobCacheResult.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CAS/HierarchicalTreeBuilder.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Debug.h"
#include <optional>

#define DEBUG_TYPE "swift-cas-backend"

using namespace swift;
using namespace llvm;
using namespace llvm::cas;
using namespace llvm::vfs;

namespace {
class SwiftCASOutputFile final : public OutputFileImpl {
public:
  Error keep() override { return OnKeep(Path, Bytes); }
  Error discard() override { return Error::success(); }
  raw_pwrite_stream &getOS() override { return OS; }

  using OnKeepType = llvm::unique_function<Error(StringRef, StringRef)>;
  SwiftCASOutputFile(StringRef Path, OnKeepType OnKeep)
      : Path(Path.str()), OS(Bytes), OnKeep(std::move(OnKeep)) {}

private:
  std::string Path;
  SmallString<16> Bytes;
  raw_svector_ostream OS;
  OnKeepType OnKeep;
};
} // namespace

namespace swift::cas {
void SwiftCASOutputBackend::anchor() {}

class SwiftCASOutputBackend::Implementation {
public:
  Implementation(ObjectStore &CAS, ActionCache &Cache, ObjectRef BaseKey,
                 const FrontendInputsAndOutputs &InputsAndOutputs,
                 FrontendOptions::ActionType Action)
      : CAS(CAS), Cache(Cache), BaseKey(BaseKey),
        InputsAndOutputs(InputsAndOutputs), Action(Action) {
    initBackend(InputsAndOutputs);
  }

  llvm::Expected<std::unique_ptr<llvm::vfs::OutputFileImpl>>
  createFileImpl(llvm::StringRef ResolvedPath,
                 llvm::Optional<llvm::vfs::OutputConfig> Config) {
    auto ProducingInput = OutputToInputMap.find(ResolvedPath);
    assert(ProducingInput != OutputToInputMap.end() && "Unknown output file");

    std::string InputFilename = ProducingInput->second.first.getFileName();
    auto OutputType = ProducingInput->second.second;

    // Uncached output kind.
    if (file_types::isProducedFromDiagnostics(OutputType))
      return std::make_unique<llvm::vfs::NullOutputFileImpl>();

    return std::make_unique<SwiftCASOutputFile>(
        ResolvedPath,
        [this, InputFilename, OutputType](StringRef Path,
                                          StringRef Bytes) -> Error {
          return storeImpl(Path, Bytes, InputFilename, OutputType);
        });
  }

  void initBackend(const FrontendInputsAndOutputs &InputsAndOutputs);

  Error storeImpl(StringRef Path, StringRef Bytes, StringRef CorrespondingInput,
                  file_types::ID OutputKind);

  Error finalizeCacheKeysFor(StringRef Input);

private:
  friend class SwiftCASOutputBackend;
  ObjectStore &CAS;
  ActionCache &Cache;
  ObjectRef BaseKey;
  const FrontendInputsAndOutputs &InputsAndOutputs;
  FrontendOptions::ActionType Action;

  StringMap<std::pair<const InputFile &, file_types::ID>> OutputToInputMap;
  StringMap<DenseMap<file_types::ID, ObjectRef>> OutputRefs;
};

SwiftCASOutputBackend::SwiftCASOutputBackend(
    ObjectStore &CAS, ActionCache &Cache, ObjectRef BaseKey,
    const FrontendInputsAndOutputs &InputsAndOutputs,
    FrontendOptions::ActionType Action)
    : Impl(*new SwiftCASOutputBackend::Implementation(
          CAS, Cache, BaseKey, InputsAndOutputs, Action)) {}

SwiftCASOutputBackend::~SwiftCASOutputBackend() { delete &Impl; }

IntrusiveRefCntPtr<OutputBackend> SwiftCASOutputBackend::cloneImpl() const {
  return makeIntrusiveRefCnt<SwiftCASOutputBackend>(
      Impl.CAS, Impl.Cache, Impl.BaseKey, Impl.InputsAndOutputs, Impl.Action);
}

Expected<std::unique_ptr<OutputFileImpl>>
SwiftCASOutputBackend::createFileImpl(StringRef ResolvedPath,
                                      Optional<OutputConfig> Config) {
  return Impl.createFileImpl(ResolvedPath, Config);
}

file_types::ID SwiftCASOutputBackend::getOutputFileType(StringRef Path) const {
  return file_types::lookupTypeForExtension(llvm::sys::path::extension(Path));
}

Error SwiftCASOutputBackend::storeImpl(StringRef Path, StringRef Bytes,
                                       StringRef CorrespondingInput,
                                       file_types::ID OutputKind) {
  return Impl.storeImpl(Path, Bytes, CorrespondingInput, OutputKind);
}

Error SwiftCASOutputBackend::storeCachedDiagnostics(StringRef InputFile,
                                                    StringRef Bytes) {
  return storeImpl("<cached-diagnostics>", Bytes, InputFile,
                   file_types::ID::TY_CachedDiagnostics);
}

void SwiftCASOutputBackend::Implementation::initBackend(
    const FrontendInputsAndOutputs &InputsAndOutputs) {
  // FIXME: The output to input map might not be enough for example all the
  // outputs can be written to `-`, but the backend cannot distinguish which
  // input it actually comes from. Maybe the solution is just not to cache
  // any commands write output to `-`.
  file_types::ID mainOutputType = InputsAndOutputs.getPrincipalOutputType();
  auto addInput = [&](const InputFile &Input) {
    if (!Input.outputFilename().empty())
      OutputToInputMap.insert(
          {Input.outputFilename(), {Input, mainOutputType}});
    Input.getPrimarySpecificPaths()
        .SupplementaryOutputs.forEachSetOutputAndType(
            [&](const std::string &Out, file_types::ID ID) {
              if (!file_types::isProducedFromDiagnostics(ID))
                OutputToInputMap.insert({Out, {Input, ID}});
            });
  };
  llvm::for_each(InputsAndOutputs.getAllInputs(), addInput);
}

Error SwiftCASOutputBackend::Implementation::storeImpl(
    StringRef Path, StringRef Bytes, StringRef CorrespondingInput,
    file_types::ID OutputKind) {
  Optional<ObjectRef> BytesRef;
  if (Error E = CAS.storeFromString(None, Bytes).moveInto(BytesRef))
    return E;

  LLVM_DEBUG(llvm::dbgs() << "DEBUG: producing CAS output of type \'"
                          << file_types::getTypeName(OutputKind)
                          << "\' for input \'" << CorrespondingInput << "\': \'"
                          << CAS.getID(*BytesRef).toString() << "\'\n";);

  OutputRefs[CorrespondingInput].insert({OutputKind, *BytesRef});

  return finalizeCacheKeysFor(CorrespondingInput);
}

Error SwiftCASOutputBackend::Implementation::finalizeCacheKeysFor(
    StringRef Input) {
  auto Entry = OutputRefs.find(Input);
  assert(Entry != OutputRefs.end() && "Unexpected input");

  // If not all outputs for the input are emitted, return.
  if (!llvm::all_of(OutputToInputMap, [&](auto &E) {
        return (E.second.first.getFileName() != Input ||
                Entry->second.count(E.second.second));
      }))
    return Error::success();

  std::vector<std::pair<file_types::ID, ObjectRef>> OutputsForInput;
  llvm::for_each(Entry->second, [&OutputsForInput](auto E) {
    OutputsForInput.emplace_back(E.first, E.second);
  });
  // Sort to a stable ordering for deterministic output cache object.
  llvm::sort(OutputsForInput,
             [](auto &LHS, auto &RHS) { return LHS.first < RHS.first; });

  Optional<ObjectRef> Result;
  // Use a clang compatible result CAS object schema when emiting PCM.
  if (Action == FrontendOptions::ActionType::EmitPCM) {
    clang::cas::CompileJobCacheResult::Builder Builder;

    for (auto &Outs : OutputsForInput) {
      if (Outs.first == file_types::ID::TY_ClangModuleFile)
        Builder.addOutput(
            clang::cas::CompileJobCacheResult::OutputKind::MainOutput,
            Outs.second);
      else if (Outs.first == file_types::ID::TY_CachedDiagnostics)
        Builder.addOutput(clang::cas::CompileJobCacheResult::OutputKind::
                              SerializedDiagnostics,
                          Outs.second);
      else if (Outs.first == file_types::ID::TY_Dependencies)
        Builder.addOutput(
            clang::cas::CompileJobCacheResult::OutputKind::Dependencies,
            Outs.second);
      else
        llvm_unreachable("Unexpected output when compiling clang module");
    }

    if (auto Err = Builder.build(CAS).moveInto(Result))
      return Err;

  } else {
    swift::cas::CompileJobCacheResult::Builder Builder;

    for (auto &Outs : OutputsForInput)
      Builder.addOutput(Outs.first, Outs.second);

    if (auto Err = Builder.build(CAS).moveInto(Result))
      return Err;
  }

  auto CacheKey = createCompileJobCacheKeyForOutput(CAS, BaseKey, Input);
  if (!CacheKey)
    return CacheKey.takeError();

  LLVM_DEBUG(llvm::dbgs() << "DEBUG: writing cache entry for input \'" << Input
                          << "\': \'" << CAS.getID(*CacheKey).toString()
                          << "\' => \'" << CAS.getID(*Result).toString()
                          << "\'\n";);

  if (auto E = Cache.put(CAS.getID(*CacheKey), CAS.getID(*Result)))
    return E;

  return Error::success();
}

} // end namespace swift::cas
