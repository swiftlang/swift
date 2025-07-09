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

#include "swift/Basic/Assertions.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Frontend/CachingUtils.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/CompileJobCacheResult.h"
#include "clang/Frontend/CompileJobCacheResult.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CAS/HierarchicalTreeBuilder.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Mutex.h"
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
                 const FrontendOptions &Opts,
                 FrontendOptions::ActionType Action)
      : CAS(CAS), Cache(Cache), BaseKey(BaseKey),
        InputsAndOutputs(InputsAndOutputs), Opts(Opts), Action(Action) {
    initBackend(InputsAndOutputs);
  }

  llvm::Expected<std::unique_ptr<llvm::vfs::OutputFileImpl>>
  createFileImpl(llvm::StringRef ResolvedPath,
                 std::optional<llvm::vfs::OutputConfig> Config) {
    auto ProducingInput = OutputToInputMap.find(ResolvedPath);
    if (ProducingInput == OutputToInputMap.end() &&
        ResolvedPath.starts_with(Opts.SymbolGraphOutputDir)) {
      return std::make_unique<SwiftCASOutputFile>(
          ResolvedPath, [this](StringRef Path, StringRef Bytes) -> Error {
            bool Shortened = Path.consume_front(Opts.SymbolGraphOutputDir);
            assert(Shortened && "symbol graph path outside output dir");
            (void)Shortened;
            std::optional<ObjectRef> PathRef;
            if (Error E =
                    CAS.storeFromString(std::nullopt, Path).moveInto(PathRef))
              return E;
            std::optional<ObjectRef> BytesRef;
            if (Error E =
                    CAS.storeFromString(std::nullopt, Bytes).moveInto(BytesRef))
              return E;
            auto Ref = CAS.store({*PathRef, *BytesRef}, {});
            if (!Ref)
              return Ref.takeError();
            SymbolGraphOutputRefs.push_back(*Ref);
            return Error::success();
          });
    }
    assert(ProducingInput != OutputToInputMap.end() && "Unknown output file");

    unsigned InputIndex = ProducingInput->second.first;
    auto OutputType = ProducingInput->second.second;

    // Uncached output kind.
    if (!isStoredDirectly(OutputType))
      return std::make_unique<llvm::vfs::NullOutputFileImpl>();

    return std::make_unique<SwiftCASOutputFile>(
        ResolvedPath,
        [this, InputIndex, OutputType](StringRef Path,
                                       StringRef Bytes) -> Error {
          return storeImpl(Path, Bytes, InputIndex, OutputType);
        });
  }

  void initBackend(const FrontendInputsAndOutputs &InputsAndOutputs);

  Error storeImpl(StringRef Path, StringRef Bytes, unsigned InputIndex,
                  file_types::ID OutputKind);

  // Return true if all the outputs are produced for the given index.
  bool addProducedOutput(unsigned InputIndex, file_types::ID OutputKind,
                         ObjectRef BytesRef);
  Error finalizeCacheKeysFor(unsigned InputIndex);

private:
  friend class SwiftCASOutputBackend;
  ObjectStore &CAS;
  ActionCache &Cache;
  ObjectRef BaseKey;
  const FrontendInputsAndOutputs &InputsAndOutputs;
  const FrontendOptions &Opts;
  FrontendOptions::ActionType Action;

  // Lock for updating output file status.
  llvm::sys::SmartMutex<true> OutputLock;

  // Map from output path to the input index and output kind.
  StringMap<std::pair<unsigned, file_types::ID>> OutputToInputMap;

  // A vector of output refs where the index is the input index.
  SmallVector<DenseMap<file_types::ID, ObjectRef>> OutputRefs;

  SmallVector<ObjectRef> SymbolGraphOutputRefs;
};

SwiftCASOutputBackend::SwiftCASOutputBackend(
    ObjectStore &CAS, ActionCache &Cache, ObjectRef BaseKey,
    const FrontendInputsAndOutputs &InputsAndOutputs,
    const FrontendOptions &Opts, FrontendOptions::ActionType Action)
    : Impl(*new SwiftCASOutputBackend::Implementation(
          CAS, Cache, BaseKey, InputsAndOutputs, Opts, Action)) {}

SwiftCASOutputBackend::~SwiftCASOutputBackend() { delete &Impl; }

bool SwiftCASOutputBackend::isStoredDirectly(file_types::ID Kind) {
  return !file_types::isProducedFromDiagnostics(Kind) &&
         Kind != file_types::TY_Dependencies;
}

IntrusiveRefCntPtr<OutputBackend> SwiftCASOutputBackend::cloneImpl() const {
  return makeIntrusiveRefCnt<SwiftCASOutputBackend>(
      Impl.CAS, Impl.Cache, Impl.BaseKey, Impl.InputsAndOutputs, Impl.Opts,
      Impl.Action);
}

Expected<std::unique_ptr<OutputFileImpl>>
SwiftCASOutputBackend::createFileImpl(StringRef ResolvedPath,
                                      std::optional<OutputConfig> Config) {
  return Impl.createFileImpl(ResolvedPath, Config);
}

file_types::ID SwiftCASOutputBackend::getOutputFileType(StringRef Path) const {
  return file_types::lookupTypeForExtension(llvm::sys::path::extension(Path));
}

Error SwiftCASOutputBackend::storeImpl(StringRef Path, StringRef Bytes,
                                       unsigned InputIndex,
                                       file_types::ID OutputKind) {
  return Impl.storeImpl(Path, Bytes, InputIndex, OutputKind);
}

Error SwiftCASOutputBackend::storeCachedDiagnostics(unsigned InputIndex,
                                                    StringRef Bytes) {
  return storeImpl("<cached-diagnostics>", Bytes, InputIndex,
                   file_types::ID::TY_CachedDiagnostics);
}

Error SwiftCASOutputBackend::storeMakeDependenciesFile(StringRef OutputFilename,
                                                       llvm::StringRef Bytes) {
  auto Input = Impl.OutputToInputMap.find(OutputFilename);
  if (Input == Impl.OutputToInputMap.end())
    return llvm::createStringError("InputIndex for output file not found!");
  auto InputIndex = Input->second.first;
  assert(Input->second.second == file_types::TY_Dependencies &&
         "wrong output type");
  return storeImpl(OutputFilename, Bytes, InputIndex,
                   file_types::TY_Dependencies);
}

Error SwiftCASOutputBackend::storeMCCASObjectID(StringRef OutputFilename,
                                                llvm::cas::CASID ID) {
  auto Input = Impl.OutputToInputMap.find(OutputFilename);
  if (Input == Impl.OutputToInputMap.end())
    return llvm::createStringError("InputIndex for output file not found!");
  auto InputIndex = Input->second.first;
  auto MCRef = Impl.CAS.getReference(ID);
  if (!MCRef)
    return createStringError("Invalid CASID: " + ID.toString() +
                             ". No associated ObjectRef found!");

  if (Impl.addProducedOutput(InputIndex, file_types::TY_Object, *MCRef))
    return Impl.finalizeCacheKeysFor(InputIndex);

  return Error::success();
}

void SwiftCASOutputBackend::Implementation::initBackend(
    const FrontendInputsAndOutputs &InputsAndOutputs) {
  // FIXME: The output to input map might not be enough for example all the
  // outputs can be written to `-`, but the backend cannot distinguish which
  // input it actually comes from. Maybe the solution is just not to cache
  // any commands write output to `-`.
  file_types::ID mainOutputType = InputsAndOutputs.getPrincipalOutputType();
  auto addInput = [&](const InputFile &Input, unsigned Index) {
    // Ignore the outputFilename for typecheck action since it is not producing
    // an output file for that.
    if (!Input.outputFilename().empty() &&
        Action != FrontendOptions::ActionType::Typecheck)
      OutputToInputMap.insert(
          {Input.outputFilename(), {Index, mainOutputType}});
    Input.getPrimarySpecificPaths()
        .SupplementaryOutputs.forEachSetOutputAndType(
            [&](const std::string &Out, file_types::ID ID) {
              // FIXME: Opt Remarks are not setup correctly to be cached and
              // didn't go through the output backend. Do not cache them.
              if (ID == file_types::ID::TY_YAMLOptRecord ||
                  ID == file_types::ID::TY_BitstreamOptRecord)
                return;

              if (!file_types::isProducedFromDiagnostics(ID))
                OutputToInputMap.insert({Out, {Index, ID}});
            });
  };

  for (unsigned idx = 0; idx < InputsAndOutputs.getAllInputs().size(); ++idx)
    addInput(InputsAndOutputs.getAllInputs()[idx], idx);

  // FIXME: Cached diagnostics is associated with the first output producing
  // input file.
  OutputToInputMap.insert(
      {"<cached-diagnostics>",
       {InputsAndOutputs.getIndexOfFirstOutputProducingInput(),
        file_types::TY_CachedDiagnostics}});

  // Resize the output refs to hold all inputs.
  OutputRefs.resize(InputsAndOutputs.getAllInputs().size());
}

Error SwiftCASOutputBackend::Implementation::storeImpl(
    StringRef Path, StringRef Bytes, unsigned InputIndex,
    file_types::ID OutputKind) {
  std::optional<ObjectRef> BytesRef;
  if (Error E = CAS.storeFromString(std::nullopt, Bytes).moveInto(BytesRef))
    return E;

  LLVM_DEBUG(llvm::dbgs() << "DEBUG: producing CAS output of type \'"
                          << file_types::getTypeName(OutputKind)
                          << "\' for input \'" << InputIndex << "\': \'"
                          << CAS.getID(*BytesRef).toString() << "\'\n";);

  if (addProducedOutput(InputIndex, OutputKind, *BytesRef))
    return finalizeCacheKeysFor(InputIndex);
  return Error::success();
}

bool SwiftCASOutputBackend::Implementation::addProducedOutput(
    unsigned InputIndex, file_types::ID OutputKind,
    ObjectRef BytesRef) {
  llvm::sys::SmartScopedLock<true> LockOutput(OutputLock);
  auto &ProducedOutputs = OutputRefs[InputIndex];
  ProducedOutputs.insert({OutputKind, BytesRef});

  return llvm::all_of(OutputToInputMap, [&](auto &E) {
    return (E.second.first != InputIndex ||
            ProducedOutputs.count(E.second.second));
  });
}

Error SwiftCASOutputBackend::Implementation::finalizeCacheKeysFor(
    unsigned InputIndex) {
  const auto &ProducedOutputs = OutputRefs[InputIndex];
  assert(!ProducedOutputs.empty() && "Expect outputs for this input");

  std::vector<std::pair<file_types::ID, ObjectRef>> OutputsForInput;
  llvm::for_each(ProducedOutputs, [&OutputsForInput](auto E) {
    OutputsForInput.emplace_back(E.first, E.second);
  });
  if (InputIndex == InputsAndOutputs.getIndexOfFirstOutputProducingInput() &&
      !SymbolGraphOutputRefs.empty()) {
    auto SGRef = CAS.store(SymbolGraphOutputRefs, {});
    if (!SGRef)
      return SGRef.takeError();
    OutputsForInput.emplace_back(file_types::ID::TY_SymbolGraphFile, *SGRef);
  }
  // Sort to a stable ordering for deterministic output cache object.
  llvm::sort(OutputsForInput,
             [](auto &LHS, auto &RHS) { return LHS.first < RHS.first; });

  std::optional<ObjectRef> Result;
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

  auto CacheKey = createCompileJobCacheKeyForOutput(CAS, BaseKey, InputIndex);
  if (!CacheKey)
    return CacheKey.takeError();

  LLVM_DEBUG(llvm::dbgs() << "DEBUG: writing cache entry for input \'"
                          << InputIndex << "\': \'"
                          << CAS.getID(*CacheKey).toString() << "\' => \'"
                          << CAS.getID(*Result).toString() << "\'\n";);

  if (auto E = Cache.put(CAS.getID(*CacheKey), CAS.getID(*Result))) {
    // If `SWIFT_STRICT_CAS_ERRORS` environment is set, do not attempt to
    // recover from error.
    if (::getenv("SWIFT_STRICT_CAS_ERRORS"))
      return E;
    // Failed to update the action cache, this can happen when there is a
    // non-deterministic output from compiler. Check that the cache entry is
    // not missing, if so, then assume this is a cache poisoning that might
    // be benign and the build might continue without problem.
    auto Check = Cache.get(CAS.getID(*CacheKey));
    if (!Check) {
      // A real CAS error, return the original error.
      consumeError(Check.takeError());
      return E;
    }
    if (!*Check)
      return E;
    // Emit an error message to stderr but don't fail the job. Ideally this
    // should be sent to a DiagnosticEngine but CASBackend lives longer than
    // diagnostic engine and needs to capture all diagnostic outputs before
    // sending this request.
    llvm::errs() << "error: failed to update cache: " << toString(std::move(E))
                 << "\n";
  }

  return Error::success();
}

} // end namespace swift::cas
