//===--- CompileJobCacheResult.cpp - compile cache result schema ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains the cache schema for swift cached compile job result.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/CompileJobCacheResult.h"
#include "swift/Basic/FileTypes.h"

using namespace swift;
using namespace swift::cas;
using namespace llvm;
using namespace llvm::cas;

Error CompileJobCacheResult::forEachOutput(
    llvm::function_ref<Error(Output)> Callback) const {
  size_t Count = getNumOutputs();
  for (size_t I = 0; I < Count; ++I) {
    file_types::ID Kind = getOutputKind(I);
    ObjectRef Object = getOutputObject(I);
    if (auto Err = Callback({Object, Kind}))
      return Err;
  }
  return Error::success();
}

Error CompileJobCacheResult::forEachLoadedOutput(
    llvm::function_ref<Error(Output, std::optional<ObjectProxy>)> Callback) {
  // Kick-off materialization for all outputs concurrently.
  SmallVector<std::future<AsyncProxyValue>, 4> FutureOutputs;
  size_t Count = getNumOutputs();
  for (size_t I = 0; I < Count; ++I) {
    ObjectRef Ref = getOutputObject(I);
    FutureOutputs.push_back(getCAS().getProxyFuture(Ref));
  }

  // Make sure all the outputs have materialized.
  std::optional<Error> OccurredError;
  SmallVector<std::optional<ObjectProxy>, 4> Outputs;
  for (auto &FutureOutput : FutureOutputs) {
    auto Obj = FutureOutput.get().take();
    if (!Obj) {
      if (!OccurredError)
        OccurredError = Obj.takeError();
      else
        OccurredError =
            llvm::joinErrors(std::move(*OccurredError), Obj.takeError());
      continue;
    }
    Outputs.push_back(*Obj);
  }
  if (OccurredError)
    return std::move(*OccurredError);

  // Pass the loaded outputs.
  for (size_t I = 0; I < Count; ++I) {
    if (auto Err = Callback({getOutputObject(I), getOutputKind(I)}, Outputs[I]))
      return Err;
  }
  return Error::success();
}

CompileJobCacheResult::Output CompileJobCacheResult::getOutput(size_t I) const {
  return Output{getOutputObject(I), getOutputKind(I)};
}

std::optional<CompileJobCacheResult::Output>
CompileJobCacheResult::getOutput(file_types::ID Kind) const {
  size_t Count = getNumOutputs();
  for (size_t I = 0; I < Count; ++I) {
    file_types::ID K = getOutputKind(I);
    if (Kind == K)
      return Output{getOutputObject(I), Kind};
  }
  return {};
}

Error CompileJobCacheResult::print(llvm::raw_ostream &OS) {
  return forEachOutput([&](Output O) -> Error {
    OS << file_types::getTypeName(O.Kind) << "    " << getCAS().getID(O.Object)
       << '\n';
    return Error::success();
  });
}

size_t CompileJobCacheResult::getNumOutputs() const { return getData().size(); }
ObjectRef CompileJobCacheResult::getOutputObject(size_t I) const {
  return getReference(I);
}

file_types::ID
CompileJobCacheResult::getOutputKind(size_t I) const {
  return static_cast<file_types::ID>(getData()[I]);
}

CompileJobCacheResult::CompileJobCacheResult(const ObjectProxy &Obj)
    : ObjectProxy(Obj) {}

struct CompileJobCacheResult::Builder::PrivateImpl {
  SmallVector<ObjectRef> Objects;
  SmallVector<file_types::ID> Kinds;

  struct KindMap {
    file_types::ID Kind;
    std::string Path;
  };
  SmallVector<KindMap> KindMaps;
};

CompileJobCacheResult::Builder::Builder() : Impl(*new PrivateImpl) {}
CompileJobCacheResult::Builder::~Builder() { delete &Impl; }

void CompileJobCacheResult::Builder::addKindMap(file_types::ID Kind,
                                                StringRef Path) {
  Impl.KindMaps.push_back({Kind, std::string(Path)});
}
void CompileJobCacheResult::Builder::addOutput(file_types::ID Kind,
                                               ObjectRef Object) {
  Impl.Kinds.push_back(Kind);
  Impl.Objects.push_back(Object);
}
Error CompileJobCacheResult::Builder::addOutput(StringRef Path,
                                                ObjectRef Object) {
  Impl.Objects.push_back(Object);
  for (auto &KM : Impl.KindMaps) {
    if (KM.Path == Path) {
      Impl.Kinds.push_back(KM.Kind);
      return Error::success();
    }
  }
  return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                 "cached output file has unknown path '" +
                                     Path + "'");
}

Expected<ObjectRef> CompileJobCacheResult::Builder::build(ObjectStore &CAS) {
  CompileJobResultSchema Schema(CAS);
  // The resulting Refs contents are:
  // Object 0...N, SchemaKind
  SmallVector<ObjectRef> Refs;
  std::swap(Impl.Objects, Refs);
  Refs.push_back(Schema.getKindRef());
  return CAS.store(Refs, {(char *)Impl.Kinds.begin(), Impl.Kinds.size()});
}

static constexpr llvm::StringLiteral CompileJobResultSchemaName =
    "swift::cas::schema::compile_job_result::v1";

char CompileJobResultSchema::ID = 0;

CompileJobResultSchema::CompileJobResultSchema(ObjectStore &CAS)
    : CompileJobResultSchema::RTTIExtends(CAS),
      KindRef(
          llvm::cantFail(CAS.storeFromString({}, CompileJobResultSchemaName))) {
}

Expected<CompileJobCacheResult>
CompileJobResultSchema::load(ObjectRef Ref) const {
  auto Proxy = CAS.getProxy(Ref);
  if (!Proxy)
    return Proxy.takeError();
  if (!isNode(*Proxy))
    return llvm::createStringError(llvm::inconvertibleErrorCode(),
                                   "not a compile job result");
  return CompileJobCacheResult(*Proxy);
}

bool CompileJobResultSchema::isRootNode(const ObjectProxy &Node) const {
  return isNode(Node);
}

bool CompileJobResultSchema::isNode(const ObjectProxy &Node) const {
  size_t N = Node.getNumReferences();
  return N && Node.getReference(N - 1) == getKindRef();
}
