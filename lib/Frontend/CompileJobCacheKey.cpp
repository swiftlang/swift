//===--- CompileJobCacheKey.cpp - compile cache key methods ---------------===//
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
// This file contains utility methods for creating compile job cache keys.
//
//===----------------------------------------------------------------------===//

#include "swift/Option/Options.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/HierarchicalTreeBuilder.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/CAS/TreeEntry.h"
#include "llvm/CAS/TreeSchema.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Endian.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include <llvm/ADT/SmallString.h>
#include <swift/Basic/Version.h>
#include <swift/Frontend/CompileJobCacheKey.h>

using namespace swift;

// TODO: Rewrite this into CASNodeSchema.
llvm::Expected<llvm::cas::ObjectRef> swift::createCompileJobBaseCacheKey(
    llvm::cas::ObjectStore &CAS, ArrayRef<const char *> Args) {
  // Don't count the `-frontend` in the first location since only frontend
  // invocation can have a cache key.
  if (Args.size() > 1 && StringRef(Args.front()) == "-frontend")
    Args = Args.drop_front();

  unsigned MissingIndex;
  unsigned MissingCount;
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  llvm::opt::InputArgList ParsedArgs = Table->ParseArgs(
      Args, MissingIndex, MissingCount, options::FrontendOption);

  SmallString<256> CommandLine;
  for (auto *Arg : ParsedArgs) {
    const auto &Opt = Arg->getOption();

    // Skip the options that doesn't affect caching.
    if (Opt.hasFlag(options::CacheInvariant))
      continue;

    if (Opt.hasFlag(options::ArgumentIsFileList)) {
      auto FileList = llvm::MemoryBuffer::getFile(Arg->getValue());
      if (!FileList)
        return llvm::errorCodeToError(FileList.getError());
      CommandLine.append(Opt.getRenderName());
      CommandLine.push_back(0);
      CommandLine.append((*FileList)->getBuffer());
      CommandLine.push_back(0);
      continue;
    }

    CommandLine.append(Arg->getAsString(ParsedArgs));
    CommandLine.push_back(0);
  }

  llvm::cas::HierarchicalTreeBuilder Builder;
  auto CMD = CAS.storeFromString(/*Refs*/ {}, CommandLine);
  if (!CMD)
    return CMD.takeError();
  Builder.push(*CMD, llvm::cas::TreeEntry::Regular, "command-line");

  // FIXME: The version is maybe insufficient...
  auto Version =
      CAS.storeFromString(/*Refs*/ {}, version::getSwiftFullVersion());
  if (!Version)
    return Version.takeError();
  Builder.push(*Version, llvm::cas::TreeEntry::Regular, "version");

  if (auto Out = Builder.create(CAS))
    return Out->getRef();
  else
    return Out.takeError();
}

llvm::Expected<llvm::cas::ObjectRef>
swift::createCompileJobCacheKeyForOutput(llvm::cas::ObjectStore &CAS,
                                         llvm::cas::ObjectRef BaseKey,
                                         unsigned InputIndex) {
  std::string InputInfo;
  llvm::raw_string_ostream OS(InputInfo);

  // CacheKey is the index of the producting input + the base key.
  // Encode the unsigned value as little endian in the field.
  llvm::support::endian::write<uint32_t>(OS, InputIndex,
                                         llvm::endianness::little);

  return CAS.storeFromString({BaseKey}, OS.str());
}

static llvm::Error validateCacheKeyNode(llvm::cas::ObjectProxy Proxy) {
  if (Proxy.getData().size() != sizeof(uint32_t))
    return llvm::createStringError("incorrect size for cache key node");
  if (Proxy.getNumReferences() != 1)
    return llvm::createStringError("incorrect child number for cache key node");

  return llvm::Error::success();
}

llvm::Error swift::printCompileJobCacheKey(llvm::cas::ObjectStore &CAS,
                                           llvm::cas::ObjectRef Key,
                                           llvm::raw_ostream &OS) {
  auto Proxy = CAS.getProxy(Key);
  if (!Proxy)
    return Proxy.takeError();
  if (auto Err = validateCacheKeyNode(*Proxy))
    return Err;

  uint32_t InputIndex = llvm::support::endian::read<uint32_t>(
      Proxy->getData().data(), llvm::endianness::little);

  auto Base = Proxy->getReference(0);
  llvm::cas::TreeSchema Schema(CAS);
  auto Tree = Schema.load(Base);
  if (!Tree)
    return Tree.takeError();

  std::string BaseStr;
  llvm::raw_string_ostream BaseOS(BaseStr);
  auto Err = Tree->forEachEntry(
      [&](const llvm::cas::NamedTreeEntry &Entry) -> llvm::Error {
        auto Ref = Entry.getRef();
        auto DataProxy = CAS.getProxy(Ref);
        if (!DataProxy)
          return DataProxy.takeError();

        BaseOS.indent(2) << Entry.getName() << "\n";
        StringRef Line, Remain = DataProxy->getData();
        while (!Remain.empty()) {
          std::tie(Line, Remain) = Remain.split(0);
          BaseOS.indent(4) << Line << "\n";
        }
        return llvm::Error::success();
      });
  if (Err)
    return Err;

  llvm::outs() << "Cache Key " << CAS.getID(Key).toString() << "\n";
  llvm::outs() << "Swift Compiler Invocation Info:\n";
  llvm::outs() << BaseStr;
  llvm::outs() << "Input index: " << InputIndex << "\n";

  return llvm::Error::success();
}

llvm::Error
swift::iterateCommandLine(llvm::cas::ObjectStore &CAS, llvm::cas::ObjectRef Key,
                          std::function<llvm::Error(StringRef)> Callback) {
  auto Proxy = CAS.getProxy(Key);
  if (!Proxy)
    return Proxy.takeError();

  if (auto Err = validateCacheKeyNode(*Proxy))
    return Err;

  auto Base = Proxy->getReference(0);
  llvm::cas::TreeSchema Schema(CAS);
  auto Tree = Schema.load(Base);
  if (!Tree)
    return Tree.takeError();

  std::string BaseStr;
  llvm::raw_string_ostream BaseOS(BaseStr);
  return Tree->forEachEntry(
      [&](const llvm::cas::NamedTreeEntry &Entry) -> llvm::Error {
        auto Ref = Entry.getRef();
        auto DataProxy = CAS.getProxy(Ref);
        if (!DataProxy)
          return DataProxy.takeError();

        if (Entry.getName() != "command-line")
          return llvm::Error::success();

        StringRef Line, Remain = DataProxy->getData();
        while (!Remain.empty()) {
          std::tie(Line, Remain) = Remain.split(0);
          if (auto Err = Callback(Line))
            return Err;
        }
        return llvm::Error::success();
      });
}
