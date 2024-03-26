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
#include <swift/Frontend/CompileJobCacheKey.h>
#include <swift/Basic/Version.h>
#include <llvm/ADT/SmallString.h>
#include "llvm/ADT/STLExtras.h"
#include "llvm/CAS/HierarchicalTreeBuilder.h"
#include "llvm/CAS/ObjectStore.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/EndianStream.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"

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
  auto CMD = CAS.storeFromString(std::nullopt, CommandLine);
  if (!CMD)
    return CMD.takeError();
  Builder.push(*CMD, llvm::cas::TreeEntry::Regular, "command-line");

  // FIXME: The version is maybe insufficient...
  auto Version =
      CAS.storeFromString(std::nullopt, version::getSwiftFullVersion());
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
  llvm::support::endian::write<uint32_t>(OS, InputIndex, llvm::support::little);

  return CAS.storeFromString({BaseKey}, OS.str());
}
