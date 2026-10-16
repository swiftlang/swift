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
#include "llvm/CAS/CASReference.h"
#include "llvm/CAS/ObjectStore.h"
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

// The base key is a CAS node with the following layout:
//   data: per-job arguments, i.e. the primary inputs.
//   refs: the references at the fixed positions in CompileJobBaseKeyRef,
//         followed by the CAS objects referenced by the entries in the stable
//         command-line arguments and the per-job arguments, in the order they
//         appear.
// Each argument list is a sequence of NUL-terminated entries, and each entry
// starts with an EntryKind. The clang include tree options are not in the
// argument lists since they are stored at the fixed positions.
namespace {
enum class EntryKind : char {
  /// A regular argument.
  Argument = 'A',
  /// An argument with a CAS ID that is stored as the next reference. The entry
  /// is the argument without the CAS ID.
  CASID = 'C',
  /// An argument with a file list whose content is stored as the next
  /// reference. The entry is the option spelling.
  FileList = 'F',
  /// The input files on the command-line. The list of inputs, separated by
  /// NUL, is stored as the next reference. The entry is empty.
  InputList = 'I',
};
} // namespace

static constexpr unsigned refIndex(CompileJobBaseKeyRef Ref) {
  return static_cast<unsigned>(Ref);
}

static void appendEntry(SmallVectorImpl<char> &Out, EntryKind Kind,
                        StringRef Entry) {
  Out.push_back(static_cast<char>(Kind));
  Out.append(Entry.begin(), Entry.end());
  Out.push_back(0);
}

#ifndef NDEBUG
static void assertNoCASID(llvm::cas::ObjectStore &CAS,
                          const llvm::opt::Arg *A) {
  for (StringRef Value : A->getValues()) {
    auto ID = CAS.parseID(Value);
    if (!ID) {
      llvm::consumeError(ID.takeError());
      continue;
    }
    llvm::errs() << "CAS ID '" << Value << "' found in '" << A->getSpelling()
                 << "' that is not labeled as ArgumentIsCASID\n";
    assert(false && "unexpected CAS ID in command-line");
  }
}
#endif

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

  // With a file list, `-primary-file` only selects the primary inputs from
  // the file list and doesn't add new inputs.
  bool HasFileList = ParsedArgs.hasArg(options::OPT_filelist);

  SmallString<256> CommandLine;
  SmallString<256> ClangArgs;
  SmallString<256> JobArgs;
  SmallString<256> InputList;
  SmallVector<llvm::cas::ObjectRef> CommandLineRefs;
  SmallVector<llvm::cas::ObjectRef> JobRefs;
  std::optional<unsigned> InputListRefIndex;

  auto addFileList =
      [&](const llvm::opt::Arg *A, SmallVectorImpl<char> &Out,
          SmallVectorImpl<llvm::cas::ObjectRef> &Refs) -> llvm::Error {
    auto FileList = llvm::MemoryBuffer::getFile(A->getValue());
    if (!FileList)
      return llvm::errorCodeToError(FileList.getError());
    auto Ref = CAS.storeFromString(/*Refs*/ {}, (*FileList)->getBuffer());
    if (!Ref)
      return Ref.takeError();
    appendEntry(Out, EntryKind::FileList, A->getSpelling());
    Refs.push_back(*Ref);
    return llvm::Error::success();
  };

  // Resolve the CAS ID in the argument, which can also be in the form of
  // `<name>=<CAS ID>`. Return std::nullopt if the value is not a CAS ID.
  auto resolveCASID = [&](const llvm::opt::Arg *A, StringRef &Value)
      -> llvm::Expected<std::optional<llvm::cas::ObjectRef>> {
    if (A->getNumValues() != 1)
      return std::nullopt;
    Value = A->getValue();
    auto ID = CAS.parseID(Value);
    if (!ID) {
      llvm::consumeError(ID.takeError());
      Value = Value.split('=').second;
      if (Value.empty())
        return std::nullopt;
      ID = CAS.parseID(Value);
      if (!ID) {
        llvm::consumeError(ID.takeError());
        return std::nullopt;
      }
    }
    auto Ref = CAS.getReference(*ID);
    if (!Ref)
      return llvm::createStringError("CAS ID '" + Value + "' for '" +
                                     A->getSpelling() + "' is not found");
    return *Ref;
  };

  // Add the argument with a CAS ID as a reference. Return false if the value
  // is not a CAS ID.
  auto addCASID = [&](const llvm::opt::Arg *A) -> llvm::Expected<bool> {
    StringRef Value;
    auto Ref = resolveCASID(A, Value);
    if (!Ref)
      return Ref.takeError();
    if (!*Ref)
      return false;
    std::string Rendered = A->getAsString(ParsedArgs);
    assert(StringRef(Rendered).ends_with(Value) &&
           "CAS ID is not at the end of the argument");
    appendEntry(CommandLine, EntryKind::CASID,
                StringRef(Rendered).drop_back(Value.size()));
    CommandLineRefs.push_back(**Ref);
    return true;
  };

  // The clang include tree is stored at the fixed positions.
  std::optional<llvm::cas::ObjectRef> IncludeTreeRoot;
  std::optional<llvm::cas::ObjectRef> IncludeTreeFileList;

  for (auto *Arg : ParsedArgs) {
    const auto &Opt = Arg->getOption();

    // Skip the options that doesn't affect caching.
    if (Opt.hasFlag(options::CacheInvariant))
      continue;

    if (Opt.matches(options::OPT_Xcc)) {
      appendEntry(ClangArgs, EntryKind::Argument, Arg->getAsString(ParsedArgs));
      continue;
    }

    if (Opt.matches(options::OPT_primary_filelist)) {
      if (auto Err = addFileList(Arg, JobArgs, JobRefs))
        return std::move(Err);
      continue;
    }

    if (Opt.matches(options::OPT_primary_file)) {
      appendEntry(JobArgs, EntryKind::Argument, Arg->getAsString(ParsedArgs));
      if (HasFileList)
        continue;
    }

    // The primary files are also part of the inputs, in the order they appear
    // on the command-line, so all the jobs in the module share the same list.
    if (Opt.matches(options::OPT_INPUT) ||
        Opt.matches(options::OPT_primary_file)) {
      if (!InputListRefIndex) {
        appendEntry(CommandLine, EntryKind::InputList, "");
        InputListRefIndex = CommandLineRefs.size();
      }
      InputList.append(Arg->getValue());
      InputList.push_back(0);
      continue;
    }

    if (Opt.hasFlag(options::ArgumentIsFileList)) {
      if (auto Err = addFileList(Arg, CommandLine, CommandLineRefs))
        return std::move(Err);
      continue;
    }

    // Only the last include tree option is used by the compiler.
    if (Opt.matches(options::OPT_clang_include_tree_root) ||
        Opt.matches(options::OPT_clang_include_tree_filelist)) {
      StringRef Value;
      auto Ref = resolveCASID(Arg, Value);
      if (!Ref)
        return Ref.takeError();
      if (*Ref) {
        if (Opt.matches(options::OPT_clang_include_tree_root))
          IncludeTreeRoot = **Ref;
        else
          IncludeTreeFileList = **Ref;
        continue;
      }
    }

    if (Opt.hasFlag(options::ArgumentIsCASID)) {
      auto Added = addCASID(Arg);
      if (!Added)
        return Added.takeError();
      if (*Added)
        continue;
    } else {
#ifndef NDEBUG
      assertNoCASID(CAS, Arg);
#endif
    }

    appendEntry(CommandLine, EntryKind::Argument, Arg->getAsString(ParsedArgs));
  }

  if (InputListRefIndex) {
    auto Inputs = CAS.storeFromString(/*Refs*/ {}, InputList);
    if (!Inputs)
      return Inputs.takeError();
    CommandLineRefs.insert(CommandLineRefs.begin() + *InputListRefIndex,
                           *Inputs);
  }

  // FIXME: The version is maybe insufficient...
  auto Version =
      CAS.storeFromString(/*Refs*/ {}, version::getSwiftFullVersion());
  if (!Version)
    return Version.takeError();
  auto CMD = CAS.storeFromString(/*Refs*/ {}, CommandLine);
  if (!CMD)
    return CMD.takeError();
  auto Clang = CAS.storeFromString(/*Refs*/ {}, ClangArgs);
  if (!Clang)
    return Clang.takeError();

  // Use an empty blob for the include tree that is not used.
  auto Empty = CAS.storeFromString(/*Refs*/ {}, "");
  if (!Empty)
    return Empty.takeError();

  SmallVector<llvm::cas::ObjectRef> Refs = {
      *Version, *CMD, *Clang, IncludeTreeRoot.value_or(*Empty),
      IncludeTreeFileList.value_or(*Empty)};
  assert(Refs.size() == refIndex(CompileJobBaseKeyRef::NumFixedRefs));
  Refs.append(CommandLineRefs.begin(), CommandLineRefs.end());
  Refs.append(JobRefs.begin(), JobRefs.end());
  return CAS.storeFromString(Refs, JobArgs);
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

  auto Base = CAS.getProxy(Proxy->getReference(0));
  if (!Base)
    return Base.takeError();
  if (Base->getNumReferences() < refIndex(CompileJobBaseKeyRef::NumFixedRefs))
    return llvm::createStringError("incorrect child number for base key");

  auto load = [&](CompileJobBaseKeyRef Ref) {
    return CAS.getProxy(Base->getReference(refIndex(Ref)));
  };

  std::string BaseStr;
  llvm::raw_string_ostream BaseOS(BaseStr);
  unsigned NextRef = refIndex(CompileJobBaseKeyRef::NumFixedRefs);
  auto printEntries = [&](StringRef Name, StringRef Entries) -> llvm::Error {
    BaseOS.indent(2) << Name << "\n";
    StringRef Entry, Remain = Entries;
    while (!Remain.empty()) {
      std::tie(Entry, Remain) = Remain.split(0);
      if (Entry.empty())
        return llvm::createStringError("invalid entry in base key");
      auto Kind = static_cast<EntryKind>(Entry.front());
      Entry = Entry.drop_front();
      if (Kind == EntryKind::Argument) {
        BaseOS.indent(4) << Entry << "\n";
        continue;
      }
      if (NextRef >= Base->getNumReferences())
        return llvm::createStringError("missing reference in base key");
      auto Ref = CAS.getProxy(Base->getReference(NextRef++));
      if (!Ref)
        return Ref.takeError();
      if (Kind == EntryKind::CASID) {
        BaseOS.indent(4) << Entry << Ref->getID().toString() << "\n";
        continue;
      }
      // Print the content of the file list.
      BaseOS.indent(4) << (Kind == EntryKind::InputList ? "<inputs>" : Entry)
                       << " " << Ref->getID().toString() << "\n";
      StringRef Line, Lines = Ref->getData();
      char Separator = Kind == EntryKind::InputList ? 0 : '\n';
      while (!Lines.empty()) {
        std::tie(Line, Lines) = Lines.split(Separator);
        BaseOS.indent(6) << Line << "\n";
      }
    }
    return llvm::Error::success();
  };

  auto CommandLine = load(CompileJobBaseKeyRef::CommandLine);
  if (!CommandLine)
    return CommandLine.takeError();
  if (auto Err = printEntries("command-line", CommandLine->getData()))
    return Err;
  auto ClangArgs = load(CompileJobBaseKeyRef::ClangArguments);
  if (!ClangArgs)
    return ClangArgs.takeError();
  if (auto Err = printEntries("clang-arguments", ClangArgs->getData()))
    return Err;
  if (auto Err = printEntries("job-arguments", Base->getData()))
    return Err;

  BaseOS.indent(2) << "include-tree\n";
  std::pair<CompileJobBaseKeyRef, StringRef> IncludeTrees[] = {
      {CompileJobBaseKeyRef::IncludeTreeRoot, "-clang-include-tree-root"},
      {CompileJobBaseKeyRef::IncludeTreeFileList,
       "-clang-include-tree-filelist"}};
  for (auto &[Ref, Option] : IncludeTrees) {
    auto IncludeTree = load(Ref);
    if (!IncludeTree)
      return IncludeTree.takeError();
    if (IncludeTree->getNumReferences() || !IncludeTree->getData().empty())
      BaseOS.indent(4) << Option << " " << IncludeTree->getID().toString()
                       << "\n";
  }

  auto Version = load(CompileJobBaseKeyRef::Version);
  if (!Version)
    return Version.takeError();
  BaseOS.indent(2) << "version\n";
  BaseOS.indent(4) << Version->getData() << "\n";

  OS << "Cache Key " << CAS.getID(Key).toString() << "\n";
  OS << "Swift Compiler Invocation Info:\n";
  OS << BaseStr;
  OS << "Input index: " << InputIndex << "\n";

  return llvm::Error::success();
}
