//===--- lldb-moduleimport-test.cpp - LLDB moduleimport tester ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This program simulates LLDB importing modules from the __apple_ast
// section in Mach-O files. We use it to test for regressions in the
// deserialization API.
//
//===----------------------------------------------------------------------===//

#include "swift/ASTSectionImporter/ASTSectionImporter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/Utils.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/Validation.h"
#include "swift/Basic/Dwarf.h"
#include "llvm/Object/ELFObjectFile.h"
#include "swift/Basic/LLVMInitialize.h"
#include "llvm/Object/MachO.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ManagedStatic.h"
#include <fstream>
#include <sstream>

void anchorForGetMainExecutable() {}

using namespace llvm::MachO;

static bool
validateModule(llvm::StringRef data, bool Verbose,
               swift::serialization::ValidationInfo &info,
               swift::serialization::ExtendedValidationInfo &extendedInfo) {
  info = swift::serialization::validateSerializedAST(data, &extendedInfo);
  if (info.status != swift::serialization::Status::Valid)
    return false;

  swift::CompilerInvocation CI;
  if (CI.loadFromSerializedAST(data) != swift::serialization::Status::Valid)
    return false;

  if (Verbose) {
    if (!info.shortVersion.empty())
      llvm::outs() << "- Swift Version: " << info.shortVersion << "\n";
    llvm::outs() << "- Compatibility Version: "
                 << CI.getLangOptions()
                        .EffectiveLanguageVersion.asAPINotesVersionString()
                 << "\n";
    llvm::outs() << "- Target: " << info.targetTriple << "\n";
    if (!extendedInfo.getSDKPath().empty())
      llvm::outs() << "- SDK path: " << extendedInfo.getSDKPath() << "\n";
    if (!extendedInfo.getExtraClangImporterOptions().empty()) {
      llvm::outs() << "- -Xcc options:";
      for (llvm::StringRef option : extendedInfo.getExtraClangImporterOptions())
        llvm::outs() << " " << option;
      llvm::outs() << "\n";
    }
  }

  return true;
}

static void resolveDeclFromMangledNameList(
    swift::ASTContext &Ctx, llvm::ArrayRef<std::string> MangledNames) {
  std::string Error;
  for (auto &Mangled : MangledNames) {
    swift::Decl *ResolvedDecl =
        swift::ide::getDeclFromMangledSymbolName(Ctx, Mangled, Error);
    if (!ResolvedDecl) {
      llvm::errs() << "Can't resolve decl of " << Mangled << "\n";
    } else {
      ResolvedDecl->print(llvm::errs());
      llvm::errs() << "\n";
    }
  }
}

static void resolveTypeFromMangledNameList(
    swift::ASTContext &Ctx, llvm::ArrayRef<std::string> MangledNames) {
  std::string Error;
  for (auto &Mangled : MangledNames) {
    swift::Type ResolvedType =
        swift::ide::getTypeFromMangledSymbolname(Ctx, Mangled, Error);
    if (!ResolvedType) {
      llvm::errs() << "Can't resolve type of " << Mangled << "\n";
    } else {
      ResolvedType->print(llvm::errs());
      llvm::errs() << "\n";
    }
  }
}

static void
collectMangledNames(const std::string &FilePath,
                    llvm::SmallVectorImpl<std::string> &MangledNames) {
  std::string Name;
  std::ifstream InputStream(FilePath);
  while (std::getline(InputStream, Name)) {
    if (Name.empty())
      continue;
    MangledNames.push_back(Name);
  }
}

llvm::BumpPtrAllocator Alloc;

static bool
collectASTModules(llvm::cl::list<std::string> &InputNames,
                  llvm::SmallVectorImpl<std::pair<char *, uint64_t>> &Modules) {
  for (auto &name : InputNames) {
    auto OF = llvm::object::ObjectFile::createObjectFile(name);
    if (!OF) {
      llvm::outs() << "error: " << name << " "
                   << errorToErrorCode(OF.takeError()).message() << "\n";
      return false;
    }
    auto *Obj = OF->getBinary();
    auto *MachO = llvm::dyn_cast<llvm::object::MachOObjectFile>(Obj);
    auto *ELF = llvm::dyn_cast<llvm::object::ELFObjectFileBase>(Obj);

    if (MachO) {
      for (auto &Symbol : Obj->symbols()) {
        auto RawSym = Symbol.getRawDataRefImpl();
        llvm::MachO::nlist nlist = MachO->getSymbolTableEntry(RawSym);
        if (nlist.n_type != N_AST)
          continue;
        auto Path = MachO->getSymbolName(RawSym);
        if (!Path) {
          llvm::outs() << "Cannot get symbol name\n;";
          return false;
        }

        auto fileBuf = llvm::MemoryBuffer::getFile(*Path);
        if (!fileBuf) {
          llvm::outs() << "Cannot read from '" << *Path
                       << "': " << fileBuf.getError().message();
          return false;
        }

        uint64_t Size = fileBuf.get()->getBufferSize();
        char *Module = Alloc.Allocate<char>(Size);
        std::memcpy(Module, (void *)fileBuf.get()->getBufferStart(), Size);
        Modules.push_back({Module, Size});
      }
    }

    for (auto &Section : Obj->sections()) {
      llvm::StringRef Name;
      Section.getName(Name);
      if ((MachO && Name == swift::MachOASTSectionName) ||
          (ELF && Name == swift::ELFASTSectionName)) {
        uint64_t Size = Section.getSize();
        StringRef ContentsReference;
        Section.getContents(ContentsReference);
        char *Module = Alloc.Allocate<char>(Size);
        std::memcpy(Module, (void *)ContentsReference.begin(), Size);
        Modules.push_back({Module, Size});
      }
    }
  }
  return true;
}

int main(int argc, char **argv) {
  PROGRAM_START(argc, argv);
  INITIALIZE_LLVM();

  // Command line handling.
  llvm::cl::list<std::string> InputNames(
    llvm::cl::Positional, llvm::cl::desc("compiled_swift_file1.o ..."),
    llvm::cl::OneOrMore);

  llvm::cl::opt<bool> DumpModule(
    "dump-module", llvm::cl::desc(
      "Dump the imported module after checking it imports just fine"));

  llvm::cl::opt<bool> Verbose(
      "verbose", llvm::cl::desc("Dump informations on the loaded module"));

  llvm::cl::opt<std::string> ModuleCachePath(
    "module-cache-path", llvm::cl::desc("Clang module cache path"));

  llvm::cl::opt<std::string> DumpDeclFromMangled(
      "decl-from-mangled", llvm::cl::desc("dump decl from mangled names list"));

  llvm::cl::opt<std::string> DumpTypeFromMangled(
      "type-from-mangled", llvm::cl::desc("dump type from mangled names list"));

  llvm::cl::opt<std::string> ResourceDir("resource-dir",
      llvm::cl::desc("The directory that holds the compiler resource files"));

  llvm::cl::ParseCommandLineOptions(argc, argv);
  // Unregister our options so they don't interfere with the command line
  // parsing in CodeGen/BackendUtil.cpp.
  ModuleCachePath.removeArgument();
  DumpModule.removeArgument();
  DumpTypeFromMangled.removeArgument();
  InputNames.removeArgument();

  auto validateInputFile = [](std::string Filename) {
    if (Filename.empty())
      return true;
    if (!llvm::sys::fs::exists(llvm::Twine(Filename))) {
      llvm::errs() << Filename << " does not exists, exiting.\n";
      return false;
    }
    if (!llvm::sys::fs::is_regular_file(llvm::Twine(Filename))) {
      llvm::errs() << Filename << " is not a regular file, exiting.\n";
      return false;
    }
    return true;
  };

  if (!validateInputFile(DumpTypeFromMangled))
    return 1;
  if (!validateInputFile(DumpDeclFromMangled))
    return 1;

  // Fetch the serialized module bitstreams from the Mach-O files and
  // register them with the module loader.
  llvm::SmallVector<std::pair<char *, uint64_t>, 8> Modules;
  if (!collectASTModules(InputNames, Modules))
    return 1;

  if (Modules.empty())
    return 0;

  swift::serialization::ValidationInfo info;
  swift::serialization::ExtendedValidationInfo extendedInfo;
  for (auto &Module : Modules) {
    if (!validateModule(StringRef(Module.first, Module.second), Verbose, info,
                        extendedInfo)) {
      llvm::errs() << "Malformed module!\n";
      return 1;
    }
  }

  // Create a Swift compiler.
  llvm::SmallVector<std::string, 4> modules;
  swift::CompilerInstance CI;
  swift::CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  // Infer SDK and Target triple from the module.
  Invocation.setSDKPath(extendedInfo.getSDKPath());
  Invocation.setTargetTriple(info.targetTriple);

  Invocation.setModuleName("lldbtest");
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;

  if (!ResourceDir.empty()) {
    Invocation.setRuntimeResourcePath(ResourceDir);
  }

  if (CI.setup(Invocation))
    return 1;

  for (auto &Module : Modules)
    if (!parseASTSection(CI.getSerializedModuleLoader(),
                         StringRef(Module.first, Module.second), modules))
      return 1;

  // Attempt to import all modules we found.
  for (auto path : modules) {
    if (Verbose)
      llvm::outs() << "Importing " << path << "... ";

#ifdef SWIFT_SUPPORTS_SUBMODULES
    std::vector<std::pair<swift::Identifier, swift::SourceLoc> > AccessPath;
    for (auto i = llvm::sys::path::begin(path);
         i != llvm::sys::path::end(path); ++i)
      if (!llvm::sys::path::is_separator((*i)[0]))
          AccessPath.push_back({ CI.getASTContext().getIdentifier(*i),
                                 swift::SourceLoc() });
#else
    std::vector<std::pair<swift::Identifier, swift::SourceLoc> > AccessPath;
    AccessPath.push_back({ CI.getASTContext().getIdentifier(path),
                           swift::SourceLoc() });
#endif

    auto Module = CI.getASTContext().getModule(AccessPath);
    if (!Module) {
      if (Verbose)
        llvm::errs() << "FAIL!\n";
      return 1;
    }
    if (Verbose)
      llvm::outs() << "ok!\n";
    if (DumpModule) {
      llvm::SmallVector<swift::Decl*, 10> Decls;
      Module->getTopLevelDecls(Decls);
      for (auto Decl : Decls) {
        Decl->dump(llvm::outs());
      }
    }
    if (!DumpTypeFromMangled.empty()) {
      llvm::SmallVector<std::string, 8> MangledNames;
      collectMangledNames(DumpTypeFromMangled, MangledNames);
      resolveTypeFromMangledNameList(CI.getASTContext(), MangledNames);
    }
    if (!DumpDeclFromMangled.empty()) {
      llvm::SmallVector<std::string, 8> MangledNames;
      collectMangledNames(DumpDeclFromMangled, MangledNames);
      resolveDeclFromMangledNameList(CI.getASTContext(), MangledNames);
    }
  }
  return 0;
}
