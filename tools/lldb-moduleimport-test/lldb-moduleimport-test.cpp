//===-- lldb-moduleimport-test.cpp - LLDB moduleimport tester -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Basic/Dwarf.h"
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
#include <fstream>
#include <sstream>

static llvm::cl::list<std::string>
InputNames(llvm::cl::Positional, llvm::cl::desc("compiled_swift_file1.o ..."),
           llvm::cl::OneOrMore);

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("path to the SDK to build against"));

static llvm::cl::opt<bool>
DumpModule("dump-module",
           llvm::cl::desc("Dump the imported module after checking it imports just fine"));

static llvm::cl::opt<std::string>
ModuleCachePath("module-cache-path", llvm::cl::desc("Clang module cache path"));

static llvm::cl::list<std::string>
ImportPaths("I", llvm::cl::desc("add a directory to the import search path"));

static llvm::cl::list<std::string>
FrameworkPaths("F", llvm::cl::desc("add a directory to the framework search path"));


void anchorForGetMainExecutable() {}

using namespace llvm::MachO;

static void printValidationInfo(llvm::StringRef data) {
  swift::SerializedModuleLoader::ExtendedValidationInfo extendedInfo;
  swift::SerializedModuleLoader::ValidationInfo info =
      swift::SerializedModuleLoader::validateSerializedAST(data,
                                                           &extendedInfo);
  if (info.status != swift::ModuleStatus::Valid)
    return;

  llvm::outs() << "- Target: " << info.targetTriple << "\n";
  if (!extendedInfo.getSDKPath().empty())
    llvm::outs() << "- SDK path: " << extendedInfo.getSDKPath() << "\n";
  if (!extendedInfo.getClangImporterOptions().empty()) {
    llvm::outs() << "- -Xcc options:";
    for (llvm::StringRef option : extendedInfo.getClangImporterOptions())
      llvm::outs() << " " << option;
    llvm::outs() << "\n";
  }
}

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram ST(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv);

  // If no SDK was specified via -sdk, check environment variable SDKROOT.
  if (SDK.getNumOccurrences() == 0) {
    const char *SDKROOT = getenv("SDKROOT");
    if (SDKROOT)
      SDK = SDKROOT;
  }

  // Create a Swift compiler.
  llvm::SmallVector<std::string, 4> modules;
  swift::CompilerInstance CI;
  swift::CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  Invocation.setSDKPath(SDK);
  Invocation.setTargetTriple(llvm::sys::getDefaultTargetTriple());
  Invocation.setModuleName("lldbtest");
  Invocation.getClangImporterOptions().ModuleCachePath = ModuleCachePath;
  Invocation.setImportSearchPaths(ImportPaths);
  Invocation.setFrameworkSearchPaths(FrameworkPaths);

  if (CI.setup(Invocation))
    return 1;

  std::vector<llvm::object::OwningBinary<llvm::object::ObjectFile>> ObjFiles;

  // Fetch the serialized module bitstreams from the Mach-O files and
  // register them with the module loader.
  for (std::string name : InputNames) {
    auto OF = llvm::object::ObjectFile::createObjectFile(name);
    if (!OF) {
      llvm::outs() << name << " is not an object file.\n";
      exit(1);
    }
    auto Obj = llvm::dyn_cast<llvm::object::MachOObjectFile>(OF->getBinary());
    if (!Obj) {
      llvm::outs() << name << " is not a Mach-O file.\n";
      exit(1);
    }

    for (auto &Symbol : Obj->symbols()) {
      auto RawSym = Symbol.getRawDataRefImpl();
      llvm::MachO::nlist nlist = Obj->getSymbolTableEntry(RawSym);
      if (nlist.n_type == N_AST) {
        llvm::StringRef Path;
        if (Obj->getSymbolName(RawSym, Path)) {
          llvm::outs() << "Cannot get symbol name\n;";
          exit(1);
        }

        auto fileBuf = llvm::MemoryBuffer::getFile(Path);
        if (!fileBuf) {
          llvm::outs() << "Cannot read from '" << Path << "': "
                       << fileBuf.getError().message();
          exit(1);
        }

        if (!parseASTSection(CI.getSerializedModuleLoader(),
                             fileBuf.get()->getBuffer(),
                             modules)) {
          exit(1);
        }

        for (auto path : modules)
          llvm::outs() << "Loaded module " << path << " from " << name
                       << "\n";
        printValidationInfo(fileBuf.get()->getBuffer());

        // Deliberately leak the llvm::MemoryBuffer. We can't delete it
        // while it's in use anyway.
        fileBuf.get().release();
      }
    }
    for (auto &Section : Obj->sections()) {
      llvm::StringRef Name;
      Section.getName(Name);
      if (Name == swift::MachOASTSectionName) {
        llvm::StringRef Buf;
        Section.getContents(Buf);
        if (!parseASTSection(CI.getSerializedModuleLoader(), Buf, modules))
          exit(1);

        for (auto path : modules)
          llvm::outs() << "Loaded module " << path << " from " << name
                       << "\n";
        printValidationInfo(Buf);
      }
    }
    ObjFiles.push_back(std::move(*OF));
  }

  // Attempt to import all modules we found.
  for (auto path : modules) {
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
      llvm::errs() << "FAIL!\n";
      return 1;
    }
    llvm::outs() << "ok!\n";
    if (DumpModule) {
      llvm::SmallVector<swift::Decl*, 10> Decls;
      Module->getTopLevelDecls(Decls);
      for (auto Decl : Decls) {
        Decl->dump(llvm::outs());
      }
    }
  }
  return 0;
}
