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

#include "swift/Frontend/Frontend.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include <mach-o/loader.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <unistd.h>

struct apple_ast_section {
  uint32_t version;
  uint32_t nmods;
};

struct module_header {
 uint64_t bitstream_ofs;
 uint64_t bitstream_size;
 uint64_t name_ofs;
 uint32_t language;
 uint32_t flags;
};


static llvm::cl::list<std::string>
InputNames(llvm::cl::Positional, llvm::cl::desc("compiled_swift_file1.o ..."),
             llvm::cl::OneOrMore);

#ifndef SWIFT_MODULES_SDK
#define SWIFT_MODULES_SDK ""
#endif

static llvm::cl::opt<std::string>
SDK("sdk", llvm::cl::desc("path to the SDK to build against"),
    llvm::cl::init(SWIFT_MODULES_SDK));

void anchorForGetMainExecutable() {}

int main(int argc, char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram ST(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv);

  // Create a Swift compiler.
  std::vector<std::string> modules;
  swift::CompilerInstance CI;
  swift::CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(
      llvm::sys::fs::getMainExecutable(argv[0],
          reinterpret_cast<void *>(&anchorForGetMainExecutable)));

  Invocation.setSDKPath(SDK);
  Invocation.setTargetTriple(llvm::sys::getDefaultTargetTriple());
  Invocation.setModuleName("lldbtest");

  if (CI.setup(Invocation))
    return 1;

  auto SML = swift::SerializedModuleLoader::create(CI.getASTContext());

  // Fetch the serialized module bitstreams from the Mach-O files and
  // register them with the module loader.
  for (std::string name : InputNames) {
    // We assume Macho-O 64 bit.
    std::ifstream macho(name);
    struct mach_header_64 h;
    macho.read((char*)&h, sizeof(h));
    assert(h.magic == MH_MAGIC_64);
    // Load command.
    for (uint32_t i = 0; i < h.ncmds; ++i) {
      struct load_command lc;
      macho.read((char*)&lc, sizeof(lc));
      // Segment command.
      if (lc.cmd == LC_SEGMENT_64) {
        macho.seekg(-sizeof(lc), macho.cur);
        struct segment_command_64 sc;
        macho.read((char*)&sc, sizeof(sc));
        // Sections.
        for (uint32_t j = 0; j < sc.nsects; ++j) {
          struct section_64 section;
          macho.read((char*)&section, sizeof(section));
          auto sectname = "__apple_ast";
          if (strncmp(section.sectname, sectname, strlen(sectname)) == 0) {
              macho.seekg(section.offset, macho.beg);
              size_t base = macho.tellg();
              struct apple_ast_section apple_ast_section;
              macho.read((char*)&apple_ast_section, sizeof(apple_ast_section));
              assert(apple_ast_section.version == 1);
              // AST Modules.
              for (uint32_t k = 0; k < apple_ast_section.nmods; ++k) {
                struct module_header mh;
                macho.read((char*)&mh, sizeof(mh));

                // Path.
                macho.seekg(base+mh.name_ofs, macho.beg);
                uint32_t nchars;
                macho.read((char*)&nchars, sizeof(nchars));
                assert(nchars < 2<<10);
                char path[nchars+1];
                macho.read(path, nchars);
                path[nchars] = 0;

                // Bitstream.
                macho.seekg(base+mh.bitstream_ofs, macho.beg);
                char* data = new char[mh.bitstream_size];
                macho.read(data, mh.bitstream_size);
                auto sr = llvm::StringRef(data, mh.bitstream_size);
                auto bitstream = (llvm::MemoryBuffer::getMemBuffer(sr, path));

                // Register it.
                std::cout<<"Loaded module "<<path<<" from "<<name<<"\n";
                SML->registerBitstream(path, llvm::OwningPtr<llvm::MemoryBuffer>
                                             (bitstream));
                modules.push_back(path);

                assert(macho.good());
              }
          }
        }
      } else macho.seekg(lc.cmdsize-sizeof(lc), macho.cur);
    }
  }

  // Attempt to import all modules we found.
  for (auto path : modules) {
    std::cout<<"Importing "<<path<<"... ";

#ifdef SWIFT_SUPPORTS_SUBMODULES
    std::vector<std::pair<swift::Identifier, swift::SourceLoc> > AccessPath;
    for (auto i = llvm::sys::path::begin(path);
             i != llvm::sys::path::end(path);  ++i)
      if (!llvm::sys::path::is_separator((*i)[0]))
          AccessPath.push_back({ CI.getASTContext().getIdentifier(*i),
                                 swift::SourceLoc() });
#else
    std::vector<std::pair<swift::Identifier, swift::SourceLoc> > AccessPath;
    AccessPath.push_back({ CI.getASTContext().getIdentifier(path),
                           swift::SourceLoc() });
#endif

    auto Module = SML->loadModule(swift::SourceLoc(), AccessPath);
    if (!Module) {
      std::cerr<<"FAIL!\n";
      return 1;
    }
    std::cout<<"ok!\n";
  }
  return 0;
}
