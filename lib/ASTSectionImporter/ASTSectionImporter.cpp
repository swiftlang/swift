//===--- ASTSectionImporter.cpp - Import AST Section Modules --------------===//
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
// This file implements support for loading modules serialized into a
// Mach-O AST section into Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/ASTSectionImporter/ASTSectionImporter.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static bool parseBareASTs(SerializedModuleLoader *SML, StringRef buf,
                          SmallVectorImpl<std::string> &foundModules) {
  while (!buf.empty()) {
    auto info = SerializedModuleLoader::validateSerializedAST(buf);

    assert(info.name.size() < (2 << 10) && "name failed sanity check");

    if (info.status == ModuleStatus::Valid) {
      assert(info.bytes != 0);
      if (!info.name.empty()) {
        StringRef moduleData = buf.substr(0, info.bytes);
        std::unique_ptr<llvm::MemoryBuffer> bitstream(
          llvm::MemoryBuffer::getMemBuffer(moduleData, info.name, false));

        // Register the memory buffer.
        SML->registerMemoryBuffer(info.name, std::move(bitstream));
        foundModules.push_back(info.name);
      }
    } else {
      llvm::dbgs() << "Unable to load module";
      if (!info.name.empty())
        llvm::dbgs() << '\'' << info.name << '\'';
      llvm::dbgs() << ".\n";
    }

    if (info.bytes == 0)
      return false;

    if (info.bytes > buf.size()) {
      llvm::dbgs() << "AST section too small.\n";
      return false;
    }

    buf = buf.substr(info.bytes);
  }

  return true;
}

static bool parseASTsWithHeaders(SerializedModuleLoader *SML, StringRef buf,
                                 SmallVectorImpl<std::string> &foundModules) {
  struct apple_ast_hdr {
    uint32_t magic;
    uint32_t version;
    uint32_t language;
    uint32_t flags;
    uint64_t bitstream_ofs;
    uint64_t bitstream_size;
    uint32_t name_len;
    char name[1]; // There's at least a NUL there.
  };

  while (buf.size() > sizeof(struct apple_ast_hdr)) {
    auto h = reinterpret_cast<const struct apple_ast_hdr *>(buf.data());

    // Check for fatal errors first.
    if (h->magic != 0x41535473) {
      llvm::dbgs() << "Magic number not found.\n";
      return false;
    }

    if (h->version != 1) {
      llvm::dbgs() << "Unsupported __ast section version.\n";
      return false;
    }

    if (h->language == dwarf::DW_LANG_Swift) {
      // Get the access path.
      if (sizeof(struct apple_ast_hdr)+h->name_len > buf.size()) {
        llvm::dbgs() << "Impossible access path length. Section corrupted?\n";
        return false;
      }
      assert(h->name_len < (2 << 10) && "path failed sanity check");
      llvm::StringRef AccessPath(h->name, h->name_len);

      if (h->bitstream_ofs + h->bitstream_size > buf.size())
        return false;

      // loadModule() wants to take ownership of the input memory
      // buffer, but we don't let it own the memory, since it's just a
      // window into the buffer.
      auto mem = buf.substr(h->bitstream_ofs, h->bitstream_size);
      std::unique_ptr<llvm::MemoryBuffer> bitstream(
         llvm::MemoryBuffer::getMemBuffer(mem, AccessPath, false));

      // Register the memory buffer.
      SML->registerMemoryBuffer(AccessPath, std::move(bitstream));
      foundModules.push_back(AccessPath);
    }

    uint64_t skip =
      llvm::RoundUpToAlignment(h->bitstream_ofs + h->bitstream_size, 32);

    if (skip > buf.size()) {
      llvm::dbgs() << "AST section too small.\n";
      return false;
    }

    buf = buf.substr(skip);
  }

  return true;
}

bool swift::parseASTSection(SerializedModuleLoader *SML, StringRef buf,
                            SmallVectorImpl<std::string> &foundModules) {

  // An AST section consists of one or more AST modules, optionally with
  // headers. Iterate over all AST modules.
  // FIXME: Drop header support once we've switched over entirely to bare data.
  if (SerializedModuleLoader::isSerializedAST(buf))
    return parseBareASTs(SML, buf, foundModules);
  else
    return parseASTsWithHeaders(SML, buf, foundModules);
}
