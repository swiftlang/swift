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

#include "swift/Serialization/SerializedModuleLoader.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

bool parseASTSection(SerializedModuleLoader* SML, StringRef Buf,
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

  size_t size = Buf.size();
  const char *data = Buf.data();

  // An AST section consists of one or more AST modules + headers.
  // Iterate over all AST modules.
  while (size > sizeof(struct apple_ast_hdr)) {
    auto h = reinterpret_cast<const struct apple_ast_hdr *>(data);

    // Check for fatal errors first.
    if (h->magic != 0x41535473) {
      llvm::dbgs() << "Magic number not found.\n";
      return false;
    }

    if (h->version != 1) {
      llvm::dbgs() << "Unsupported __ast section version.\n";
      return false;
    }
    
    if (h->language != dwarf::DW_LANG_Swift)
      continue;

    // Get the access path.
    if (sizeof(h)+h->name_len > size) {
      llvm::dbgs() << "Impossible access path length. Section corrupted?\n";
      return false;
    }
    assert(h->name_len < (2 << 10) && "path failed sanity check");
    llvm::StringRef AccessPath(h->name, h->name_len);

    // loadModule() wants to take ownership of the input memory
    // buffer, but we don't let it own the memory, since it's just a
    // window into Buf.
    if (h->bitstream_ofs + h->bitstream_size > size) return false;
    auto mem = llvm::StringRef(data+h->bitstream_ofs, h->bitstream_size);
    auto bitstream = llvm::MemoryBuffer::getMemBuffer(mem, AccessPath, false);

    // Register the memory buffer.
    SML->registerMemoryBuffer(AccessPath,
                              std::unique_ptr<llvm::MemoryBuffer>(bitstream));
    foundModules.push_back(AccessPath);

    // Forward to the next module.
    uint64_t skip =
      llvm::RoundUpToAlignment(h->bitstream_ofs + h->bitstream_size, 32);

    if (skip > size) {
      llvm::dbgs() << "AST section too small.\n";
      return false;
    }

    data += skip;
    size -= skip;
  }
  return true;
}

}
