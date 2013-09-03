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

bool parseASTSection(SerializedModuleLoader* SML,
                     const std::unique_ptr<llvm::MemoryBuffer> &MemoryBuffer,
                     SmallVectorImpl<std::string> &foundModules) {
  struct apple_ast_hdr {
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

  if (MemoryBuffer->getBufferSize() < sizeof(struct apple_ast_hdr)
                                    + sizeof(struct module_header)) {
    llvm::dbgs() << "__apple_ast section is too small.\n";
    return false;
  }

  size_t size = MemoryBuffer->getBufferSize();
  const char *data = MemoryBuffer->getBufferStart();
  auto apple_ast_hdr = reinterpret_cast<const struct apple_ast_hdr *>(data);
  if (apple_ast_hdr->version != 1) {
    llvm::dbgs() << "Unsupported __apple_ast section version.\n";
    return false;
  }

  // Iterate over all AST modules.
  for (uint32_t i = 0; i < apple_ast_hdr->nmods; ++i) {
    auto mh = reinterpret_cast<const struct module_header *>
      (data+sizeof(apple_ast_hdr));

    if (mh->language != dwarf::DW_LANG_Swift)
      continue;

    // Get the access path.
    if (mh->name_ofs + 4 > size) return false;
    auto nchars = *reinterpret_cast<const uint32_t *>(data + mh->name_ofs);
    if (mh->name_ofs+sizeof(nchars) > size) return false;
    assert(nchars < (2 << 10) && "path failed sanity check");
    llvm::StringRef AccessPath(data+mh->name_ofs+sizeof(nchars), nchars);

    // loadModule() wants to take ownership of the input memory buffer.
    // Copy the bitstream into a new memory buffer.
    if (mh->bitstream_ofs + mh->bitstream_size > size) return false;
    auto mem = llvm::StringRef(data+mh->bitstream_ofs, mh->bitstream_size);
    auto bitstream = llvm::MemoryBuffer::getMemBufferCopy(mem, AccessPath);

    // Register the memory buffer.
    SML->registerMemoryBuffer(AccessPath,
                              std::unique_ptr<llvm::MemoryBuffer>(bitstream));
    foundModules.push_back(AccessPath);
  }
  return true;
}

}
