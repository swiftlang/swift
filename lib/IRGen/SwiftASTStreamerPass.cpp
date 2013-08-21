//===--- SwiftASTStreamerPass.cpp - DWARF SwiftAST Streamer ---------------===//
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
// This file implements the SwiftASTStreamerPass class.
//
//===----------------------------------------------------------------------===//

#include "SwiftASTStreamerPass.h"
#include "IRGenDebugInfo.h"
#include "swift/Subsystems.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/MC/MCStreamer.h"

namespace swift {
namespace irgen {

char SwiftASTStreamerPass::ID = 0;

bool SwiftASTStreamerPass::runOnModule(llvm::Module &M) {
  if (!DebugInfo)
    return false;

  // FIXME (LLVM-Swift-branch). This shouldn't be hardcoded for Mach-O.
  auto SwiftASTSection = Context.
    getMachOSection("__DWARF", "__apple_swiftast",
                    llvm::MCSectionMachO::S_ATTR_DEBUG,
                    llvm::SectionKind::getMetadata());

  AsmStreamer.SwitchSection(SwiftASTSection);
  std::string Buf;
  llvm::raw_string_ostream out(Buf);
  serializeToStream(TU, out);

  StringRef ModuleID = DebugInfo->getMainFilename();
  AsmStreamer.EmitIntValue(ModuleID.size(), 8);
  AsmStreamer.EmitBytes(ModuleID);

  // FIXME: There is a potential problem with using
  // MCStreamer::EmitBytes() for this, because (in the assembler
  // output) it will result in a .asciz directive, but the data
  // stream _will_ contain NUL characters.
  AsmStreamer.EmitIntValue(Buf.size(), 8);
  AsmStreamer.EmitBytes(Buf);
  return false;
}

} // namespace irgen
} // namespace swift
