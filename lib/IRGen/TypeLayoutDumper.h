//===--- TypeLayoutDumper.h - Tool to dump fixed type layouts ---*- C++ -*-===//
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
//  A tool to dump fixed-size type layouts in YAML format.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPE_LAYOUT_DUMPER_H
#define SWIFT_IRGEN_TYPE_LAYOUT_DUMPER_H

#include "llvm/ADT/ArrayRef.h"

namespace llvm {
class raw_ostream;
}  // namespace llvm

namespace swift {

class ModuleDecl;

namespace irgen {

class IRGenModule;

class TypeLayoutDumper {
  IRGenModule &IGM;

public:
  explicit TypeLayoutDumper(IRGenModule &IGM) : IGM(IGM) {}

  void write(llvm::ArrayRef<ModuleDecl *> AllModules, llvm::raw_ostream &os) const;
};

}  // namespace irgen
}  // namespace swift

#endif
