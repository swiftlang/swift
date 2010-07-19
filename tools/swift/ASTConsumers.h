//===--- ASTConsumers.h - Various ASTConsumers ------------------*- C++ -*-===//
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
// This file defines a few ASTConsumer subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ASTCONSUMERS_H
#define SWIFT_ASTCONSUMERS_H

#include "swift/AST/ASTConsumer.h"

namespace swift {

class ASTDumper : public ASTConsumer {
public:
  ASTDumper(ASTContext &Ctx) : ASTConsumer(Ctx) {}
  
  /// HandleTopLevelDecl - This is called for every top-level declaration.
  virtual void HandleTopLevelDecl(Decl *D);

};

} // end namespace swift

#endif
