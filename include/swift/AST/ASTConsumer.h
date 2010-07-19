//===--- ASTConsumer.h - Consumer for ASTs ----------------------*- C++ -*-===//
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
// This file defines the ASTConsumer class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTCONSUMER_H
#define SWIFT_AST_ASTCONSUMER_H

namespace swift {
  class Decl;
  class ASTContext;
  
/// ASTConsumer - This is an abstract interface that should be implemented by
/// clients that read ASTs.  This abstraction layer allows the client to be
/// independent of the AST producer (e.g. parser vs AST dump file reader, etc).
class ASTConsumer {
  ASTContext &Context;
  
  ASTConsumer(const ASTConsumer&);          // DO NOT IMPLEMENT
  void operator=(const ASTConsumer&);       // DO NOT IMPLEMENT
public:
  ASTConsumer(ASTContext &Ctx) : Context(Ctx) {}
  virtual ~ASTConsumer();
  
  ASTContext &getContext() const { return Context; }
  
  virtual void HandleTopLevelDecl(Decl *D) {}
};  
  
} // end namespace swift

#endif
