//===--- ASTMutationListener.h - AST Mutation Listener ----------*- C++ -*-===//
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
// This file defines the ASTMutationListener interface.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_ASTMUTATIONLISTENER_H
#define SWIFT_AST_ASTMUTATIONLISTENER_H

namespace swift {
  class Decl;

  /// \brief Abstract interface for listeners that can respond to AST
  /// mutation events, such as the introduction of a new declaration.
  class ASTMutationListener {
  public:
    virtual ~ASTMutationListener();

    /// \brief A new declaration was added to the AST.
    virtual void addedExternalDecl(Decl *decl) = 0;

    /// \brief A new type was added to the AST.
    virtual void addedExternalType(Type type) = 0;
  };
}

#endif // LLVM_SWIFT_AST_ASTMUTATIONLISTENER_H

