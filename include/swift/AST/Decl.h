//===--- Decl.h - Swift Language Declaration ASTs ---------------*- C++ -*-===//
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
// This file defines the Decl class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECL_H
#define SWIFT_DECL_H

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class StringRef;
  class raw_ostream;
}

namespace swift {
  class ASTContext;
  class Type;
  class Expr;
  
enum DeclKind {
  VarDeclKind
  //FuncDeclKind
};

/// Decl - Base class for all declarations in Swift.
class Decl {
  Decl(const Decl&);                 // DO NOT IMPLEMENT
  void operator=(const Decl&);       // DO NOT IMPLEMENT
  DeclKind Kind;
protected:
  Decl(DeclKind kind) : Kind(kind) {}
public:
  DeclKind getKind() const { return Kind; }
  
  
  void dump() const;
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *) { return true; }
  
private:
  // Make placement new and vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) throw();  // DO NOT IMPLEMENT.
  void operator delete(void *Data) throw();  // DO NOT IMPLEMENT.
  void *operator new(size_t Bytes, void *Mem) throw();  // DO NOT IMPLEMENT.
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};

/// VarDecl - 'var' declaration.
class VarDecl : public Decl {
  friend class ASTContext;
public:
  llvm::SMLoc VarLoc;    // Location of the 'var' token.
  llvm::StringRef Name;
  Type *Ty;
  Expr *Init;

  VarDecl(llvm::SMLoc varloc, llvm::StringRef name, Type *ty, Expr *init)
    : Decl(VarDeclKind), VarLoc(varloc), Name(name), Ty(ty), Init(init) {}

  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == VarDeclKind; }
  static bool classof(const VarDecl *D) { return true; }

};
  
} // end namespace swift

#endif
