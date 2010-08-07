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

#include "swift/AST/Identifier.h"
#include "llvm/Support/SMLoc.h"
#include <cstddef>

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ASTContext;
  class Type;
  class Expr;
  
enum DeclKind {
  VarDeclKind,
  FuncDeclKind,
  ArgDeclKind,
  AnonDeclKind,
  ElementRefDeclKind
};
  
/// DeclAttributes - These are attributes that may be applied to declarations.
class DeclAttributes {
public:
  /// LSquareLoc/RSquareLoc - This is the location of the '[' and ']' in the
  /// attribute specifier.  If this is an empty attribute specifier, then these
  /// will be invalid locs.
  llvm::SMLoc LSquareLoc, RSquareLoc;
  
  /// InfixPrecedence - If this is not negative, it indicates that the decl is
  /// an infix operator with the specified precedence.  Otherwise, it is in the
  /// range of 0-255.
  short InfixPrecedence;

  DeclAttributes() : InfixPrecedence(-1) { }
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

/// NamedDecl - The common base class between VarDecl and FuncDecl. 
class NamedDecl : public Decl {
public:
  Identifier Name;
  Type *Ty;
  Expr *Init;
  DeclAttributes Attrs;
  
  llvm::SMLoc getLocStart() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return (D->getKind() == VarDeclKind || D->getKind() == FuncDeclKind ||
            D->getKind() == AnonDeclKind || D->getKind() == ElementRefDeclKind);
  }
  static bool classof(const NamedDecl *D) { return true; }
  
protected:
  NamedDecl(Identifier name, Type *ty, Expr *init, const DeclAttributes &attrs,
            DeclKind K)
    : Decl(K), Name(name), Ty(ty), Init(init), Attrs(attrs) {
  }
  NamedDecl(Identifier name, Type *ty, Expr *init, DeclKind K)
    : Decl(K), Name(name), Ty(ty), Init(init) {
  }
  
  void printCommon(llvm::raw_ostream &OS, unsigned Indent) const;
};

/// VarDecl - 'var' declaration.
class VarDecl : public NamedDecl {
public:
  llvm::SMLoc VarLoc;    // Location of the 'var' token.

  VarDecl(llvm::SMLoc varloc, Identifier name, Type *ty, Expr *init,
          const DeclAttributes &attrs)
    : NamedDecl(name, ty, init, attrs, VarDeclKind), VarLoc(varloc) {}

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == VarDeclKind; }
  static bool classof(const VarDecl *D) { return true; }

};
  

/// FuncDecl - 'func' declaration.
class FuncDecl : public NamedDecl {
public:
  llvm::SMLoc FuncLoc;    // Location of the 'func' token.

  FuncDecl(llvm::SMLoc funcloc, Identifier name, Type *ty, Expr *init,
          const DeclAttributes &attrs)
    : NamedDecl(name, ty, init, attrs, VarDeclKind), FuncLoc(funcloc) {}
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == FuncDeclKind; }
  static bool classof(const FuncDecl *D) { return true; }
};
  
  
/// ArgDecl - A declaration representing a named function argument, in a func
/// declaration.  For example, in "func x(a : int);", 'a' is an ArgDecl.
///
class ArgDecl : public NamedDecl {
public:
  // FIXME: We don't have good location information for the function argument
  // declaration.
  llvm::SMLoc FuncLoc;
  
  // FIXME: Store the access path here.
  
  ArgDecl(llvm::SMLoc funcloc, Identifier name, Type *ty)
    : NamedDecl(name, ty, 0, DeclAttributes(), ArgDeclKind), FuncLoc(funcloc) {}

  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == ArgDeclKind; }
  static bool classof(const ArgDecl *D) { return true; }
};

/// AnonDecl - Anonymous closure argument declaration, synthesized by referecing
/// symbols _0 ... _9.
class AnonDecl : public NamedDecl {
public:
  llvm::SMLoc UseLoc;    // Location of the first use in a context.
  
  AnonDecl(llvm::SMLoc useloc, Identifier name, Type *ty)
  : NamedDecl(name, ty, 0, DeclAttributes(), AnonDeclKind), UseLoc(useloc) {}
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == AnonDeclKind; }
  static bool classof(const AnonDecl *D) { return true; }
};
  
/// ElementRefDecl - A reference to the element of another decl which is formed
/// through name binding.  For example, in "var (a,b) = f();" there is a VarDecl
/// with no name and two ElementRefDecls (named A and B) referring to elements
/// of the nameless vardecl.
class ElementRefDecl : public NamedDecl {
public:
  VarDecl *VD;
  llvm::SMLoc NameLoc;
  // TODO: Access path.
  
  ElementRefDecl(VarDecl *vd, llvm::SMLoc nameloc, Identifier name, Type *ty)
    : NamedDecl(name, ty, 0, ElementRefDeclKind), VD(vd), NameLoc(nameloc) {
  }

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind()==ElementRefDeclKind;}
  static bool classof(const ElementRefDecl *D) { return true; }
};

  
} // end namespace swift

#endif
