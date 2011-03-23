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
#include "llvm/ADT/ArrayRef.h"
#include <cstddef>
#include <cassert>

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ASTContext;
  class Type;
  class Expr;
  class BraceExpr;
  class OneOfElementDecl;
  class NameAliasType;
  class TypeAliasDecl;
  
enum DeclKind {
  TranslationUnitDeclKind,
  ImportDeclKind,
  TypeAliasDeclKind,
  VarDeclKind,
  FuncDeclKind,
  OneOfElementDeclKind,
  ArgDeclKind,
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
  
  
  bool empty() const { return InfixPrecedence == -1; }
};

  
  
/// DeclVarName - This is a recursive structure that represents the varname
/// datatype which can be used to name various pieces of a var definition.  For
/// example:  var ((a, b), c) = foo();
///
class DeclVarName {
public:
  /// LPLoc/RPLoc - This is the location of the '(' and ')' if this is a complex
  /// name, or both contain the same location if this is simple.
  llvm::SMLoc LPLoc, RPLoc;
  
  // If this is a simple name like "a", this contains the identifier.
  Identifier Name;
  
  llvm::ArrayRef<DeclVarName*> Elements;
  
  DeclVarName() {}
  bool isSimple() const { return LPLoc == RPLoc; }

private:
  // Make placement new and vanilla new/delete illegal for DeclVarNames.
  void *operator new(size_t Bytes) throw();  // DO NOT IMPLEMENT.
  void operator delete(void *Data) throw();  // DO NOT IMPLEMENT.
  void *operator new(size_t Bytes, void *Mem) throw();  // DO NOT IMPLEMENT.
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
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
  
  llvm::SMLoc getLocStart() const;
  
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
  
/// TranslationUnitDecl - This contains information about all of the decls and
/// external references in a translation unit, which is one file.
class TranslationUnitDecl : public Decl {
public:
  ASTContext &Ctx;

  /// Body - This is a synthesized BraceExpr that holds the top level
  /// expressions and declarations for a translation unit.
  BraceExpr *Body;
  
  /// UnresolvedTypes - This is a list of types that were unresolved at the end
  /// of the translation unit's parse phase.
  llvm::ArrayRef<TypeAliasDecl*> UnresolvedTypesForParser;
  
  TranslationUnitDecl(ASTContext &C)
    : Decl(TranslationUnitDeclKind), Ctx(C) {
  }

  llvm::SMLoc getLocStart() const;
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == TranslationUnitDeclKind;
  }
  static bool classof(const TranslationUnitDecl *D) { return true; }
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import swift
///   import swift.int
class ImportDecl : public Decl {
public:
  llvm::SMLoc ImportLoc;
  llvm::ArrayRef<std::pair<Identifier,llvm::SMLoc> > AccessPath;
  
  ImportDecl(llvm::SMLoc importLoc,
             llvm::ArrayRef<std::pair<Identifier,llvm::SMLoc> > path)
    : Decl(ImportDeclKind), ImportLoc(importLoc), AccessPath(path) {
  }
  
  llvm::SMLoc getLocStart() const { return ImportLoc; }

  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == ImportDeclKind;
  }
  static bool classof(const ImportDecl *D) { return true; }
};

  
/// NamedDecl - The common base class between TypeAliasDecl and ValueDecl.
class NamedDecl : public Decl {
public:
  Identifier Name;
  DeclAttributes Attrs;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return (D->getKind() == VarDeclKind || D->getKind() == FuncDeclKind ||
            D->getKind() == OneOfElementDeclKind ||
            D->getKind() == ArgDeclKind || 
            D->getKind() == ElementRefDeclKind ||
            D->getKind() == TypeAliasDeclKind);
  }
  static bool classof(const NamedDecl *D) { return true; }
  
protected:
  NamedDecl(DeclKind K, Identifier name,
            const DeclAttributes &attrs = DeclAttributes())
    : Decl(K), Name(name), Attrs(attrs) {
  }
  
  void printCommon(llvm::raw_ostream &OS, unsigned Indent) const;
};
  
/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias foo : int
///
class TypeAliasDecl : public NamedDecl {
  /// The type that represents this (sugared) name alias.
  mutable NameAliasType *AliasTy;
public:
  llvm::SMLoc TypeAliasLoc;
  Type *UnderlyingTy;
  
  TypeAliasDecl(llvm::SMLoc typealiasloc, Identifier name, Type *underlyingty,
                const DeclAttributes &attrs = DeclAttributes())
    : NamedDecl(TypeAliasDeclKind, name, attrs),
      TypeAliasLoc(typealiasloc), UnderlyingTy(underlyingty) {
  }

  llvm::SMLoc getLocStart() const { return TypeAliasLoc; }

  /// getAliasType - Return the sugared version of this decl as a Type.
  NameAliasType *getAliasType(ASTContext &C) const;
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == TypeAliasDeclKind;
  }
  static bool classof(const TypeAliasDecl *D) { return true; }
};
  
/// ValueDecl - All named decls that are values in the language.  These can
/// have an initializer, type, etc.
class ValueDecl : public NamedDecl {
public:
  Type *Ty;
  Expr *Init;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return (D->getKind() == VarDeclKind || D->getKind() == FuncDeclKind ||
            D->getKind() == OneOfElementDeclKind ||D->getKind() == ArgDeclKind||
            D->getKind() == ElementRefDeclKind);
  }
  static bool classof(const ValueDecl *D) { return true; }

protected:
  ValueDecl(DeclKind K, Identifier name, Type *ty, Expr *init,
            const DeclAttributes &attrs = DeclAttributes())
    : NamedDecl(K, name, attrs), Ty(ty), Init(init) {
  }
  void printCommon(llvm::raw_ostream &OS, unsigned Indent) const;
};  

/// VarDecl - 'var' declaration.
class VarDecl : public ValueDecl {
public:
  llvm::SMLoc VarLoc;    // Location of the 'var' token.

  /// NestedName - If this is a simple var definition, the name is stored in the
  /// name identifier.  If the varname is complex, Name is empty and this
  /// contains the nested name specifier.
  DeclVarName *NestedName;
  
  VarDecl(llvm::SMLoc varloc, Identifier name, Type *ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(VarDeclKind, name, ty, init, attrs), VarLoc(varloc),
      NestedName(0) {}
  VarDecl(llvm::SMLoc varloc, DeclVarName *name, Type *ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(VarDeclKind, Identifier(), ty, init, attrs), VarLoc(varloc),
      NestedName(name) {}

  
  llvm::SMLoc getLocStart() const { return VarLoc; }
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == VarDeclKind; }
  static bool classof(const VarDecl *D) { return true; }

};
  

/// FuncDecl - 'func' declaration.
class FuncDecl : public ValueDecl {
public:
  llvm::SMLoc FuncLoc;    // Location of the 'func' token.

  FuncDecl(llvm::SMLoc funcloc, Identifier name, Type *ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(FuncDeclKind, name, ty, init, attrs), FuncLoc(funcloc) {}
  
  
  llvm::SMLoc getLocStart() const { return FuncLoc; }

  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == FuncDeclKind; }
  static bool classof(const FuncDecl *D) { return true; }
};

  
/// OneOfElementDecl - This represents an element of a 'oneof' declaration, e.g.
/// X and Y in:
///   oneof d { X : int, Y : int, Z }
/// The type of a OneOfElementDecl is always the OneOfType for the containing
/// oneof.
class OneOfElementDecl : public ValueDecl {
public:
  llvm::SMLoc IdentifierLoc;
  
  /// ArgumentType - This is the type specified with the oneof element.  For
  /// example 'int' in the Y example above.  This is null if there is no type
  /// associated with this element (such as in the Z example).
  Type *ArgumentType;
  
  
  OneOfElementDecl(llvm::SMLoc identloc, Identifier name, Type *ty,
                   Type *argtype)
  : ValueDecl(OneOfElementDeclKind, name, ty, 0),
    IdentifierLoc(identloc), ArgumentType(argtype) {}

  
  llvm::SMLoc getLocStart() const { return IdentifierLoc; }
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == OneOfElementDeclKind;
  }
  static bool classof(const OneOfElementDecl *D) { return true; }
 
};

  
/// ArgDecl - A declaration representing a named function argument, in a func
/// declaration.  For example, in "func x(a : int);", 'a' is an ArgDecl.
///
/// TODO: Should this be a special case of ElementRefDecl?
class ArgDecl : public ValueDecl {
public:
  // FIXME: We don't have good location information for the function argument
  // declaration.
  llvm::SMLoc FuncLoc;
  
  // FIXME: Store the access path here.
  
  ArgDecl(llvm::SMLoc funcloc, Identifier name, Type *ty)
    : ValueDecl(ArgDeclKind, name, ty, 0, DeclAttributes()), FuncLoc(funcloc) {}


  llvm::SMLoc getLocStart() const { return FuncLoc; }
 
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == ArgDeclKind; }
  static bool classof(const ArgDecl *D) { return true; }
};

/// ElementRefDecl - A reference to the element of another decl which is formed
/// through name binding.  For example, in "var (a,b) = f();" there is a VarDecl
/// with no name and two ElementRefDecls (named A and B) referring to elements
/// of the nameless vardecl.
class ElementRefDecl : public ValueDecl {
public:
  VarDecl *VD;
  llvm::SMLoc NameLoc;
  llvm::ArrayRef<unsigned> AccessPath;
  
  ElementRefDecl(VarDecl *vd, llvm::SMLoc nameloc, Identifier name,
                 llvm::ArrayRef<unsigned> path, Type *ty)
    : ValueDecl(ElementRefDeclKind, name, ty, 0), VD(vd), NameLoc(nameloc),
      AccessPath(path) {
  }

  /// getTypeForPath - Given a type and an access path into it, return the
  /// referenced element type.  If the access path is invalid for the specified
  /// type, this returns null.  If the query goes into an unresolved (dependent)
  /// part of the type, this returns TheDependentType.
  static Type *getTypeForPath(Type *Ty, llvm::ArrayRef<unsigned> Path);
  
  
  llvm::SMLoc getLocStart() const { return NameLoc; }
  
  void print(llvm::raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind()==ElementRefDeclKind;}
  static bool classof(const ElementRefDecl *D) { return true; }
};

  
} // end namespace swift

#endif
