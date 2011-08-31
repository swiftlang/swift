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

#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include <cstddef>

namespace swift {
  class ASTContext;
  class Type;
  class Expr;
  class BraceStmt;
  class OneOfElementDecl;
  class NameAliasType;
  class TypeAliasDecl;
  
enum class DeclKind {
  TranslationUnit,
  Import,
  TypeAlias,
  Var,
  Func,
  OneOfElement,
  Arg,
  ElementRef
};
  
/// DeclAttributes - These are attributes that may be applied to declarations.
class DeclAttributes {
public:
  /// LSquareLoc/RSquareLoc - This is the location of the '[' and ']' in the
  /// attribute specifier.  If this is an empty attribute specifier, then these
  /// will be invalid locs.
  SMLoc LSquareLoc, RSquareLoc;
  
  /// InfixPrecedence - If this is not negative, it indicates that the decl is
  /// an infix operator with the specified precedence.  Otherwise, it is in the
  /// range of 0-255.
  short InfixPrecedence;
  
  DeclAttributes() : InfixPrecedence(-1) { }
  
  
  bool empty() const {
    return InfixPrecedence == -1;
  }
  
  /// isInfix - Return true if this is a binary infix operation.
  bool isInfix() const { return InfixPrecedence != -1; }
};

  
  
/// DeclVarName - This is a recursive structure that represents the varname
/// datatype which can be used to name various pieces of a var definition.  For
/// example:  var ((a, b), c) = foo();
///
class DeclVarName {
  /// LPLoc/RPLoc - This is the location of the '(' and ')' if this is a complex
  /// name, or both contain the same location if this is simple.
  SMLoc LPLoc, RPLoc;
  
  union {
    void *Name; //< Storage for a simple variable name
    struct {
      DeclVarName * const *Start;
      unsigned Length;
    } Elements; //< Storage for a parenthesized list of variable names
  };
  
public:
  DeclVarName() {}
  
  DeclVarName(Identifier Name, SMLoc NameLoc)
    : LPLoc(NameLoc), RPLoc(NameLoc), Name(Name.getAsOpaquePointer()) { }
  
  DeclVarName(SMLoc LPLoc, ArrayRef<DeclVarName *> Elements, SMLoc RPLoc)
    : LPLoc(LPLoc), RPLoc(RPLoc) 
  {
    this->Elements.Start = Elements.data();
    this->Elements.Length = Elements.size();
  }
  
  SMLoc getLocation() const { return LPLoc; }
  
  Identifier getIdentifier() const {
    assert(isSimple() && 
           "Cannot retrieve an identifier for a non-simple name");
    return Identifier::getFromOpaquePointer(Name);
  }
  
  ArrayRef<DeclVarName *> getElements() const {
    assert(!isSimple() && "Cannot retrieve elements for a simple name");
    return ArrayRef<DeclVarName *>(Elements.Start, Elements.Length);
  }
   
  bool isSimple() const { return LPLoc == RPLoc; }

  // FIXME: We need an SMRange type!
  std::pair<SMLoc, SMLoc> getSourceRange() {
    return std::make_pair(LPLoc, RPLoc);
  }
  
private:
  // Make placement new and vanilla new/delete illegal for DeclVarNames.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};


/// Decl - Base class for all declarations in Swift.
class Decl {
  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;
protected:
  Decl(DeclKind kind, DeclContext *DC) : Kind(kind), Context(DC) {}
public:
  const DeclKind Kind;
  DeclContext *Context;
  
  SMLoc getLocStart() const;
 
  void dump() const;
  void print(raw_ostream &OS, unsigned Indent = 0) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *) { return true; }
  
private:
  // Make placement new and vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};
  
/// TranslationUnitDecl - This contains information about all of the decls and
/// external references in a translation unit, which is one file.
class TranslationUnitDecl : public Decl, public DeclContext {
public:
  ASTContext &Ctx;

  /// Body - This is a synthesized BraceStmt that holds the top level
  /// expressions and declarations for a translation unit.
  BraceStmt *Body;
  
  /// UnresolvedTypes - This is a list of types that were unresolved at the end
  /// of the translation unit's parse phase.
  ArrayRef<TypeAliasDecl*> UnresolvedTypesForParser;
  
  TranslationUnitDecl(ASTContext &C)
    : Decl(DeclKind::TranslationUnit, nullptr),
      DeclContext(DeclContextKind::TranslationUnitDecl, nullptr),
      Ctx(C) {
  }

  SMLoc getLocStart() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::TranslationUnit;
  }
  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::TranslationUnitDecl;
  }
  static bool classof(const TranslationUnitDecl *D) { return true; }
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import swift
///   import swift.int
class ImportDecl : public Decl {
public:
  SMLoc ImportLoc;
  ArrayRef<std::pair<Identifier,SMLoc>> AccessPath;
  
  ImportDecl(DeclContext *DC, SMLoc importLoc,
             ArrayRef<std::pair<Identifier,SMLoc>> path)
    : Decl(DeclKind::Import, DC), ImportLoc(importLoc), AccessPath(path) {
  }
  
  SMLoc getLocStart() const { return ImportLoc; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::Import;
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
    return (D->Kind == DeclKind::Var || D->Kind == DeclKind::Func ||
            D->Kind == DeclKind::OneOfElement ||
            D->Kind == DeclKind::Arg || 
            D->Kind == DeclKind::ElementRef ||
            D->Kind == DeclKind::TypeAlias);
  }
  static bool classof(const NamedDecl *D) { return true; }
  
protected:
  NamedDecl(DeclKind K, DeclContext *DC, Identifier name,
            const DeclAttributes &attrs = DeclAttributes())
    : Decl(K, DC), Name(name), Attrs(attrs) {
  }
};
  
/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias foo : int
///
class TypeAliasDecl : public NamedDecl {
  /// The type that represents this (sugared) name alias.
  mutable NameAliasType *AliasTy;
public:
  SMLoc TypeAliasLoc;
  Type UnderlyingTy;
  
  TypeAliasDecl(DeclContext *DC, SMLoc typealiasloc, Identifier name,
                Type underlyingty,
                const DeclAttributes &attrs = DeclAttributes())
    : NamedDecl(DeclKind::TypeAlias, DC, name, attrs), AliasTy(0),
      TypeAliasLoc(typealiasloc), UnderlyingTy(underlyingty) {
  }

  SMLoc getLocStart() const { return TypeAliasLoc; }

  /// getAliasType - Return the sugared version of this decl as a Type.
  NameAliasType *getAliasType(ASTContext &C) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::TypeAlias;
  }
  static bool classof(const TypeAliasDecl *D) { return true; }
};
  
/// ValueDecl - All named decls that are values in the language.  These can
/// have an initializer, type, etc.
class ValueDecl : public NamedDecl {
public:
  Type Ty;
  Expr *Init;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return (D->Kind == DeclKind::Var || D->Kind == DeclKind::Func ||
            D->Kind == DeclKind::OneOfElement ||
            D->Kind == DeclKind::Arg ||
            D->Kind == DeclKind::ElementRef);
  }
  static bool classof(const ValueDecl *D) { return true; }

protected:
  ValueDecl(DeclKind K, DeclContext *DC, Identifier name, Type ty, Expr *init,
            const DeclAttributes &attrs = DeclAttributes())
    : NamedDecl(K, DC, name, attrs), Ty(ty), Init(init) {
  }
};  

/// VarDecl - 'var' declaration.
class VarDecl : public ValueDecl {
public:
  SMLoc VarLoc;    // Location of the 'var' token.

  /// NestedName - If this is a simple var definition, the name is stored in the
  /// name identifier.  If the varname is complex, Name is empty and this
  /// contains the nested name specifier.
  DeclVarName *NestedName;
  
  VarDecl(DeclContext *DC, SMLoc varloc, Identifier name, Type ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(DeclKind::Var, DC, name, ty, init, attrs), VarLoc(varloc),
      NestedName(0) {}
  VarDecl(DeclContext *DC, SMLoc varloc, DeclVarName *name, Type ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(DeclKind::Var, DC, Identifier(), ty, init, attrs), VarLoc(varloc),
      NestedName(name) {}

  
  SMLoc getLocStart() const { return VarLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->Kind == DeclKind::Var; }
  static bool classof(const VarDecl *D) { return true; }

};
  

/// FuncDecl - 'func' declaration.
class FuncDecl : public ValueDecl {
public:
  SMLoc FuncLoc;    // Location of the 'func' token.

  FuncDecl(DeclContext *DC, SMLoc funcloc, Identifier name, Type ty, Expr *init,
          const DeclAttributes &attrs)
    : ValueDecl(DeclKind::Func, DC, name, ty, init, attrs), FuncLoc(funcloc) {}
  
  
  SMLoc getLocStart() const { return FuncLoc; }

  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->Kind == DeclKind::Func; }
  static bool classof(const FuncDecl *D) { return true; }
};

/// OneOfElementDecl - This represents an element of a 'oneof' declaration, e.g.
/// X and Y in:
///   oneof d { X : int, Y : int, Z }
/// The type of a OneOfElementDecl is always the OneOfType for the containing
/// oneof.
class OneOfElementDecl : public ValueDecl {
public:
  SMLoc IdentifierLoc;
  
  /// ArgumentType - This is the type specified with the oneof element.  For
  /// example 'int' in the Y example above.  This is null if there is no type
  /// associated with this element (such as in the Z example).
  Type ArgumentType;
  
  
  OneOfElementDecl(DeclContext *DC, SMLoc identloc, Identifier name, Type ty,
                   Type argtype)
  : ValueDecl(DeclKind::OneOfElement, DC, name, ty, 0),
    IdentifierLoc(identloc), ArgumentType(argtype) {}

  
  SMLoc getLocStart() const { return IdentifierLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::OneOfElement;
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
  SMLoc FuncLoc;
  
  // FIXME: Store the access path here.
  
  ArgDecl(DeclContext *DC, SMLoc funcloc, Identifier name, Type ty)
    : ValueDecl(DeclKind::Arg, DC, name, ty, 0, DeclAttributes()),
      FuncLoc(funcloc) {}


  SMLoc getLocStart() const { return FuncLoc; }
 
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->Kind == DeclKind::Arg; }
  static bool classof(const ArgDecl *D) { return true; }
};

/// ElementRefDecl - A reference to the element of another decl which is formed
/// through name binding.  For example, in "var (a,b) = f();" there is a VarDecl
/// with no name and two ElementRefDecls (named A and B) referring to elements
/// of the nameless vardecl.
class ElementRefDecl : public ValueDecl {
public:
  VarDecl *VD;
  SMLoc NameLoc;
  ArrayRef<unsigned> AccessPath;
  
  ElementRefDecl(DeclContext *DC, VarDecl *vd, SMLoc nameloc, Identifier name,
                 ArrayRef<unsigned> path, Type ty)
    : ValueDecl(DeclKind::ElementRef, DC, name, ty, 0), VD(vd),
      NameLoc(nameloc), AccessPath(path) {
  }

  /// getTypeForPath - Given a type and an access path into it, return the
  /// referenced element type.  If the access path is invalid for the specified
  /// type, this returns null.  If the query goes into an unresolved (dependent)
  /// part of the type, this returns DependentType.
  static Type getTypeForPath(Type Ty, ArrayRef<unsigned> Path);
  
  
  SMLoc getLocStart() const { return NameLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::ElementRef;
  }
  static bool classof(const ElementRefDecl *D) { return true; }
};

  
} // end namespace swift

#endif
