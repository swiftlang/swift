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
#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/ErrorHandling.h"
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
  // The indentation of the members of this enum describe the inheritance
  // hierarchy.  Commented out members are abstract classes.  This formation
  // allows for range checks in classof.
  Import,
//Named
    TypeAlias,
  //Value
      Var,
      Func,
      OneOfElement,
      Arg,
      ElementRef,
  
  FirstNamed = TypeAlias,
  LastNamed = ElementRef,
  FirstValue = Var,
  LastValue = ElementRef
};

/// The associativity of a binary operator.
enum class Associativity {
  /// Non-associative operators cannot be written next to other
  /// operators with the same precedence.  Relational operators are
  /// typically non-associative.
  None,

  /// Left-associative operators associate to the left if written next
  /// to other left-associative operators of the same precedence.
  Left,

  /// Right-associative operators associate to the right if written
  /// next to other right-associative operators of the same precedence.
  Right
};

class InfixData {
  unsigned Precedence : 8;

  /// Zero if invalid, or else an Associativity+1.
  unsigned InvalidOrAssoc : 8;
public:
  InfixData() : Precedence(0), InvalidOrAssoc(0) {}
  InfixData(unsigned prec, Associativity assoc)
    : Precedence(prec), InvalidOrAssoc(unsigned(assoc) + 1) {}

  bool isValid() const { return InvalidOrAssoc != 0; }

  Associativity getAssociativity() const {
    assert(isValid());
    return Associativity(InvalidOrAssoc - 1);
  }
  bool isLeftAssociative() const {
    return getAssociativity() == Associativity::Left;
  }
  bool isRightAssociative() const {
    return getAssociativity() == Associativity::Right;
  }
  bool isNonAssociative() const {
    return getAssociativity() == Associativity::None;
  }

  unsigned getPrecedence() const {
    assert(isValid());
    return Precedence;
  }

  friend bool operator==(InfixData L, InfixData R) {
    return L.Precedence == R.Precedence
        && L.InvalidOrAssoc == R.InvalidOrAssoc;
  }
  friend bool operator!=(InfixData L, InfixData R) {
    return !operator==(L, R);
  }
};
  
/// DeclAttributes - These are attributes that may be applied to declarations.
class DeclAttributes {
public:
  /// LSquareLoc/RSquareLoc - This is the location of the '[' and ']' in the
  /// attribute specifier.  If this is an empty attribute specifier, then these
  /// will be invalid locs.
  SourceLoc LSquareLoc, RSquareLoc;

  InfixData Infix;

  DeclAttributes() { }

  bool isInfix() const { return Infix.isValid(); }
  InfixData getInfixData() const { return Infix; }
    
  bool empty() const {
    return !isInfix();
  }
};

  
  
/// DeclVarName - This is a recursive structure that represents the varname
/// datatype which can be used to name various pieces of a var definition.  For
/// example:  var ((a, b), c) = foo();
///
class DeclVarName {
  /// LPLoc/RPLoc - This is the location of the '(' and ')' if this is a complex
  /// name, or both contain the same location if this is simple.
  SourceLoc LPLoc, RPLoc;
  
  union {
    void *Name; //< Storage for a simple variable name
    struct {
      DeclVarName * const *Start;
      unsigned Length;
    } Elements; //< Storage for a parenthesized list of variable names
  };
  
public:
  DeclVarName() {}
  
  DeclVarName(Identifier Name, SourceLoc NameLoc)
    : LPLoc(NameLoc), RPLoc(NameLoc), Name(Name.getAsOpaquePointer()) { }
  
  DeclVarName(SourceLoc LPLoc, ArrayRef<DeclVarName *> Elements,
              SourceLoc RPLoc) : LPLoc(LPLoc), RPLoc(RPLoc) {
    this->Elements.Start = Elements.data();
    this->Elements.Length = Elements.size();
  }
  
  SourceLoc getLocation() const { return LPLoc; }
  
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

  SourceRange getSourceRange() {
    return SourceRange(LPLoc, RPLoc);
  }
  
private:
  // Make placement new and vanilla new/delete illegal for DeclVarNames.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
  void *operator new(size_t Bytes, void *Mem) throw() = delete;
public:
  // Only allow allocation of DeclVarNames using the allocator in ASTContext
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

  /// getASTContext - Return the ASTContext that this decl lives in.
  ASTContext &getASTContext() const {
    assert(Context && "Decl doesn't have an assigned context");
    return Context->getASTContext();
  }
  
  SourceLoc getLocStart() const;
 
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
  // Only allow allocation of Decls using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import swift
///   import swift.int
class ImportDecl : public Decl {
public:
  SourceLoc ImportLoc;
  ArrayRef<std::pair<Identifier, SourceLoc>> AccessPath;
  
  ImportDecl(SourceLoc ImportLoc,
             ArrayRef<std::pair<Identifier,SourceLoc>> Path, DeclContext *DC)
    : Decl(DeclKind::Import, DC), ImportLoc(ImportLoc), AccessPath(Path) {
  }
  
  SourceLoc getLocStart() const { return ImportLoc; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::Import;
  }
  static bool classof(const ImportDecl *D) { return true; }
};

  
/// NamedDecl - An abstract base class for declarations with names.
class NamedDecl : public Decl {
public:
  Identifier Name;
  DeclAttributes Attrs;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind >= DeclKind::FirstNamed && D->Kind <= DeclKind::LastNamed;
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
  SourceLoc TypeAliasLoc;
  Type UnderlyingTy;
  
  TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                Type underlyingty, const DeclAttributes &Attrs, DeclContext *DC)
    : NamedDecl(DeclKind::TypeAlias, DC, Name, Attrs), AliasTy(0),
      TypeAliasLoc(TypeAliasLoc), UnderlyingTy(underlyingty) {
  }

  SourceLoc getLocStart() const { return TypeAliasLoc; }

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
    return D->Kind >= DeclKind::FirstValue && D->Kind <= DeclKind::LastValue;
  }
  static bool classof(const ValueDecl *D) { return true; }

  TypeJudgement getTypeJudgement() const;

protected:
  ValueDecl(DeclKind K, DeclContext *DC, Identifier name, Type ty, Expr *init,
            const DeclAttributes &attrs = DeclAttributes())
    : NamedDecl(K, DC, name, attrs), Ty(ty), Init(init) {
  }
};  

/// VarDecl - 'var' declaration.
class VarDecl : public ValueDecl {
public:
  SourceLoc VarLoc;    // Location of the 'var' token.

  /// NestedName - If this is a simple var definition, the name is stored in the
  /// name identifier.  If the varname is complex, Name is empty and this
  /// contains the nested name specifier.
  DeclVarName *NestedName;
  
  VarDecl(SourceLoc VarLoc, Identifier Name, Type Ty, Expr *Init,
          const DeclAttributes &Attrs, DeclContext *DC)
    : ValueDecl(DeclKind::Var, DC, Name, Ty, Init, Attrs), VarLoc(VarLoc),
      NestedName(0) {}
  VarDecl(SourceLoc VarLoc, DeclVarName *Name, Type Ty, Expr *Init,
          const DeclAttributes &Attrs, DeclContext *DC)
    : ValueDecl(DeclKind::Var, DC, Identifier(), Ty, Init, Attrs),
      VarLoc(VarLoc), NestedName(Name) {}

  
  SourceLoc getLocStart() const { return VarLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->Kind == DeclKind::Var; }
  static bool classof(const VarDecl *D) { return true; }

};
  

/// FuncDecl - 'func' declaration.
class FuncDecl : public ValueDecl {
public:
  SourceLoc FuncLoc;    // Location of the 'func' token.

  FuncDecl(SourceLoc FuncLoc, Identifier Name, Type Ty, Expr *Init,
           const DeclAttributes &Attrs, DeclContext *DC)
    : ValueDecl(DeclKind::Func, DC, Name, Ty, Init, Attrs), FuncLoc(FuncLoc) {}
    
  SourceLoc getLocStart() const { return FuncLoc; }

  
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
  SourceLoc IdentifierLoc;
  
  /// ArgumentType - This is the type specified with the oneof element.  For
  /// example 'int' in the Y example above.  This is null if there is no type
  /// associated with this element (such as in the Z example).
  Type ArgumentType;
  
  
  OneOfElementDecl(SourceLoc IdentifierLoc, Identifier Name, Type Ty,
                   Type ArgumentType, DeclContext *DC)
  : ValueDecl(DeclKind::OneOfElement, DC, Name, Ty, 0),
    IdentifierLoc(IdentifierLoc), ArgumentType(ArgumentType) {}

  
  SourceLoc getLocStart() const { return IdentifierLoc; }
  
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
  SourceLoc FuncLoc;
  
  // FIXME: Store the access path here.
  
  ArgDecl(SourceLoc FuncLoc, Identifier Name, Type Ty, DeclContext *DC)
    : ValueDecl(DeclKind::Arg, DC, Name, Ty, 0, DeclAttributes()),
      FuncLoc(FuncLoc) {}


  SourceLoc getLocStart() const { return FuncLoc; }
 
  
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
  SourceLoc NameLoc;
  ArrayRef<unsigned> AccessPath;
  
  ElementRefDecl(VarDecl *VD, SourceLoc NameLoc, Identifier Name,
                 ArrayRef<unsigned> Path, Type Ty, DeclContext *DC)
    : ValueDecl(DeclKind::ElementRef, DC, Name, Ty, 0), VD(VD),
      NameLoc(NameLoc), AccessPath(Path) {
  }

  /// getTypeForPath - Given a type and an access path into it, return the
  /// referenced element type.  If the access path is invalid for the specified
  /// type, this returns null.  If the query goes into an unresolved (dependent)
  /// part of the type, this returns DependentType.
  static Type getTypeForPath(Type Ty, ArrayRef<unsigned> Path);
  
  
  SourceLoc getLocStart() const { return NameLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->Kind == DeclKind::ElementRef;
  }
  static bool classof(const ElementRefDecl *D) { return true; }
};
  
} // end namespace swift

#endif
