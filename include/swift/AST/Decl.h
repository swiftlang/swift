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
  class FuncExpr;
  class BraceStmt;
  class Component;
  class DeclAttributes;
  class OneOfElementDecl;
  class NameAliasType;
  enum class Resilience : unsigned char;
  class TypeAliasDecl;
  
enum class DeclKind {
#define DECL(Id, Parent) Id,
#define DECL_RANGE(Id, FirstId, LastId) \
  First_##Id##Decl = FirstId, Last_##Id##Decl = LastId,
#include "swift/AST/DeclNodes.def"
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
  const DeclKind Kind;
  DeclContext *Context;

  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;

protected:
  Decl(DeclKind kind, DeclContext *DC) : Kind(kind), Context(DC) {}

public:
  /// Alignment - The required alignment of Decl objects.
  enum { Alignment = 8 };

  DeclKind getKind() const { return Kind; }

  DeclContext *getDeclContext() const { return Context; }
  void setDeclContext(DeclContext *DC) { Context = DC; }

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
  
  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  // Only allow allocation of Decls using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = Decl::Alignment) throw();
  void *operator new(size_t Bytes, void *Mem) throw() { 
    assert(Mem); 
    return Mem; 
  }
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import swift
///   import swift.int
class ImportDecl : public Decl {
public:
  typedef std::pair<Identifier, SourceLoc> AccessPathElement;

private:
  SourceLoc ImportLoc;

  /// The number of elements in this path.
  unsigned NumPathElements;

  AccessPathElement *getPathBuffer() {
    return reinterpret_cast<AccessPathElement*>(this+1);
  }
  const AccessPathElement *getPathBuffer() const {
    return reinterpret_cast<const AccessPathElement*>(this+1);
  }
  
  ImportDecl(DeclContext *DC, SourceLoc ImportLoc,
             ArrayRef<AccessPathElement> Path);

public:
  static ImportDecl *create(ASTContext &C, DeclContext *DC,
                            SourceLoc ImportLoc,
                            ArrayRef<AccessPathElement> Path);

  ArrayRef<AccessPathElement> getAccessPath() const {
    return ArrayRef<AccessPathElement>(getPathBuffer(), NumPathElements);
  }
  
  SourceLoc getLocStart() const { return ImportLoc; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Import;
  }
  static bool classof(const ImportDecl *D) { return true; }
};

/// ExtensionDecl - This represents a type extension containing methods
/// associated with the type.  This is not a ValueDecl and has no Type because
/// there are no runtime values of the Extension's type.  
class ExtensionDecl : public Decl, public DeclContext {
  SourceLoc ExtensionLoc;  // Location of 'extension' keyword.
  
  /// ExtendedType - The type being extended.
  Type ExtendedType;
  ArrayRef<Decl*> Members;
public:

  ExtensionDecl(SourceLoc ExtensionLoc, Type ExtendedType,
                ArrayRef<Decl*> Members, DeclContext *Parent)
    : Decl(DeclKind::Extension, Parent),
      DeclContext(DeclContextKind::ExtensionDecl, Parent),
      ExtensionLoc(ExtensionLoc),
      ExtendedType(ExtendedType), Members(Members) {
  }
  
  SourceLoc getExtensionLoc() const { return ExtensionLoc; }
  SourceLoc getLocStart() const { return ExtensionLoc; }
  Type getExtendedType() const { return ExtendedType; }
  ArrayRef<Decl*> getMembers() const { return Members; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Extension;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::ExtensionDecl;
  }
  static bool classof(const ExtensionDecl *D) { return true; }
};
  
  
  
/// NamedDecl - An abstract base class for declarations with names.
class NamedDecl : public Decl {
  Identifier Name;
  const DeclAttributes *Attrs;
  static const DeclAttributes EmptyAttrs;

protected:
  NamedDecl(DeclKind K, DeclContext *DC, Identifier name)
    : Decl(K, DC), Name(name), Attrs(&EmptyAttrs) {
  }
  
public:
  Identifier getName() const { return Name; }
  bool isOperator() const { return Name.isOperator(); }

  DeclAttributes &getMutableAttrs();
  const DeclAttributes &getAttrs() const { return *Attrs; }

  Resilience getResilienceFrom(Component *C) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_NamedDecl &&
           D->getKind() <= DeclKind::Last_NamedDecl;
  }
  static bool classof(const NamedDecl *D) { return true; }
};

/// ValueDecl - All named decls that are values in the language.  These can
/// have an initializer, type, etc.
class ValueDecl : public NamedDecl {
  Type Ty;

protected:
  ValueDecl(DeclKind K, DeclContext *DC, Identifier name, Type ty)
    : NamedDecl(K, DC, name), Ty(ty) {
  }

public:

  /// isDefinition - Return true if this is a definition of a decl, not a
  /// forward declaration (e.g. of a function) that is implemented outside of
  /// the swift code.
  bool isDefinition() const;
  
  bool hasType() const { return !Ty.isNull(); }
  Type getType() const {
    assert(!Ty.isNull() && "declaration has no type set yet");
    return Ty;
  }

  /// Set the type of this declaration for the first time.
  void setType(Type T) {
    assert(Ty.isNull() && "changing type of declaration");
    Ty = T;
  }

  /// Overwrite the type of this declaration.
  void overwriteType(Type T) {
    Ty = T;
  }

  /// getTypeJudgement - Returns the type judgement that should arise
  /// from a normal reference to this declaration.
  TypeJudgement getTypeJudgement() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_ValueDecl &&
           D->getKind() <= DeclKind::Last_ValueDecl;
  }
  static bool classof(const ValueDecl *D) { return true; }
};  

/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias foo : int
///
/// TypeAliasDecl's always have 'MetaTypeType' type.
///
class TypeAliasDecl : public ValueDecl {
  /// The type that represents this (sugared) name alias.
  mutable NameAliasType *AliasTy;

  SourceLoc TypeAliasLoc;
  Type UnderlyingTy;
  
public:
  TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                Type Underlyingty, DeclContext *DC);
  
  SourceLoc getTypeAliasLoc() const { return TypeAliasLoc; }
  void setTypeAliasLoc(SourceLoc loc) { TypeAliasLoc = loc; }

  /// hasUnderlyingType - Returns whether the underlying type has been set.
  bool hasUnderlyingType() const {
    return !UnderlyingTy.isNull();
  }

  /// getUnderlyingType - Returns the underlying type, which is
  /// assumed to have been set.
  Type getUnderlyingType() const {
    assert(!UnderlyingTy.isNull() && "getting invalid underlying type");
    return UnderlyingTy;
  }

  /// setUnderlyingType - Set the underlying type.  This is meant to
  /// be used when resolving an unresolved type name during name-binding.
  void setUnderlyingType(Type T) {
    assert(UnderlyingTy.isNull() && "changing underlying type of type-alias");
    UnderlyingTy = T;
  }

  /// overwriteUnderlyingType - Actually change the underlying type.
  /// Typically it is overwritten to an error type.  It's possible for
  /// type canonicalization to not see these changes.
  void overwriteUnderlyingType(Type T) {
    UnderlyingTy = T;
  }

  SourceLoc getLocStart() const { return TypeAliasLoc; }

  /// getAliasType - Return the sugared version of this decl as a Type.
  NameAliasType *getAliasType() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TypeAlias;
  }
  static bool classof(const TypeAliasDecl *D) { return true; }
};
  

/// VarDecl - 'var' declaration.
class VarDecl : public ValueDecl {
private:
  SourceLoc VarLoc;    // Location of the 'var' token.
  Expr *Init;

  /// NestedName - If this is a simple var definition, the name is stored in the
  /// name identifier.  If the varname is complex, Name is empty and this
  /// contains the nested name specifier.
  DeclVarName *NestedName;
  
public:
  VarDecl(SourceLoc VarLoc, Identifier Name, Type Ty, Expr *Init,
          DeclContext *DC)
    : ValueDecl(DeclKind::Var, DC, Name, Ty), VarLoc(VarLoc), Init(Init),
      NestedName(0) {}
  VarDecl(SourceLoc VarLoc, DeclVarName *Name, Type Ty, Expr *Init,
          DeclContext *DC)
    : ValueDecl(DeclKind::Var, DC, Identifier(), Ty),
      VarLoc(VarLoc), Init(Init), NestedName(Name) {}

  Expr *getInit() const { return Init; }
  void setInit(Expr *init) { Init = init; }

  /// getVarLoc - The location of the 'var' token.
  SourceLoc getVarLoc() const { return VarLoc; }

  /// getNestedName - Returns the nested-name-specifier of this
  /// variable, if it has one.
  DeclVarName *getNestedName() const { return NestedName; }
  void setNestedName(DeclVarName *name) { NestedName = name; }
  
  SourceLoc getLocStart() const { return VarLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Var; }
  static bool classof(const VarDecl *D) { return true; }
};
  

/// FuncDecl - 'func' declaration.
class FuncDecl : public ValueDecl {
  SourceLoc PlusLoc;    // Location of the 'plus' token or invalid if not 'plus'
  SourceLoc FuncLoc;    // Location of the 'func' token.
  FuncExpr *Body;
public:
  FuncDecl(SourceLoc PlusLoc, SourceLoc FuncLoc, Identifier Name,
           Type Ty, FuncExpr *Body, DeclContext *DC)
    : ValueDecl(DeclKind::Func, DC, Name, Ty), PlusLoc(PlusLoc),
      FuncLoc(FuncLoc), Body(Body) {
  }
  
  bool isPlus() const { return PlusLoc.isValid(); }

  FuncExpr *getBody() const { return Body; }
  void setBody(FuncExpr *NewBody) { Body = NewBody; }

  
  SourceLoc getPlusLoc() const { return PlusLoc; }
  SourceLoc getFuncLoc() const { return FuncLoc; }
    
  SourceLoc getLocStart() const {
    return PlusLoc.isValid() ? PlusLoc : FuncLoc;
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Func; }
  static bool classof(const FuncDecl *D) { return true; }
};

/// OneOfElementDecl - This represents an element of a 'oneof' declaration, e.g.
/// X and Y in:
///   oneof d { X : int, Y : int, Z }
/// The type of a OneOfElementDecl is always the OneOfType for the containing
/// oneof.
class OneOfElementDecl : public ValueDecl {
  SourceLoc IdentifierLoc;
  
  /// ArgumentType - This is the type specified with the oneof element.  For
  /// example 'int' in the Y example above.  This is null if there is no type
  /// associated with this element (such as in the Z example).
  Type ArgumentType;
    
public:
  OneOfElementDecl(SourceLoc IdentifierLoc, Identifier Name, Type Ty,
                   Type ArgumentType, DeclContext *DC)
  : ValueDecl(DeclKind::OneOfElement, DC, Name, Ty),
    IdentifierLoc(IdentifierLoc), ArgumentType(ArgumentType) {}

  Type getArgumentType() const { return ArgumentType; }

  SourceLoc getIdentifierLoc() const { return IdentifierLoc; }
  SourceLoc getLocStart() const { return IdentifierLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::OneOfElement;
  }
  static bool classof(const OneOfElementDecl *D) { return true; }
 
};

  
/// ElementRefDecl - A reference to the element of another decl which is formed
/// through name binding.  For example, in "var (a,b) = f();" there is a VarDecl
/// with no name and two ElementRefDecls (named A and B) referring to elements
/// of the nameless vardecl.
class ElementRefDecl : public ValueDecl {
  VarDecl *VD;
  SourceLoc NameLoc;
  ArrayRef<unsigned> AccessPath;
  
public:
  ElementRefDecl(VarDecl *VD, SourceLoc NameLoc, Identifier Name,
                 ArrayRef<unsigned> Path, Type Ty, DeclContext *DC)
    : ValueDecl(DeclKind::ElementRef, DC, Name, Ty), VD(VD),
      NameLoc(NameLoc), AccessPath(Path) {
  }

  VarDecl *getVarDecl() const { return VD; }

  ArrayRef<unsigned> getAccessPath() const { return AccessPath; }

  /// getTypeForPath - Given a type and an access path into it, return the
  /// referenced element type.  If the access path is invalid for the specified
  /// type, this returns null.  If the query goes into an unresolved (dependent)
  /// part of the type, this returns DependentType.
  static Type getTypeForPath(Type Ty, ArrayRef<unsigned> Path);
  
  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLocStart() const { return NameLoc; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::ElementRef;
  }
  static bool classof(const ElementRefDecl *D) { return true; }
};
  
} // end namespace swift

#endif
