//===--- TypeRepr.h - Swift Language Type Representation --------*- C++ -*-===//
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
// This file defines the TypeRepr and related classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPEREPR_H
#define SWIFT_AST_TYPEREPR_H

#include "swift/AST/Attr.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {
  class ASTWalker;
  class DeclContext;
  class ValueDecl;
  class Module;
  class ExprHandle;
  class NamedTypeRepr;

enum class TypeReprKind : uint8_t {
#define TYPEREPR(ID, PARENT) ID,
#include "TypeReprNodes.def"
};

/// \brief Representation of a type as written in source.
class alignas(8) TypeRepr {
  TypeRepr(const TypeRepr&) = delete;
  void operator=(const TypeRepr&) = delete;

  /// \brief The subclass of TypeRepr that this is.
  const TypeReprKind Kind;

protected:
  TypeRepr(TypeReprKind K) : Kind(K) {}

public:
  TypeReprKind getKind() const { return Kind; }

  SourceLoc getStartLoc() const;
  SourceLoc getEndLoc() const;
  SourceRange getSourceRange() const;

  static bool classof(const TypeRepr *T) { return true; }

  /// Walk this type representation.
  TypeRepr *walk(ASTWalker &walker);
  TypeRepr *walk(ASTWalker &&walker) {
    return walk(walker);
  }

  //*** Allocation Routines ************************************************/

  void *operator new(size_t bytes, ASTContext &C,
                     unsigned Alignment = alignof(TypeRepr));

  // Make placement new and vanilla new/delete illegal for TypeReprs.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;
  void *operator new(size_t bytes, void *data) = delete;

  void print(llvm::raw_ostream &OS) const;
  void dump() const;
};

/// \brief A TypeRepr for a type with a syntax error.  Can be used both as a
/// top-level TypeRepr and as a part of other TypeRepr.
///
/// The client should make sure to emit a diagnostic at the construction time
/// (in the parser).  All uses of this type should be ignored and not
/// re-diagnosed.
class ErrorTypeRepr : public TypeRepr {
  SourceRange Range;

public:
  ErrorTypeRepr() : TypeRepr(TypeReprKind::Error) {}
  ErrorTypeRepr(SourceLoc Loc) : TypeRepr(TypeReprKind::Error), Range(Loc) {}
  ErrorTypeRepr(SourceRange Range)
      : TypeRepr(TypeReprKind::Error), Range(Range) {}

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Error;
  }
  static bool classof(const ErrorTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Range.Start; }
  SourceLoc getEndLocImpl() const { return Range.End; }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A type with attributes.
/// \code
///   [byref] Foo
/// \endcode
class AttributedTypeRepr : public TypeRepr {
  // FIXME: DeclAttributes wastes space.
  DeclAttributes Attrs;
  TypeRepr *Ty;

public:
  AttributedTypeRepr(const DeclAttributes &Attrs, TypeRepr *Ty)
    : TypeRepr(TypeReprKind::Attributed), Attrs(Attrs), Ty(Ty) {
  }

  const DeclAttributes &getAttrs() const { return Attrs; }
  TypeRepr *getTypeRepr() const { return Ty; }

  void printAttrs(llvm::raw_ostream &OS) const;

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Attributed;
  }
  static bool classof(const AttributedTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Attrs.LSquareLoc; }
  SourceLoc getEndLocImpl() const { return Ty->getEndLoc(); }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A type with identifier components.
/// \code
///   Foo.Bar<Gen>
/// \endcode
/// FIXME: Investigate splitting into a TypeRepr for each component,
/// so that "Int" does not need an allocation for the components array and the
/// generic arguments info.
class IdentTypeRepr : public TypeRepr {
public:
  class Component {
    SourceLoc Loc;
    Identifier Id;
    MutableArrayRef<TypeRepr*> GenericArgs;

    /// Value is the decl or module that this refers to.
    ///
    /// Before name binding, each component has its value set to a DeclContext
    /// for the root lookup, giving a context for that lookup.
    ///
    /// After name binding, the value is set to the decl being referenced, and
    /// the last entry in the component list is known to be a Type.
    ///
    /// FIXME: DeclContext* should be available from the context within the type
    /// checker. In that case, remove it and sneak the Id in its place.
    llvm::PointerUnion4<DeclContext*, ValueDecl*, Type, Module*> Value;

  public:
    Component(SourceLoc Loc, Identifier Id,
              MutableArrayRef<TypeRepr*> GenericArgs,
              DeclContext *Ctx) :
      Loc(Loc), Id(Id), GenericArgs(GenericArgs), Value(Ctx) {}

    SourceLoc getIdLoc() const { return Loc; }
    Identifier getIdentifier() const { return Id; }
    MutableArrayRef<TypeRepr*> getGenericArgs() const { return GenericArgs; }

    /// isBound - Return true if this Component has been namebound already.
    bool isBound() const { return !Value.is<DeclContext*>(); }
    bool isBoundDecl() const { return Value.is<ValueDecl*>(); }
    bool isBoundType() const { return Value.is<Type>(); }
    bool isBoundModule() const { return Value.is<Module*>(); }

    DeclContext *getUnboundContext() const {
      return Value.dyn_cast<DeclContext*>();
    }
    ValueDecl *getBoundDecl() const {
      return Value.dyn_cast<ValueDecl*>();
    }
    Type getBoundType() const {
      return Value.dyn_cast<Type>();
    }
    Module *getBoundModule() const {
      return Value.dyn_cast<Module*>();
    }

    void setValue(ValueDecl *VD) { Value = VD; }
    void setValue(Type T) { Value = T; }
    void setValue(Module *M) { Value = M; }

    void revertToContext(DeclContext *DC) { Value = DC; }
  };

public:
  const MutableArrayRef<Component> Components;

  IdentTypeRepr(MutableArrayRef<Component> Components)
    : TypeRepr(TypeReprKind::Ident), Components(Components) {
    assert(!Components.empty());
  }

  static IdentTypeRepr *create(ASTContext &C, ArrayRef<Component> Components);

  static IdentTypeRepr *createSimple(ASTContext &C, SourceLoc Loc,
                                     Identifier Id, DeclContext *Ctx);

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Ident;
  }
  static bool classof(const IdentTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Components.front().getIdLoc(); }
  SourceLoc getEndLocImpl() const {
    if (!Components.back().getGenericArgs().empty())
      return Components.back().getGenericArgs().back()->getEndLoc();
    return Components.back().getIdLoc();
  }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A function type.
/// \code
///   Foo -> Bar
/// \endcode
class FunctionTypeRepr : public TypeRepr {
  TypeRepr *ArgsTy;
  TypeRepr *RetTy;

public:
  FunctionTypeRepr(TypeRepr *ArgsTy, TypeRepr *RetTy)
    : TypeRepr(TypeReprKind::Function), ArgsTy(ArgsTy), RetTy(RetTy) {
  }

  TypeRepr *getArgsTypeRepr() const { return ArgsTy; }
  TypeRepr *getResultTypeRepr() const { return RetTy; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Function;
  }
  static bool classof(const FunctionTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return ArgsTy->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return RetTy->getEndLoc(); }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief An array type.
/// \code
///   Foo[]
/// \endcode
class ArrayTypeRepr : public TypeRepr {
  // FIXME: Tail allocation. Use bits to determine whether Base/Size are
  // availble.
  TypeRepr *Base;
  ExprHandle *Size;
  SourceRange Brackets;

public:
  ArrayTypeRepr(TypeRepr *Base, ExprHandle *Size, SourceRange Brackets)
    : TypeRepr(TypeReprKind::Array), Base(Base), Size(Size), Brackets(Brackets){
  }

  TypeRepr *getBase() const { return Base; }
  ExprHandle *getSize() const { return Size; }
  SourceRange getBrackets() const { return Brackets; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Array;
  }
  static bool classof(const ArrayTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const {
    // This test is necessary because the type Int[4][2] is represented as
    // ArrayTypeRepr(ArrayTypeRepr(Int, 2), 4), so the range needs to cover both
    // sets of brackets.
    if (isa<ArrayTypeRepr>(Base))
      return Base->getEndLoc();
    return Brackets.End;
  }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief An optional type.
/// \code
///   Foo?
/// \endcode
class OptionalTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc QuestionLoc;

public:
  OptionalTypeRepr(TypeRepr *Base, SourceLoc Question)
    : TypeRepr(TypeReprKind::Optional), Base(Base), QuestionLoc(Question) {
  }

  TypeRepr *getBase() const { return Base; }
  SourceLoc getQuestionLoc() const { return QuestionLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Optional;
  }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return QuestionLoc; }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A tuple type.
/// \code
///   (Foo, Bar)
/// \endcode
class TupleTypeRepr : public TypeRepr {
  MutableArrayRef<TypeRepr *> Elements;
  SourceRange Parens;
  // FIXME: Tail allocation.
  SourceLoc Ellipsis;

public:
  TupleTypeRepr(MutableArrayRef<TypeRepr *> Elements, SourceRange Parens,
               SourceLoc Ellipsis)
    : TypeRepr(TypeReprKind::Tuple), Elements(Elements),
      Parens(Parens), Ellipsis(Ellipsis) {
  }

  MutableArrayRef<TypeRepr *> getElements() const { return Elements; }
  SourceRange getParens() const { return Parens; }
  SourceLoc getEllipsisLoc() const { return Ellipsis; }
  bool hasEllipsis() const { return Ellipsis.isValid(); }

  bool isParenType() const {
    return Elements.size() == 1 && !isa<NamedTypeRepr>(Elements[0]);
  }

  static TupleTypeRepr *create(ASTContext &C, ArrayRef<TypeRepr *> Elements,
                               SourceRange Parens, SourceLoc Ellipsis);

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Tuple;
  }
  static bool classof(const TupleTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Parens.Start; }
  SourceLoc getEndLocImpl() const { return Parens.End; }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A named element of a tuple type.
/// \code
///   (x: Foo = 0)
/// \endcode
class NamedTypeRepr : public TypeRepr {
  Identifier Id;
  TypeRepr *Ty;
  SourceLoc IdLoc;

public:
  NamedTypeRepr(Identifier Id, TypeRepr *Ty, SourceLoc IdLoc)
    : TypeRepr(TypeReprKind::Named), Id(Id), Ty(Ty), IdLoc(IdLoc) {
  }

  Identifier getName() const { return Id; }
  TypeRepr *getTypeRepr() const { return Ty; }
  SourceLoc getNameLoc() const { return IdLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Named;
  }
  static bool classof(const NamedTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return IdLoc; }
  SourceLoc getEndLocImpl() const { return Ty->getEndLoc(); }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A protocol composite type.
/// \code
///   protocol<Foo, Bar>
/// \endcode
class ProtocolCompositionTypeRepr : public TypeRepr {
  MutableArrayRef<IdentTypeRepr *> Protocols;
  SourceLoc ProtocolLoc;
  SourceRange AngleBrackets;

public:
  ProtocolCompositionTypeRepr(MutableArrayRef<IdentTypeRepr *> Protocols,
                              SourceLoc ProtocolLoc,
                              SourceRange AngleBrackets)
    : TypeRepr(TypeReprKind::ProtocolComposition), Protocols(Protocols),
      ProtocolLoc(ProtocolLoc), AngleBrackets(AngleBrackets) {
  }

  MutableArrayRef<IdentTypeRepr *> getProtocols() const { return Protocols; }
  SourceLoc getProtocolLoc() const { return ProtocolLoc; }
  SourceRange getAngleBrackets() const { return AngleBrackets; }

  static ProtocolCompositionTypeRepr *create(ASTContext &C,
                                             ArrayRef<IdentTypeRepr*> Protocols,
                                             SourceLoc ProtocolLoc,
                                             SourceRange AngleBrackets);

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::ProtocolComposition;
  }
  static bool classof(const ProtocolCompositionTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return ProtocolLoc; }
  SourceLoc getEndLocImpl() const { return AngleBrackets.End; }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

/// \brief A 'metatype' type.
/// \code
///   Foo.metatype
/// \endcode
class MetaTypeTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc MetaLoc;

public:
  MetaTypeTypeRepr(TypeRepr *Base, SourceLoc MetaLoc)
    : TypeRepr(TypeReprKind::MetaType), Base(Base), MetaLoc(MetaLoc) {
  }

  TypeRepr *getBase() const { return Base; }
  SourceLoc getMetaLoc() const { return MetaLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::MetaType;
  }
  static bool classof(const MetaTypeTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return MetaLoc; }
  void printImpl(llvm::raw_ostream &OS) const;
  friend class TypeRepr;
};

} // end namespace swift

namespace llvm {
  static inline raw_ostream &
  operator<<(raw_ostream &OS, swift::TypeRepr *TyR) {
    TyR->print(OS);
    return OS;
  }
} // end namespace llvm

#endif
