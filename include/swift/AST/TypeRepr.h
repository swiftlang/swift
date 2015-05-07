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
#include "llvm/Support/ErrorHandling.h"

namespace swift {
  class ASTWalker;
  class DeclContext;
  class ValueDecl;
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

  /// Is this type grammatically a type-simple?
  inline bool isSimple() const; // bottom of this file

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

  void print(raw_ostream &OS, const PrintOptions &Opts = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &Opts) const;
  void dump() const;

  /// Clone the given type representation.
  TypeRepr *clone(ASTContext &ctx) const;
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
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A type with attributes.
/// \code
///   @convention(thin) Foo
/// \endcode
class AttributedTypeRepr : public TypeRepr {
  // FIXME: TypeAttributes isn't a great use of space.
  TypeAttributes Attrs;
  TypeRepr *Ty;

public:
  AttributedTypeRepr(const TypeAttributes &Attrs, TypeRepr *Ty)
    : TypeRepr(TypeReprKind::Attributed), Attrs(Attrs), Ty(Ty) {
  }

  const TypeAttributes &getAttrs() const { return Attrs; }
  TypeRepr *getTypeRepr() const { return Ty; }

  void printAttrs(llvm::raw_ostream &OS) const;
  void printAttrs(ASTPrinter &Printer) const;

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Attributed;
  }
  static bool classof(const AttributedTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Attrs.AtLoc; }
  SourceLoc getEndLocImpl() const { return Ty->getEndLoc(); }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

class ComponentIdentTypeRepr;

/// \brief This is the abstract base class for types with identifier components.
/// \code
///   Foo.Bar<Gen>
/// \endcode
class IdentTypeRepr : public TypeRepr {
protected:
  explicit IdentTypeRepr(TypeReprKind K) : TypeRepr(K) {}

public:
  /// Copies the provided array and creates a CompoundIdentTypeRepr or just
  /// returns the single entry in the array if it contains only one.
  static IdentTypeRepr *create(ASTContext &C,
                               ArrayRef<ComponentIdentTypeRepr *> Components);
  
  class ComponentRange;
  inline ComponentRange getComponentRange();

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::SimpleIdent  ||
           T->getKind() == TypeReprKind::GenericIdent ||
           T->getKind() == TypeReprKind::CompoundIdent;
  }
  static bool classof(const IdentTypeRepr *T) { return true; }
};

class ComponentIdentTypeRepr : public IdentTypeRepr {
  SourceLoc Loc;
  Identifier Id;

  /// Value is the decl or type that this refers to.
  ///
  /// After name binding, the value is set to the decl being
  /// referenced. After type checking, it is set to the type.
  ///
  /// FIXME: Can we sneak the Id into this union?
  llvm::PointerUnion<ValueDecl*, Type> Value;

protected:
  ComponentIdentTypeRepr(TypeReprKind K, SourceLoc Loc, Identifier Id)
    : IdentTypeRepr(K), Loc(Loc), Id(Id) {}

public:
  SourceLoc getIdLoc() const { return Loc; }
  Identifier getIdentifier() const { return Id; }

  /// Replace the identifier with a new identifier, e.g., due to typo
  /// correction.
  void overwriteIdentifier(Identifier newId) { Id = newId; }

  /// Return true if this has been name-bound already.
  bool isBound() const { return !Value.isNull(); }
  bool isBoundDecl() const { return Value.is<ValueDecl*>() && isBound(); }
  bool isBoundType() const { return Value.is<Type>() && isBound(); }

  ValueDecl *getBoundDecl() const {
    return Value.dyn_cast<ValueDecl*>();
  }
  Type getBoundType() const {
    return Value.dyn_cast<Type>();
  }

  void setValue(ValueDecl *VD) { Value = VD; }
  void setValue(Type T) { Value = T; }

  void revert() { Value = (ValueDecl*)nullptr; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::SimpleIdent ||
           T->getKind() == TypeReprKind::GenericIdent;
  }
  static bool classof(const ComponentIdentTypeRepr *T) { return true; }

protected:
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
};

/// \brief A simple identifier type like "Int".
class SimpleIdentTypeRepr : public ComponentIdentTypeRepr {
public:
  SimpleIdentTypeRepr(SourceLoc Loc, Identifier Id)
    : ComponentIdentTypeRepr(TypeReprKind::SimpleIdent, Loc, Id) {}

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::SimpleIdent;
  }
  static bool classof(const SimpleIdentTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return getIdLoc(); }
  SourceLoc getEndLocImpl() const { return getIdLoc(); }
  friend class TypeRepr;
};

/// \brief An identifier type with generic arguments.
/// \code
///   Bar<Gen>
/// \endcode
class GenericIdentTypeRepr : public ComponentIdentTypeRepr {
  ArrayRef<TypeRepr*> GenericArgs;
  SourceRange AngleBrackets;

public:
  GenericIdentTypeRepr(SourceLoc Loc, Identifier Id,
                       ArrayRef<TypeRepr*> GenericArgs,
                       SourceRange AngleBrackets)
    : ComponentIdentTypeRepr(TypeReprKind::GenericIdent, Loc, Id),
      GenericArgs(GenericArgs), AngleBrackets(AngleBrackets) {
    assert(!GenericArgs.empty());
  }

  ArrayRef<TypeRepr*> getGenericArgs() const { return GenericArgs; }
  SourceRange getAngleBrackets() const { return AngleBrackets; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::GenericIdent;
  }
  static bool classof(const GenericIdentTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return getIdLoc(); }
  SourceLoc getEndLocImpl() const { return AngleBrackets.End; }
  friend class TypeRepr;
};

/// \brief A type with identifier components.
/// \code
///   Foo.Bar<Gen>
/// \endcode
class CompoundIdentTypeRepr : public IdentTypeRepr {
public:
  const ArrayRef<ComponentIdentTypeRepr *> Components;

  explicit CompoundIdentTypeRepr(ArrayRef<ComponentIdentTypeRepr *> Components)
    : IdentTypeRepr(TypeReprKind::CompoundIdent),
      Components(Components) {
    assert(Components.size() > 1 &&
           "should have just used the single ComponentIdentTypeRepr directly");
  }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::CompoundIdent;
  }
  static bool classof(const CompoundIdentTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Components.front()->getStartLoc();}
  SourceLoc getEndLocImpl() const { return Components.back()->getEndLoc(); }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// This wraps an IdentTypeRepr and provides an iterator interface for the
/// components (or the single component) it represents.
class IdentTypeRepr::ComponentRange {
  IdentTypeRepr *IdT;

public:
  explicit ComponentRange(IdentTypeRepr *T) : IdT(T) {}

  typedef ComponentIdentTypeRepr * const* iterator;

  iterator begin() const {
    if (isa<ComponentIdentTypeRepr>(IdT))
      return reinterpret_cast<iterator>(&IdT);
    return cast<CompoundIdentTypeRepr>(IdT)->Components.begin();
  }

  iterator end() const {
    if (isa<ComponentIdentTypeRepr>(IdT))
      return reinterpret_cast<iterator>(&IdT) + 1;
    return cast<CompoundIdentTypeRepr>(IdT)->Components.end();
  }

  bool empty() const { return begin() == end(); }

  ComponentIdentTypeRepr *front() const { return *begin(); }
  ComponentIdentTypeRepr *back() const { return *(end()-1); }
};

inline IdentTypeRepr::ComponentRange IdentTypeRepr::getComponentRange() {
  return ComponentRange(this);
}

/// \brief A function type.
/// \code
///   Foo -> Bar
/// \endcode
class FunctionTypeRepr : public TypeRepr {
  GenericParamList *Generics;
  TypeRepr *ArgsTy;
  TypeRepr *RetTy;
  SourceLoc ArrowLoc;
  SourceLoc ThrowsLoc;

public:
  FunctionTypeRepr(GenericParamList *generics, TypeRepr *argsTy,
                   SourceLoc throwsLoc, SourceLoc arrowLoc, TypeRepr *retTy)
    : TypeRepr(TypeReprKind::Function),
      Generics(generics), ArgsTy(argsTy), RetTy(retTy),
      ArrowLoc(arrowLoc), ThrowsLoc(throwsLoc) {
  }

  GenericParamList *getGenericParams() const { return Generics; }
  TypeRepr *getArgsTypeRepr() const { return ArgsTy; }
  TypeRepr *getResultTypeRepr() const { return RetTy; }
  bool throws() const { return ThrowsLoc.isValid(); }

  SourceLoc getArrowLoc() const { return ArrowLoc; }
  SourceLoc getThrowsLoc() const { return ThrowsLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Function;
  }
  static bool classof(const FunctionTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return ArgsTy->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return RetTy->getEndLoc(); }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief An array type.
/// \code
///   [Foo]
/// \endcode
class ArrayTypeRepr : public TypeRepr {
  // FIXME: Tail allocation. Use bits to determine whether Base/Size are
  // availble.
  TypeRepr *Base;
  llvm::PointerIntPair<ExprHandle *, 1, bool> SizeAndOldSyntax;
  SourceRange Brackets;

public:
  ArrayTypeRepr(TypeRepr *Base, ExprHandle *Size, SourceRange Brackets,
                bool OldSyntax)
    : TypeRepr(TypeReprKind::Array), Base(Base),
      SizeAndOldSyntax(Size, OldSyntax), Brackets(Brackets) { }

  TypeRepr *getBase() const { return Base; }
  ExprHandle *getSize() const { return SizeAndOldSyntax.getPointer(); }
  SourceRange getBrackets() const { return Brackets; }

  bool usesOldSyntax() const { return SizeAndOldSyntax.getInt(); }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Array;
  }
  static bool classof(const ArrayTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const {
    if (usesOldSyntax())
      return Base->getStartLoc();

    return Brackets.Start;
  }
  SourceLoc getEndLocImpl() const {
    // This test is necessary because the type Int[4][2] is represented as
    // ArrayTypeRepr(ArrayTypeRepr(Int, 2), 4), so the range needs to cover both
    // sets of brackets.
    if (usesOldSyntax() && isa<ArrayTypeRepr>(Base))
      return Base->getEndLoc();
    return Brackets.End;
  }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A dictionary type.
/// \code
///   [K : V]
/// \endcode
class DictionaryTypeRepr : public TypeRepr {
  TypeRepr *Key;
  TypeRepr *Value;
  SourceLoc ColonLoc;
  SourceRange Brackets;

public:
  DictionaryTypeRepr(TypeRepr *key, TypeRepr *value,
                     SourceLoc colonLoc, SourceRange brackets)
    : TypeRepr(TypeReprKind::Dictionary), Key(key), Value(value),
      ColonLoc(colonLoc), Brackets(brackets) { }

  TypeRepr *getKey() const { return Key; }
  TypeRepr *getValue() const { return Value; }
  SourceRange getBrackets() const { return Brackets; }
  SourceLoc getColonLoc() const { return ColonLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Dictionary;
  }
  static bool classof(const DictionaryTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Brackets.Start; }
  SourceLoc getEndLocImpl() const { return Brackets.End; }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
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
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief An implicitly unwrapped optional type.
/// \code
///   Foo!
/// \endcode
class ImplicitlyUnwrappedOptionalTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc ExclamationLoc;

public:
  ImplicitlyUnwrappedOptionalTypeRepr(TypeRepr *Base, SourceLoc Exclamation)
    : TypeRepr(TypeReprKind::ImplicitlyUnwrappedOptional),
      Base(Base),
      ExclamationLoc(Exclamation) {}

  TypeRepr *getBase() const { return Base; }
  SourceLoc getExclamationLoc() const { return ExclamationLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional;
  }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return ExclamationLoc; }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A tuple type.
/// \code
///   (Foo, Bar)
/// \endcode
class TupleTypeRepr : public TypeRepr {
  ArrayRef<TypeRepr *> Elements;
  SourceRange Parens;
  // FIXME: Tail allocation.
  SourceLoc Ellipsis;

public:
  TupleTypeRepr(ArrayRef<TypeRepr *> Elements, SourceRange Parens,
                SourceLoc Ellipsis)
    : TypeRepr(TypeReprKind::Tuple), Elements(Elements),
      Parens(Parens), Ellipsis(Ellipsis) {
  }

  ArrayRef<TypeRepr *> getElements() const { return Elements; }
  TypeRepr *getElement(unsigned i) const { return Elements[i]; }
  SourceRange getParens() const { return Parens; }
  SourceLoc getEllipsisLoc() const { return Ellipsis; }
  bool hasEllipsis() const { return Ellipsis.isValid(); }

  void removeEllipsis() { Ellipsis = SourceLoc(); }

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
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
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

  bool hasName() const { return !Id.empty(); }
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
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A protocol composite type.
/// \code
///   protocol<Foo, Bar>
/// \endcode
class ProtocolCompositionTypeRepr : public TypeRepr {
  ArrayRef<IdentTypeRepr *> Protocols;
  SourceLoc ProtocolLoc;
  SourceRange AngleBrackets;

public:
  ProtocolCompositionTypeRepr(ArrayRef<IdentTypeRepr *> Protocols,
                              SourceLoc ProtocolLoc,
                              SourceRange AngleBrackets)
    : TypeRepr(TypeReprKind::ProtocolComposition), Protocols(Protocols),
      ProtocolLoc(ProtocolLoc), AngleBrackets(AngleBrackets) {
  }

  ArrayRef<IdentTypeRepr *> getProtocols() const { return Protocols; }
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
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A 'metatype' type.
/// \code
///   Foo.Type
/// \endcode
class MetatypeTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc MetaLoc;

public:
  MetatypeTypeRepr(TypeRepr *Base, SourceLoc MetaLoc)
    : TypeRepr(TypeReprKind::Metatype), Base(Base), MetaLoc(MetaLoc) {
  }

  TypeRepr *getBase() const { return Base; }
  SourceLoc getMetaLoc() const { return MetaLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Metatype;
  }
  static bool classof(const MetatypeTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return MetaLoc; }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

/// \brief A 'protocol' type.
/// \code
///   Foo.Protocol
/// \endcode
class ProtocolTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc ProtocolLoc;

public:
  ProtocolTypeRepr(TypeRepr *Base, SourceLoc ProtocolLoc)
    : TypeRepr(TypeReprKind::Protocol), Base(Base), ProtocolLoc(ProtocolLoc) {
  }

  TypeRepr *getBase() const { return Base; }
  SourceLoc getProtocolLoc() const { return ProtocolLoc; }

  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::Protocol;
  }
  static bool classof(const ProtocolTypeRepr *T) { return true; }

private:
  SourceLoc getStartLocImpl() const { return Base->getStartLoc(); }
  SourceLoc getEndLocImpl() const { return ProtocolLoc; }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};
  
/// \brief An 'inout' type.
/// \code
///   inout x : Int
/// \endcode
class InOutTypeRepr : public TypeRepr {
  TypeRepr *Base;
  SourceLoc InOutLoc;
  
public:
  InOutTypeRepr(TypeRepr *Base, SourceLoc InOutLoc)
  : TypeRepr(TypeReprKind::InOut), Base(Base), InOutLoc(InOutLoc) {
  }
  
  TypeRepr *getBase() const { return Base; }
  SourceLoc getInOutLoc() const { return InOutLoc; }
  
  static bool classof(const TypeRepr *T) {
    return T->getKind() == TypeReprKind::InOut;
  }
  static bool classof(const InOutTypeRepr *T) { return true; }
  
private:
  SourceLoc getStartLocImpl() const { return InOutLoc; }
  SourceLoc getEndLocImpl() const { return Base->getEndLoc(); }
  void printImpl(ASTPrinter &Printer, const PrintOptions &Opts) const;
  friend class TypeRepr;
};

  

inline bool TypeRepr::isSimple() const {
  switch (getKind()) {
  case TypeReprKind::Attributed:
  case TypeReprKind::Error:
  case TypeReprKind::Function:
  case TypeReprKind::InOut:
    return false;
  case TypeReprKind::SimpleIdent:
  case TypeReprKind::GenericIdent:
  case TypeReprKind::CompoundIdent:
  case TypeReprKind::Metatype:
  case TypeReprKind::Protocol:
  case TypeReprKind::Named:
  case TypeReprKind::Dictionary:
  case TypeReprKind::Optional:
  case TypeReprKind::ImplicitlyUnwrappedOptional:
  case TypeReprKind::ProtocolComposition:
  case TypeReprKind::Tuple:
    return true;

  case TypeReprKind::Array:
    return !cast<ArrayTypeRepr>(this)->usesOldSyntax();
  }
  llvm_unreachable("bad TypeRepr kind");
}

} // end namespace swift

namespace llvm {
  static inline raw_ostream &
  operator<<(raw_ostream &OS, swift::TypeRepr *TyR) {
    TyR->print(OS);
    return OS;
  }
} // end namespace llvm

#endif
