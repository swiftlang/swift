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

#include "swift/AST/Attr.h"
#include "swift/AST/CaptureInfo.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/KnownProtocols.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeLoc.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <cstddef>

namespace clang {
  class Decl;
  class MacroInfo;
}

namespace swift {
  class ArchetypeType;
  class ASTContext;
  class ASTWalker;
  class Type;
  class Expr;
  class LiteralExpr;
  class FuncDecl;
  class BraceStmt;
  class DeclAttributes;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class Module;
  class NameAliasType;
  class EnumElementDecl;
  class Pattern;
  struct PrintOptions;
  class ProtocolDecl;
  class ProtocolType;
  enum class Resilience : unsigned char;
  class TypeAliasDecl;
  class Stmt;
  class SubscriptDecl;
  class ValueDecl;
  class VarDecl;

  typedef llvm::PointerUnion<const clang::Decl *, clang::MacroInfo *> ClangNode;
  
enum class DeclKind : uint8_t {
#define DECL(Id, Parent) Id,
#define DECL_RANGE(Id, FirstId, LastId) \
  First_##Id##Decl = FirstId, Last_##Id##Decl = LastId,
#include "swift/AST/DeclNodes.def"
};

/// Keeps track of stage of circularity checking for the given protocol.
enum class CircularityCheck {
  /// Circularity has not yet been checked.
  Unchecked,
  /// We're currently checking circularity.
  Checking,
  /// Circularity has already been checked.
  Checked
};

/// Decl - Base class for all declarations in Swift.
class alignas(8) Decl {
  // alignas(8) because we use three tag bits on Decl*.
  
  class DeclBitfields {
    friend class Decl;
    unsigned Kind : 8;

    /// \brief Whether this declaration is invalid.
    unsigned Invalid : 1;

    /// \brief Whether this declaration was implicitly created, e.g.,
    /// an implicit constructor in a struct.
    unsigned Implicit : 1;

    /// \brief Whether this declaration was mapped directly from a Clang AST.
    ///
    /// Use getClangAST() to retrieve the corresponding Clang AST.
    unsigned FromClang : 1;
  };
  enum { NumDeclBits = 11 };
  static_assert(NumDeclBits <= 32, "fits in an unsigned");

  class ValueDeclBitfields {
    friend class ValueDecl;
    unsigned : NumDeclBits;
  };
  enum { NumValueDeclBits = NumDeclBits };
  static_assert(NumValueDeclBits <= 32, "fits in an unsigned");

  class AbstractFunctionDeclBitfields {
    friend class AbstractFunctionDecl;
    unsigned : NumValueDeclBits;

    /// \see AbstractFunctionDecl::BodyKind
    unsigned BodyKind : 2;

    /// \brief Whether this function was declared with a selector-style
    /// signature.
    unsigned HasSelectorStyleSignature : 1;
  };
  enum { NumAbstractFunctionDeclBits = NumValueDeclBits + 3 };
  static_assert(NumAbstractFunctionDeclBits <= 32, "fits in an unsigned");

  class FuncDeclBitfields {
    friend class FuncDecl;
    unsigned : NumAbstractFunctionDeclBits;

    /// \brief Whether this function is a 'static' method.
    unsigned Static : 1;

    /// \brief Number of parameter patterns (tuples).
    unsigned NumParamPatterns : 16;
  };
  enum { NumFuncDeclBits = NumAbstractFunctionDeclBits + 17 };
  static_assert(NumFuncDeclBits <= 32, "fits in an unsigned");

  class TypeDeclBitfields {
    friend class TypeDecl;
    unsigned : NumValueDeclBits;


    /// Whether we have already checked the inheritance clause.
    ///
    /// FIXME: Is this too fine-grained?
    unsigned CheckedInheritanceClause : 1;
  };

  enum { NumTypeDeclBits = NumValueDeclBits + 1 };
  static_assert(NumTypeDeclBits <= 32, "fits in an unsigned");

  enum { NumNominalTypeDeclBits = NumTypeDeclBits};
  static_assert(NumNominalTypeDeclBits <= 32, "fits in an unsigned");

  class ProtocolDeclBitfields {
    friend class ProtocolDecl;
    unsigned : NumNominalTypeDeclBits;

    /// Whether the \c RequiresClass bit is valid.
    unsigned RequiresClassValid : 1;

    /// Whether this is a [class_bounded] protocol.
    unsigned RequiresClass : 1;

    /// Whether the \c ExistentialConformsToSelf bit is valid.
    unsigned ExistentialConformsToSelfValid : 1;

    /// Whether the existential of this protocol conforms to itself.
    unsigned ExistentialConformsToSelf : 1;

    /// If this is a compiler-known protocol, this will be a KnownProtocolKind
    /// value, plus one. Otherwise, it will be 0.
    unsigned KnownProtocol : 5;

    /// The stage of the circularity check for this protocol.
    unsigned Circularity : 2;
  };
  enum { NumProtocolDeclBits = NumNominalTypeDeclBits + 11 };
  static_assert(NumProtocolDeclBits <= 32, "fits in an unsigned");

  class ClassDeclBitfields {
    friend class ClassDecl;
    unsigned : NumNominalTypeDeclBits;

    /// The stage of the inheritance circularity check for this class.
    unsigned Circularity : 2;
  };
  enum { NumClassDeclBits = NumNominalTypeDeclBits + 2 };
  static_assert(NumClassDeclBits <= 32, "fits in an unsigned");

  class EnumDeclBitfields {
    friend class EnumDecl;
    unsigned : NumNominalTypeDeclBits;
    
    /// The stage of the raw type circularity check for this class.
    unsigned Circularity : 2;
  };
  enum { NumEnumDeclBits = NumNominalTypeDeclBits + 2 };
  static_assert(NumEnumDeclBits <= 32, "fits in an unsigned");
  
  class InfixOperatorDeclBitfields {
    friend class InfixOperatorDecl;
    unsigned : NumDeclBits;

    unsigned Associativity : 2;
    unsigned Precedence : 8;
  };
  enum { NumInfixOperatorDeclBits = NumDeclBits + 10 };
  static_assert(NumInfixOperatorDeclBits <= 32, "fits in an unsigned");

  class ImportDeclBitfields {
    friend class ImportDecl;
    unsigned : NumDeclBits;

    unsigned ImportKind : 3;
    unsigned IsExported : 1;
  };
  enum { NumImportDeclBits = NumDeclBits + 4 };
  static_assert(NumImportDeclBits <= 32, "fits in an unsigned");

  class ExtensionDeclBitfields {
    friend class ExtensionDecl;
    unsigned : NumDeclBits;

    /// Whether we have already checked the inheritance clause.
    ///
    /// FIXME: Is this too fine-grained?
    unsigned CheckedInheritanceClause : 1;
  };
  enum { NumExtensionDeclBits = NumDeclBits + 4 };
  static_assert(NumExtensionDeclBits <= 32, "fits in an unsigned");

protected:
  union {
    DeclBitfields DeclBits;
    ValueDeclBitfields ValueDeclBits;
    AbstractFunctionDeclBitfields AbstractFunctionDeclBits;
    FuncDeclBitfields FuncDeclBits;
    TypeDeclBitfields TypeDeclBits;
    ProtocolDeclBitfields ProtocolDeclBits;
    ClassDeclBitfields ClassDeclBits;
    EnumDeclBitfields EnumDeclBits;
    InfixOperatorDeclBitfields InfixOperatorDeclBits;
    ImportDeclBitfields ImportDeclBits;
    ExtensionDeclBitfields ExtensionDeclBits;
  };

private:
  DeclContext *Context;

  Decl(const Decl&) = delete;
  void operator=(const Decl&) = delete;

protected:
  // Storage for the declaration attributes.
  llvm::PointerIntPair<const DeclAttributes *, 1, bool> AttrsAndIsObjC;
  static const DeclAttributes EmptyAttrs;

  Decl(DeclKind kind, DeclContext *DC)
    : Context(DC),
      AttrsAndIsObjC(&EmptyAttrs, false) {
    DeclBits.Kind = unsigned(kind);
    DeclBits.Invalid = false;
    DeclBits.Implicit = false;
    DeclBits.FromClang = false;
  }

  ClangNode getClangNodeSlow();

public:
  DeclKind getKind() const { return DeclKind(DeclBits.Kind); }

  /// \brief Retrieve the name of the given declaration kind.
  ///
  /// This name should only be used for debugging dumps and other
  /// developer aids, and should never be part of a diagnostic or exposed
  /// to the user of the compiler in any way.
  static StringRef getKindName(DeclKind K);

  DeclContext *getDeclContext() const { return Context; }
  void setDeclContext(DeclContext *DC) { Context = DC; }

  /// Retrieve the innermost declaration context corresponding to this
  /// declaration, which will either be the declaration itself (if it's
  /// also a declaration context) or its declaration context.
  DeclContext *getInnermostDeclContext();

  /// \brief Retrieve the module in which this declaration resides.
  Module *getModuleContext() const;

  /// getASTContext - Return the ASTContext that this decl lives in.
  ASTContext &getASTContext() const {
    assert(Context && "Decl doesn't have an assigned context");
    return Context->getASTContext();
  }

  DeclAttributes &getMutableAttrs();

  const DeclAttributes &getAttrs() const {
    return *AttrsAndIsObjC.getPointer();
  }

  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }
  SourceLoc getLoc() const;
  SourceRange getSourceRange() const;

  SourceLoc TrailingSemiLoc;

  void dump() const;
  void dump(unsigned Indent) const;

  /// \brief Pretty-print the given declaration.
  ///
  /// \param OS Output stream to which the declaration will be printed.
  void print(raw_ostream &OS) const;

  /// \brief Pretty-print the given declaration.
  ///
  /// \param os Output stream to which the declaration will be printed.
  ///
  /// \param options Options to control how pretty-printing is performed.
  ///
  /// \param declOffsets If non-null, will be populated with the stream offsets
  /// at which each declaration encountered is printed.
  void print(raw_ostream &os, const PrintOptions &options,
             SmallVectorImpl<std::pair<Decl *, uint64_t>> *declOffsets
               = nullptr) const;

  /// \brief Determine whether this declaration should be printed when
  /// encountered in its declaration context's list of members.
  bool shouldPrintInContext() const;

  bool walk(ASTWalker &walker);

  /// \brief Should this declaration be treated as if annotated with transparent
  /// attribute.
  bool isTransparent() const;

  /// \brief Return whether this declaration has been determined invalid.
  bool isInvalid() const { return DeclBits.Invalid; }
  
  /// \brief Mark this declaration invalid.
  void setInvalid() { DeclBits.Invalid = true; }

  /// \brief Determine whether this declaration was implicitly generated by the
  /// compiler (rather than explicitly written in source code).
  bool isImplicit() const { return DeclBits.Implicit; }

  /// \brief Mark this declaration as implicit.
  void setImplicit() { DeclBits.Implicit = true; }

  /// \brief Returns true if there is a Clang AST node associated
  /// with self.
  bool hasClangNode() const {
    return DeclBits.FromClang;
  }

  /// \brief Retrieve the Clang AST node from which this declaration was
  /// synthesized, if any.
  ClangNode getClangNode() {
    if (!DeclBits.FromClang)
      return ClangNode();

    return getClangNodeSlow();
  }

  /// \brief Retrieve the Clang declaration from which this declaration was
  /// synthesized, if any.
  const clang::Decl *getClangDecl() {
    if (!DeclBits.FromClang)
      return nullptr;

    return getClangNodeSlow().dyn_cast<const clang::Decl *>();
  }

  /// \brief Retrieve the Clang macro from which this declaration was
  /// synthesized, if any.
  clang::MacroInfo *getClangMacro() {
    if (!DeclBits.FromClang)
      return nullptr;

    return getClangNodeSlow().dyn_cast<clang::MacroInfo *>();
  }

  /// \brief Set the Clang node associated with this declaration.
  void setClangNode(ClangNode node);

  // Make vanilla new/delete illegal for Decls.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  // Only allow allocation of Decls using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(Decl));
  void *operator new(size_t Bytes, void *Mem) { 
    assert(Mem); 
    return Mem; 
  }
};

/// GenericParam - A parameter to a generic function or type, as declared in
/// the list of generic parameters, e.g., the T and U in:
///
/// \code
/// func f<T : Range, U>(t : T, u : U) { /* ... */ }
/// \endcode
class GenericParam {
  GenericTypeParamDecl *TypeParam;

public:
  /// Construct a generic parameter from a type parameter.
  GenericParam(GenericTypeParamDecl *TypeParam) : TypeParam(TypeParam) { }

  /// getDecl - Retrieve the generic parameter declaration.
  ValueDecl *getDecl() const {
    return reinterpret_cast<ValueDecl *>(TypeParam);
  }

  /// getAsTypeParam - Retrieve the generic parameter as a type parameter.
  GenericTypeParamDecl *getAsTypeParam() const { return TypeParam; }

  /// setDeclContext - Set the declaration context for the generic parameter,
  /// once it is known.
  void setDeclContext(DeclContext *DC);
};

/// \brief A single requirement in a 'where' clause, which places additional
/// restrictions on the generic parameters or associated types of a generic
/// function, type, or protocol.
///
/// This always represents a requirement spelled in the source code.  It is
/// never generated implicitly.
class RequirementRepr {
  SourceLoc SeparatorLoc;
  RequirementKind Kind : 1;
  bool Invalid : 1;
  TypeLoc Types[2];

  RequirementRepr(SourceLoc SeparatorLoc, RequirementKind Kind, TypeLoc FirstType,
              TypeLoc SecondType)
    : SeparatorLoc(SeparatorLoc), Kind(Kind), Invalid(false),
      Types{FirstType, SecondType} { }
  
public:
  /// \brief Construct a new conformance requirement.
  ///
  /// \param Subject The type that must conform to the given protocol or
  /// composition, or be a subclass of the given class type.
  /// \param ColonLoc The location of the ':', or an invalid location if
  /// this requirement was implied.
  /// \param Constraint The protocol or protocol composition to which the
  /// subject must conform, or superclass from which the subject must inherit.
  static RequirementRepr getConformance(TypeLoc Subject,
                                    SourceLoc ColonLoc,
                                    TypeLoc Constraint) {
    return { ColonLoc, RequirementKind::Conformance, Subject, Constraint };
  }

  /// \brief Construct a new same-type requirement.
  ///
  /// \param FirstType The first type.
  /// \param EqualLoc The location of the '==' in the same-type constraint, or
  /// an invalid location if this requirement was implied.
  /// \param SecondType The second type.
  static RequirementRepr getSameType(TypeLoc FirstType,
                                 SourceLoc EqualLoc,
                                 TypeLoc SecondType) {
    return { EqualLoc, RequirementKind::SameType, FirstType, SecondType };
  }

  /// \brief Determine the kind of requirement
  RequirementKind getKind() const { return Kind; }

  /// \brief Determine whether this requirement is invalid.
  bool isInvalid() const { return Invalid; }

  /// \brief Mark this requirement invalid.
  void setInvalid() { Invalid = true; }

  /// \brief For a conformance requirement, return the subject of the
  /// conformance relationship.
  Type getSubject() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0].getType();
  }

  TypeRepr *getSubjectRepr() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0].getTypeRepr();
  }

  TypeLoc &getSubjectLoc() {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0];
  }

  const TypeLoc &getSubjectLoc() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[0];
  }

  /// \brief For a conformance requirement, return the protocol or to which
  /// the subject conforms or superclass it inherits.
  Type getConstraint() const {
    assert(getKind() == RequirementKind::Conformance);
    return Types[1].getType();
  }

  TypeLoc &getConstraintLoc() {
    assert(getKind() == RequirementKind::Conformance);
    return Types[1];
  }

  /// \brief Retrieve the location of the ':' in an explicitly-written
  /// conformance requirement.
  SourceLoc getColonLoc() const {
    assert(getKind() == RequirementKind::Conformance);
    return SeparatorLoc;
  }

  /// \brief Retrieve the first type of a same-type requirement.
  Type getFirstType() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0].getType();
  }

  TypeRepr *getFirstTypeRepr() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0].getTypeRepr();
  }

  TypeLoc &getFirstTypeLoc() {
    assert(getKind() == RequirementKind::SameType);
    return Types[0];
  }

  const TypeLoc &getFirstTypeLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[0];
  }

  /// \brief Retrieve the second type of a same-type requirement.
  Type getSecondType() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1].getType();
  }

  TypeRepr *getSecondTypeRepr() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1].getTypeRepr();
  }

  TypeLoc &getSecondTypeLoc() {
    assert(getKind() == RequirementKind::SameType);
    return Types[1];
  }

  const TypeLoc &getSecondTypeLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return Types[1];
  }

  /// \brief Retrieve the location of the '==' in an explicitly-written
  /// same-type requirement.
  SourceLoc getEqualLoc() const {
    assert(getKind() == RequirementKind::SameType);
    return SeparatorLoc;
  }
};

/// GenericParamList - A list of generic parameters that is part of a generic
/// function or type, along with extra requirements placed on those generic
/// parameters and types derived from them.
class GenericParamList {
  SourceRange Brackets;
  unsigned NumParams;
  SourceLoc WhereLoc;
  MutableArrayRef<RequirementRepr> Requirements;
  ArrayRef<ArchetypeType *> AllArchetypes;

  GenericParamList *OuterParameters;

  GenericParamList(SourceLoc LAngleLoc,
                   ArrayRef<GenericParam> Params,
                   SourceLoc WhereLoc,
                   MutableArrayRef<RequirementRepr> Requirements,
                   SourceLoc RAngleLoc);

public:
  /// create - Create a new generic parameter list within the given AST context.
  ///
  /// \param Context The ASTContext in which the generic parameter list will
  /// be allocated.
  /// \param LAngleLoc The location of the opening angle bracket ('<')
  /// \param Params The list of generic parameters, which will be copied into
  /// ASTContext-allocated memory.
  /// \param RAngleLoc The location of the closing angle bracket ('>')
  static GenericParamList *create(ASTContext &Context,
                                  SourceLoc LAngleLoc,
                                  ArrayRef<GenericParam> Params,
                                  SourceLoc RAngleLoc);

  /// create - Create a new generic parameter list and "where" clause within
  /// the given AST context.
  ///
  /// \param Context The ASTContext in which the generic parameter list will
  /// be allocated.
  /// \param LAngleLoc The location of the opening angle bracket ('<')
  /// \param Params The list of generic parameters, which will be copied into
  /// ASTContext-allocated memory.
  /// \param WhereLoc The location of the 'where' keyword, if any.
  /// \param Requirements The list of requirements, which will be copied into
  /// ASTContext-allocated memory.
  /// \param RAngleLoc The location of the closing angle bracket ('>')
  static GenericParamList *create(const ASTContext &Context,
                                  SourceLoc LAngleLoc,
                                  ArrayRef<GenericParam> Params,
                                  SourceLoc WhereLoc,
                                  MutableArrayRef<RequirementRepr> Requirements,
                                  SourceLoc RAngleLoc);

  MutableArrayRef<GenericParam> getParams() {
    return MutableArrayRef<GenericParam>(
             reinterpret_cast<GenericParam *>(this + 1), NumParams);
  }

  ArrayRef<GenericParam> getParams() const {
    return ArrayRef<GenericParam>(
             reinterpret_cast<const GenericParam *>(this + 1), NumParams);
  }

  unsigned size() const { return NumParams; }
  GenericParam *begin() { return getParams().begin(); }
  GenericParam *end()   { return getParams().end(); }
  const GenericParam *begin() const { return getParams().begin(); }
  const GenericParam *end()   const { return getParams().end(); }

  /// \brief Retrieve the location of the 'where' keyword, or an invalid
  /// location if 'where' was not present.
  SourceLoc getWhereLoc() const { return WhereLoc; }

  /// \brief Retrieve the set of additional requirements placed on these
  /// generic parameters and types derived from them.
  ///
  /// This list may contain both explicitly-written requirements as well as
  /// implicitly-generated requirements, and may be non-empty even if no
  /// 'where' keyword is present.
  MutableArrayRef<RequirementRepr> getRequirements() { return Requirements; }

  /// \brief Retrieve the set of additional requirements placed on these
  /// generic parameters and types derived from them.
  ///
  /// This list may contain both explicitly-written requirements as well as
  /// implicitly-generated requirements, and may be non-empty even if no
  /// 'where' keyword is present.
  ArrayRef<RequirementRepr> getRequirements() const { return Requirements; }

  /// \brief Override the set of requirements associated with this generic
  /// parameter list.
  ///
  /// \param NewRequirements The new set of requirements, which is expected
  /// to be a superset of the existing set of requirements (although this
  /// property is not checked here). It is assumed that the array reference
  /// refers to ASTContext-allocated memory.
  void overrideRequirements(MutableArrayRef<RequirementRepr> NewRequirements) {
    Requirements = NewRequirements;
  }

  /// \brief Retrieves the list containing all archetypes described by this
  /// generic parameter clause.
  ///
  /// In this list of archetypes, the primary archetypes come first followed by
  /// any non-primary archetypes (i.e., those archetypes that encode associated
  /// types of another archetype).
  ArrayRef<ArchetypeType *> getAllArchetypes() const { return AllArchetypes; }

  /// \brief Retrieves the list containing only the primary archetypes described
  /// by this generic parameter clause. This excludes archetypes for associated
  /// types of the primary archetypes.
  ArrayRef<ArchetypeType *> getPrimaryArchetypes() const {
    return getAllArchetypes().slice(0, size());
  }
  
  /// \brief Retrieves the list containing only the associated archetypes.
  ArrayRef<ArchetypeType *> getAssociatedArchetypes() const {
    return getAllArchetypes().slice(size());
  }

  /// \brief Sets all archetypes *without* copying the source array.
  void setAllArchetypes(ArrayRef<ArchetypeType *> AA) {
    AllArchetypes = AA;
  }

  /// \brief Retrieve the outer generic parameter list, which provides the
  /// generic parameters of the context in which this generic parameter list
  /// exists.
  ///
  /// Consider the following generic class:
  ///
  /// \code
  /// class Vector<T> {
  ///   constructor<R : Range where R.Element == T>(range : R) { }
  /// }
  /// \endcode
  ///
  /// The generic parameter list <T> has no outer parameters, because it is
  /// the outermost generic parameter list. The generic parameter list
  /// <R : Range...> for the constructor has the generic parameter list <T> as
  /// its outer generic parameter list.
  GenericParamList *getOuterParameters() const { return OuterParameters; }

  /// \brief Set the outer generic parameter list. See \c getOuterParameters
  /// for more information.
  void setOuterParameters(GenericParamList *Outer) { OuterParameters = Outer; }

  SourceRange getSourceRange() const { return Brackets; }

  /// Retrieve the depth of this generic parameter list.
  unsigned getDepth() const {
    unsigned depth = 0;
    for (auto gp = getOuterParameters(); gp; gp = gp->getOuterParameters())
      ++depth;
    return depth;
  }
};

/// Describes what kind of name is being imported.
///
/// If the enumerators here are changed, make sure to update all diagnostics
/// using ImportKind as a select index.
enum class ImportKind : uint8_t {
  Module = 0,
  Type,
  Struct,
  Class,
  Enum,
  Protocol,
  Var,
  Func
};

/// ImportDecl - This represents a single import declaration, e.g.:
///   import swift
///   import typealias swift.Int
class ImportDecl : public Decl {
public:
  typedef std::pair<Identifier, SourceLoc> AccessPathElement;

private:
  SourceLoc ImportLoc;
  SourceLoc KindLoc;

  /// The number of elements in this path.
  unsigned NumPathElements;

  AccessPathElement *getPathBuffer() {
    return reinterpret_cast<AccessPathElement*>(this+1);
  }
  const AccessPathElement *getPathBuffer() const {
    return reinterpret_cast<const AccessPathElement*>(this+1);
  }
  
  ImportDecl(DeclContext *DC, SourceLoc ImportLoc, ImportKind K,
             SourceLoc KindLoc, bool Exported,
             ArrayRef<AccessPathElement> Path);

public:
  static ImportDecl *create(ASTContext &C, DeclContext *DC,
                            SourceLoc ImportLoc, ImportKind Kind,
                            SourceLoc KindLoc, bool Exported,
                            ArrayRef<AccessPathElement> Path);

  ArrayRef<AccessPathElement> getFullAccessPath() const {
    return ArrayRef<AccessPathElement>(getPathBuffer(), NumPathElements);
  }

  ArrayRef<AccessPathElement> getModulePath() const {
    auto result = getFullAccessPath();
    if (getImportKind() != ImportKind::Module)
      result = result.slice(0, result.size()-1);
    return result;
  }

  ArrayRef<AccessPathElement> getDeclPath() const {
    if (getImportKind() == ImportKind::Module)
      return {};
    return getFullAccessPath().back();
  }

  ImportKind getImportKind() const {
    return static_cast<ImportKind>(ImportDeclBits.ImportKind);
  }

  bool isExported() const {
    return ImportDeclBits.IsExported;
  }

  SourceLoc getStartLoc() const { return ImportLoc; }
  SourceLoc getLoc() const { return getFullAccessPath().front().second; }
  SourceRange getSourceRange() const {
    return SourceRange(ImportLoc, getFullAccessPath().back().second);
  }
  SourceLoc getKindLoc() const { return KindLoc; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Import;
  }
};

/// ExtensionDecl - This represents a type extension containing methods
/// associated with the type.  This is not a ValueDecl and has no Type because
/// there are no runtime values of the Extension's type.  
class ExtensionDecl : public Decl, public DeclContext {
  SourceLoc ExtensionLoc;  // Location of 'extension' keyword.
  SourceRange Braces;

  /// ExtendedType - The type being extended.
  TypeLoc ExtendedType;
  MutableArrayRef<TypeLoc> Inherited;
  ArrayRef<Decl*> Members;

  /// \brief The set of protocols to which this extension conforms.
  ArrayRef<ProtocolDecl *> Protocols;
  
  /// \brief The set of protocol conformance mappings. The element order
  /// corresponds to the order of Protocols.
  ArrayRef<ProtocolConformance *> Conformances;

  /// \brief The next extension in the linked list of extensions.
  ///
  /// The bit indicates whether this extension has been resolved to refer to
  /// a known nominal type.
  llvm::PointerIntPair<ExtensionDecl *, 1, bool> NextExtension
    = {nullptr, false};

  friend class ExtensionIterator;
  friend class NominalTypeDecl;
  friend class MemberLookupTable;

public:
  using Decl::getASTContext;
  
  ExtensionDecl(SourceLoc ExtensionLoc, TypeLoc ExtendedType,
                MutableArrayRef<TypeLoc> Inherited,
                DeclContext *Parent)
    : Decl(DeclKind::Extension, Parent),
      DeclContext(DeclContextKind::ExtensionDecl, Parent),
      ExtensionLoc(ExtensionLoc),
      ExtendedType(ExtendedType), Inherited(Inherited)
  {
    ExtensionDeclBits.CheckedInheritanceClause = false;
  }
  
  SourceLoc getStartLoc() const { return ExtensionLoc; }
  SourceLoc getLoc() const { return ExtensionLoc; }
  SourceRange getSourceRange() const {
    return { ExtensionLoc, Braces.End };
  }

  SourceRange getBraces() const { return Braces; }

  Type getExtendedType() const { return ExtendedType.getType(); }
  TypeLoc &getExtendedTypeLoc() { return ExtendedType; }

  /// \brief Retrieve the set of protocols that this type inherits (i.e,
  /// explicitly conforms to).
  MutableArrayRef<TypeLoc> getInherited() { return Inherited; }
  ArrayRef<TypeLoc> getInherited() const { return Inherited; }

  /// Whether we already type-checked the inheritance clause.
  bool checkedInheritanceClause() const {
    return ExtensionDeclBits.CheckedInheritanceClause;
  }

  /// Note that we have already type-checked the inheritance clause.
  void setCheckedInheritanceClause(bool checked = true) {
    ExtensionDeclBits.CheckedInheritanceClause = checked;
  }

  /// \brief Retrieve the set of protocols to which this extension conforms.
  ArrayRef<ProtocolDecl *> getProtocols() const { return Protocols; }

  void setProtocols(ArrayRef<ProtocolDecl *> protocols) {
    Protocols = protocols;
  }

  /// \brief Retrieve the set of protocol conformance mappings for this type.
  ///
  /// Calculated during type-checking.
  ArrayRef<ProtocolConformance *> getConformances() const {
    return Conformances;
  }
  
  void setConformances(ArrayRef<ProtocolConformance *> c) {
    Conformances = c;
  }

  ArrayRef<Decl*> getMembers() const { return Members; }
  void setMembers(ArrayRef<Decl*> M, SourceRange B);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Extension;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::ExtensionDecl;
  }
  
  using DeclContext::operator new;
};

/// \brief Iterator that walks the extensions of a particular type.
class ExtensionIterator {
  ExtensionDecl *current;

public:
  ExtensionIterator() : current() { }
  explicit ExtensionIterator(ExtensionDecl *current) : current(current) { }

  ExtensionDecl *operator*() const { return current; }
  ExtensionDecl *operator->() const { return current; }

  ExtensionIterator &operator++() {
    current = current->NextExtension.getPointer();
    return *this;
  }

  ExtensionIterator operator++(int) {
    ExtensionIterator tmp = *this;
    ++(*this);
    return tmp;
  }

  friend bool operator==(ExtensionIterator x, ExtensionIterator y) {
    return x.current == y.current;
  }

  friend bool operator!=(ExtensionIterator x, ExtensionIterator y) {
    return x.current != y.current;
  }
};

/// \brief Range that covers a set of extensions.
class ExtensionRange {
  ExtensionIterator first;
  ExtensionIterator last;

public:

  ExtensionRange(ExtensionIterator first, ExtensionIterator last)
    : first(first), last(last) { }

  typedef ExtensionIterator iterator;
  iterator begin() const { return first; }
  iterator end() const { return last; }
};

// PatternBindingDecl - This decl contains a pattern and optional initializer
// for a set of one or more VarDecls declared together.  (For example, in
// "var (a,b) = foo()", this contains the pattern "(a,b)" and the intializer
// "foo()".  The same applies to simpler declarations like "var a = foo()".)
class PatternBindingDecl : public Decl {
  SourceLoc VarLoc; // Location of the 'var' keyword
  Pattern *Pat; // The pattern which this decl binds
  Expr *Init; // Initializer for the variables

  friend class Decl;
  
public:
  PatternBindingDecl(SourceLoc VarLoc, Pattern *Pat, Expr *E,
                     DeclContext *Parent)
    : Decl(DeclKind::PatternBinding, Parent), VarLoc(VarLoc), Pat(Pat),
      Init(E) {
  }

  SourceLoc getStartLoc() const { return VarLoc; }
  SourceLoc getLoc() const { return VarLoc; }
  SourceRange getSourceRange() const;

  Pattern *getPattern() { return Pat; }
  const Pattern *getPattern() const { return Pat; }
  void setPattern(Pattern *P) { Pat = P; }

  bool hasInit() const { return Init; }
  Expr *getInit() const { return Init; }
  void setInit(Expr *E) { Init = E; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PatternBinding;
  }

};

/// TopLevelCodeDecl - This decl is used as a container for top-level
/// expressions and statements in the main module.  It is always a direct
/// child of the body of a TranslationUnit.  The primary reason for
/// building these is to give top-level statements a DeclContext which is
/// distinct from the TranslationUnit itself.  This, among other things,
/// makes it easier to distinguish between local top-level variables (which
/// are not live past the end of the statement) and global variables.
class TopLevelCodeDecl : public Decl, public DeclContext {
  BraceStmt *Body;

public:
  TopLevelCodeDecl(DeclContext *Parent, BraceStmt *Body = nullptr)
    : Decl(DeclKind::TopLevelCode, Parent),
      DeclContext(DeclContextKind::TopLevelCodeDecl, Parent),
      Body(Body) {}

  BraceStmt *getBody() const { return Body; }
  void setBody(BraceStmt *b) { Body = b; }

  SourceLoc getStartLoc() const;
  SourceLoc getLoc() const { return getStartLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TopLevelCode;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::TopLevelCodeDecl;
  }
  
  using DeclContext::operator new;
};

/// ValueDecl - All named decls that are values in the language.  These can
/// have a type, etc.
class ValueDecl : public Decl {
  Identifier Name;
  SourceLoc NameLoc;
  Type Ty;

protected:
  ValueDecl(DeclKind K, DeclContext *DC, Identifier name, SourceLoc NameLoc)
    : Decl(K, DC), Name(name), NameLoc(NameLoc) { }

public:
  /// \brief Return true if this is a definition of a decl, not a forward
  /// declaration (e.g. of a function) that is implemented outside of the
  /// swift code.
  bool isDefinition() const;
  
  Identifier getName() const { return Name; }
  bool isOperator() const { return Name.isOperator(); }

  SourceLoc getNameLoc() const { return NameLoc; }
  SourceLoc getLoc() const { return NameLoc; }

  bool hasType() const { return !Ty.isNull(); }
  Type getType() const {
    assert(!Ty.isNull() && "declaration has no type set yet");
    return Ty;
  }

  /// Set the type of this declaration for the first time.
  void setType(Type T);

  /// Overwrite the type of this declaration.
  void overwriteType(Type T);

  /// isReferencedAsLValue - Returns 'true' if references to this
  /// declaration are l-values.
  bool isReferencedAsLValue() const {
    return getKind() == DeclKind::Var;
  }

  /// isSettable - Determine whether references to this decl may appear
  /// on the left-hand side of an assignment or as the operand of a
  /// `&` or [assignment] operator.
  bool isSettable() const;

  /// Determine whether references to this decl are settable in the
  /// above sense when used on a base of the given type (which may be
  /// null to indicate that there is no base).
  bool isSettableOnBase(Type baseType) const;
  
  /// isInstanceMember - Determine whether this value is an instance member
  /// of an enum or protocol.
  bool isInstanceMember() const;

  /// needsCapture - Check whether referring to this decl from a nested
  /// function requires capturing it.
  bool needsCapture() const;

  /// Retrieve the declaration that this declaration overrides, if any.
  ValueDecl *getOverriddenDecl() const;

  /// isObjC - Returns true if the decl requires Objective-C interop.
  bool isObjC() const { return AttrsAndIsObjC.getInt(); }
  
  void setIsObjC(bool value) {
    AttrsAndIsObjC = {AttrsAndIsObjC.getPointer(), value};
  }
  
  /// Returns true if this decl can be found by id-style dynamic lookup.
  bool canBeAccessedByDynamicLookup() const;

  /// Dump a reference to the given declaration.
  void dumpRef(raw_ostream &os) const;

  /// Dump a reference to the given declaration.
  void dumpRef() const;

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_ValueDecl &&
           D->getKind() <= DeclKind::Last_ValueDecl;
  }
};

/// This is a common base class for declarations which declare a type.
class TypeDecl : public ValueDecl {
  MutableArrayRef<TypeLoc> Inherited;

  /// \brief The set of protocols to which this type conforms.
  ArrayRef<ProtocolDecl *> Protocols;
  
  /// \brief The set of protocol conformance mappings. The element order
  /// corresponds to the order of Protocols.
  ArrayRef<ProtocolConformance *> Conformances;

protected:
  TypeDecl(DeclKind K, DeclContext *DC, Identifier name, SourceLoc NameLoc,
           MutableArrayRef<TypeLoc> inherited) :
    ValueDecl(K, DC, name, NameLoc), Inherited(inherited)
  {
    TypeDeclBits.CheckedInheritanceClause = false;
  }

public:
  Type getDeclaredType() const;

  /// \brief Retrieve the set of protocols that this type inherits (i.e,
  /// explicitly conforms to).
  MutableArrayRef<TypeLoc> getInherited() { return Inherited; }
  ArrayRef<TypeLoc> getInherited() const { return Inherited; }

  /// Whether we already type-checked the inheritance clause.
  bool checkedInheritanceClause() const {
    return TypeDeclBits.CheckedInheritanceClause;
  }

  /// Note that we have already type-checked the inheritance clause.
  void setCheckedInheritanceClause(bool checked = true) {
    TypeDeclBits.CheckedInheritanceClause = checked;
  }

  /// \brief Retrieve the set of protocols to which this type conforms.
  ///
  /// FIXME: Include protocol conformance from extensions? This will require
  /// semantic analysis to compute.
  ArrayRef<ProtocolDecl *> getProtocols() const { return Protocols; }

  void setProtocols(ArrayRef<ProtocolDecl *> protocols) {
    Protocols = protocols;
  }
  
  /// \brief True if the type can implicitly derive a conformance for the given
  /// protocol.
  ///
  /// If true, explicit conformance checking will synthesize implicit
  /// declarations for requirements of the protocol that are not satisfied by
  /// the type's explicit members.
  bool derivesProtocolConformance(ProtocolDecl *protocol) const;

  /// \brief Retrieve the set of protocol conformance mappings for this type.
  ///
  /// Calculated during type-checking.
  ArrayRef<ProtocolConformance *> getConformances() const {
    return Conformances;
  }
  
  void setConformances(ArrayRef<ProtocolConformance *> c) {
    Conformances = c;
  }

  void setInherited(MutableArrayRef<TypeLoc> i) { Inherited = i; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_TypeDecl &&
           D->getKind() <= DeclKind::Last_TypeDecl;
  }
};

/// TypeAliasDecl - This is a declaration of a typealias, for example:
///
///    typealias foo = int
///
/// TypeAliasDecl's always have 'MetaTypeType' type.
///
class TypeAliasDecl : public TypeDecl {
  /// The type that represents this (sugared) name alias.
  mutable NameAliasType *AliasTy;

  SourceLoc TypeAliasLoc; // The location of the 'typalias' keyword
  TypeLoc UnderlyingTy;

public:
  TypeAliasDecl(SourceLoc TypeAliasLoc, Identifier Name,
                SourceLoc NameLoc, TypeLoc UnderlyingTy,
                DeclContext *DC, MutableArrayRef<TypeLoc> Inherited);

  SourceLoc getStartLoc() const { return TypeAliasLoc; }
  SourceRange getSourceRange() const;

  /// getUnderlyingType - Returns the underlying type, which is
  /// assumed to have been set.
  Type getUnderlyingType() const {
    assert(!UnderlyingTy.getType().isNull() &&
           "getting invalid underlying type");
    return UnderlyingTy.getType();
  }

  /// \brief Determine whether this type alias has an underlying type.
  bool hasUnderlyingType() const { return !UnderlyingTy.getType().isNull(); }

  TypeLoc &getUnderlyingTypeLoc() { return UnderlyingTy; }

  /// getAliasType - Return the sugared version of this decl as a Type.
  NameAliasType *getAliasType() const { return AliasTy; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::TypeAlias;
  }
};

/// Abstract class describing generic type parameters and associated types,
/// whose common purpose is to anchor the abstract type parameter and specify
/// requirements for any corresponding type argument.
class AbstractTypeParamDecl : public TypeDecl {
  /// The superclass of the generic parameter.
  Type SuperclassTy;

  /// The archetype describing this abstract type parameter within its scope.
  ArchetypeType *Archetype;

protected:
  AbstractTypeParamDecl(DeclKind kind, DeclContext *dc, Identifier name,
                        SourceLoc NameLoc)
    : TypeDecl(kind, dc, name, NameLoc, { }), Archetype(nullptr) { }

public:
  /// Return the superclass of the generic parameter.
  Type getSuperclass() const {
    return SuperclassTy;
  }

  /// Set the superclass of the generic parameter.
  void setSuperclass(Type superclassTy) {
    SuperclassTy = superclassTy;
  }

  /// Retrieve the archetype that describes this abstract type parameter
  /// within its scope.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// Set the archetype used to describe this abstract type parameter within
  /// its scope.
  void setArchetype(ArchetypeType *archetype) { Archetype = archetype; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractTypeParamDecl &&
           D->getKind() <= DeclKind::Last_AbstractTypeParamDecl;
  }
};

/// A declaration of a generic type parameter.
///
/// A generic type parameter introduces a new, named type parameter along
/// with some set of requirements on any type argument used to realize this
/// type parameter. The requirements involve conformances to specific
/// protocols or inheritance from a specific class type.
///
/// In the following example, 'T' is a generic type parameter with the
/// requirement that the type argument conform to the 'Comparable' protocol.
///
/// \code
/// func min<T : Comparable>(x : T, y : T) -> T { ... }
/// \endcode
class GenericTypeParamDecl : public AbstractTypeParamDecl {
  unsigned Depth : 16;
  unsigned Index : 16;

public:
  /// Construct a new generic type parameter.
  ///
  /// \param dc The DeclContext in which the generic type parameter's owner
  /// occurs. This should later be overwritten with the actual declaration
  /// context that owns the type parameter.
  ///
  /// \param name The name of the generic parameter.
  /// \param nameLoc The location of the name.
  GenericTypeParamDecl(DeclContext *dc, Identifier name, SourceLoc nameLoc,
                       unsigned depth, unsigned index);

  /// The depth of this generic type parameter, i.e., the number of outer
  /// levels of generic parameter lists that enclose this type parameter.
  ///
  /// \code
  /// struct X<T> {
  ///   func f<U>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' has depth 0 and 'U' has depth 1. Both have index 0.
  unsigned getDepth() const { return Depth; }

  /// Set the depth of this generic type parameter.
  ///
  /// \sa getDepth
  void setDepth(unsigned depth) { Depth = depth; }

  /// The index of this generic type parameter within its generic parameter
  /// list.
  ///
  /// \code
  /// struct X<T, U> {
  ///   func f<V>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' and 'U' have indexes 0 and 1, respectively. 'V' has index 0.
  unsigned getIndex() const { return Index; }

  SourceLoc getStartLoc() const { return getNameLoc(); }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::GenericTypeParam;
  }
};

/// A declaration of an associated type.
///
/// An associated type introduces a new, named type in a protocol that
/// can vary from one conforming type to the next. Associated types have a
/// set of requirements to which the type that replaces it much realize,
/// describes via conformance to specific protocols, or inheritance from a
/// specific class type.
///
/// In the following example, 'Element' is an associated type with no
/// requirements.
///
/// \code
/// protocol Enumerator {
///   typealias Element
///   func getNext() -> Element?
/// }
/// \endcode
///
/// Every protocol has an implicitly-created associated type 'Self' that
/// describes a type that conforms to the protocol.
class AssociatedTypeDecl : public AbstractTypeParamDecl {
  /// The location of the initial keyword.
  SourceLoc KeywordLoc;

public:
  AssociatedTypeDecl(DeclContext *dc, SourceLoc keywordLoc, Identifier name,
                     SourceLoc nameLoc);

  /// Get the protocol in which this associated type is declared.
  ProtocolDecl *getProtocol() const {
    return cast<ProtocolDecl>(getDeclContext());
  }

  SourceLoc getStartLoc() const { return KeywordLoc; }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::AssociatedType;
  }
};

class MemberLookupTable;

/// A class for iterating local declarations of a nominal type that are of
/// the given FilterDeclType and for which the FilterPredicate function returns
/// true.
template<typename FilterDeclType,
         bool FilterPredicate(FilterDeclType*)>
class DeclFilterRange {
public:
  class iterator {
    /// The remaining declarations.  We need both ends here so that
    /// operator++ knows when to stop.
    ///
    /// Invariant: either this is empty or its first element matches the
    /// filter conditions.
    ArrayRef<Decl*> Remaining;

    friend class DeclFilterRange;
    iterator(ArrayRef<Decl*> remaining) : Remaining(remaining) {}
    
    void skipNonMatching() {
      while (!Remaining.empty()) {
        if (auto filtered = dyn_cast<FilterDeclType>(Remaining.front()))
          if (FilterPredicate(filtered))
            return;
        Remaining = Remaining.slice(1);
      }
    }

  public:
    inline FilterDeclType *operator*() const {
      assert(!Remaining.empty() && "dereferencing empty iterator!");
      return cast<FilterDeclType>(Remaining.front());
    }
    iterator &operator++() {
      assert(!Remaining.empty() && "incrementing empty iterator!");
      Remaining = Remaining.slice(1);
      skipNonMatching();
      return *this;
    }
    iterator operator++(int) {
      iterator old = *this;
      ++*this;
      return old;
    }
    friend bool operator==(iterator lhs, iterator rhs) {
      assert(lhs.Remaining.end() == rhs.Remaining.end() &&
             "comparing iterators from different sources?");
      return lhs.Remaining.begin() == rhs.Remaining.begin();
    }
    friend bool operator!=(iterator lhs, iterator rhs) {
      return !(lhs == rhs);
    }
  };

private:
  /// Our iterator is actually a pretty reasonable representation of
  /// the range itself.
  iterator Members;

public:
  DeclFilterRange(ArrayRef<Decl*> allMembers) : Members(allMembers) {
    // Establish the iterator's invariant.
    Members.skipNonMatching();
  }

  bool empty() const { return Members.Remaining.empty(); }

  iterator begin() const { return Members; }
  iterator end() const {
    // For the benefit of operator==, construct a range whose
    // begin() is the end of the members array.
    auto endRange = Members.Remaining.slice(Members.Remaining.size());
    return iterator(endRange);
  }
  
  FilterDeclType *front() const { return *begin(); }
};

/// Describes the generic signature of a particular declaration, including
/// both the generic type parameters and the requirements placed on those
/// generic parameters.
class GenericSignature {
  unsigned NumGenericParams;
  unsigned NumRequirements;

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  /// Retrieve a mutable version of the generic parameters.
  MutableArrayRef<GenericTypeParamType *> getGenericParamsBuffer() {
    return { reinterpret_cast<GenericTypeParamType **>(this + 1),
             NumGenericParams };
  }

  /// Retrieve a mutable verison of the requirements.
  MutableArrayRef<Requirement> getRequirementsBuffer() {
    void *genericParams = getGenericParamsBuffer().end();
    return { reinterpret_cast<Requirement *>(genericParams),
      NumRequirements };
  }

  GenericSignature(ArrayRef<GenericTypeParamType *> params,
                   ArrayRef<Requirement> requirements);

public:
  /// Create a new generic signature with the given type parameters and
  /// requirements.
  static GenericSignature *get(ArrayRef<GenericTypeParamType *> params,
                               ArrayRef<Requirement> requirements,
                               ASTContext &ctx);

  /// Retrieve the generic parameters.
  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return { reinterpret_cast<GenericTypeParamType * const *>(this + 1),
             NumGenericParams };
  }

  /// Retrieve the requirements.
  ArrayRef<Requirement> getRequirements() const {
    const void *genericParams = getGenericParams().end();
    return { reinterpret_cast<const Requirement *>(genericParams),
             NumRequirements };
  }

  // Only allow allocation by doing a placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

/// NominalTypeDecl - a declaration of a nominal type, like a struct.  This
/// decl is always a DeclContext.
class NominalTypeDecl : public TypeDecl, public DeclContext {
  SourceRange Braces;
  ArrayRef<Decl*> Members;
  GenericParamList *GenericParams;

  /// \brief The generic signature of this type.
  ///
  /// This is the semantic representation of a generic parameters and the
  /// requirements placed on them.
  ///
  /// FIXME: The generic parameters here are also derivable from
  /// \c GenericParams. However, we likely want to make \c GenericParams
  /// the parsed representation, and not part of the module file.
  GenericSignature *GenericSig = nullptr;

  /// \brief The first extension of this type.
  ExtensionDecl *FirstExtension = nullptr;

  /// \brief The last extension of this type, used solely for efficient
  /// insertion of new extensions.
  ExtensionDecl *LastExtension = nullptr;

  /// \brief The generation at which we last loaded extensions.
  unsigned ExtensionGeneration = 0;

  /// \brief A lookup table containing all of the members of this type and
  /// its extensions.
  ///
  /// The table itself is lazily constructed and updated when lookupDirect() is
  /// called.
  MemberLookupTable *LookupTable = nullptr;

  friend class MemberLookupTable;
  friend class ExtensionDecl;

  Type InterfaceTy;

protected:
  Type DeclaredTy;
  Type DeclaredTyInContext;

  void setDeclaredType(Type declaredTy) {
    assert(DeclaredTy.isNull() && "Already set declared type");
    DeclaredTy = declaredTy;
  }

  NominalTypeDecl(DeclKind K, DeclContext *DC, Identifier name,
                  SourceLoc NameLoc,
                  MutableArrayRef<TypeLoc> inherited,
                  GenericParamList *GenericParams) :
    TypeDecl(K, DC, name, NameLoc, inherited),
    DeclContext(DeclContextKind::NominalTypeDecl, DC),
    GenericParams(GenericParams), DeclaredTy(nullptr) {}

  friend class ProtocolType;

public:
  using TypeDecl::getASTContext;

  ArrayRef<Decl*> getMembers() const { return Members; }
  SourceRange getBraces() const { return Braces; }
  void setMembers(ArrayRef<Decl*> M, SourceRange B);

  GenericParamList *getGenericParams() const { return GenericParams; }

  /// Provide the set of parameters to a generic type, or null if
  /// this function is not generic.
  void setGenericParams(GenericParamList *params) {
    assert(!GenericParams && "Already has generic parameters");
    GenericParams = params;
  }

  /// Set the generic signature of this type.
  void setGenericSignature(ArrayRef<GenericTypeParamType *> params,
                           ArrayRef<Requirement> requirements);

  /// Retrieve the generic parameter types.
  ArrayRef<GenericTypeParamType *> getGenericParamTypes() const {
    if (!GenericSig)
      return { };

    return GenericSig->getGenericParams();
  }

  /// Retrieve the generic requirements.
  ArrayRef<Requirement> getGenericRequirements() const {
    if (!GenericSig)
      return { };

    return GenericSig->getRequirements();
  }

  /// getDeclaredType - Retrieve the type declared by this entity.
  Type getDeclaredType() const { return DeclaredTy; }

  /// Compute the type (and declared type) of this nominal type.
  void computeType();

  Type getDeclaredTypeInContext();

  /// Get the "interface" type of the given nominal type, which is the
  /// type used to refer to the nominal type externally.
  ///
  /// For a generic type, or a member thereof, this is the a specialization
  /// of the type using its own generic parameters.
  Type getInterfaceType();

  /// \brief Add a new extension to this nominal type.
  void addExtension(ExtensionDecl *extension);

  /// \brief Retrieve the set of extensions of this type.
  ExtensionRange getExtensions();

  /// Find all of the declarations with the given name within this nominal type
  /// and its extensions.
  ///
  /// This routine does not look into superclasses, nor does it consider
  /// protocols to which the nominal type conforms. Furthermore, the resulting
  /// set of declarations has not been filtered for visibility, nor have
  /// overridden declarations been removed.
  ArrayRef<ValueDecl *> lookupDirect(Identifier name);

  /// Collect the set of protocols to which this type should implicitly
  /// conform, such as DynamicLookup (for classes).
  void getImplicitProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

private:
  /// Predicate used to filter StoredPropertyRange.
  static bool isStoredProperty(VarDecl *vd); // at end of file
  
public:
  /// A range for iterating the stored member variables of a structure.
  using StoredPropertyRange = DeclFilterRange<VarDecl, isStoredProperty>;

  /// Return a collection of the stored member variables of this type.
  StoredPropertyRange getStoredProperties() const {
    return StoredPropertyRange(getMembers());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_NominalTypeDecl &&
           D->getKind() <= DeclKind::Last_NominalTypeDecl;
  }
  static bool classof(const DeclContext *C) {
    return C->getContextKind() == DeclContextKind::NominalTypeDecl;
  }
  static bool classof(const NominalTypeDecl *D) { return true; }
  static bool classof(const ExtensionDecl *D) { return false; }

  using DeclContext::operator new;
};

/// \brief This is the declaration of an enum.
///
/// For example:
///
/// \code
///    enum Bool {
///      case false
///      case true
///    }
///
///    enum Optional<T> {
///      case None
///      case Just(T)
///    }
/// \endcode
///
/// The type of the decl itself is a MetaTypeType; use getDeclaredType()
/// to get the declared type ("Bool" or "Optional" in the above example).
class EnumDecl : public NominalTypeDecl {
  SourceLoc EnumLoc;
  Type RawType;

public:
  EnumDecl(SourceLoc EnumLoc, Identifier Name, SourceLoc NameLoc,
            MutableArrayRef<TypeLoc> Inherited,
            GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return EnumLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(EnumLoc, getBraces().End);
  }

  EnumElementDecl *getElement(Identifier Name) const;
  
private:
  /// Predicate used to filter ElementRange.
  static bool isElement(EnumElementDecl *ued) { return true; }
  
public:
  /// A range for iterating the elements of an enum.
  using ElementRange = DeclFilterRange<EnumElementDecl, isElement>;

  /// Return a range that iterates over all the elements of an enum.
  ElementRange getAllElements() const {
    return ElementRange(getMembers());
  }
  
  /// Insert all of the 'case' element declarations into a DenseSet.
  void getAllElements(llvm::DenseSet<EnumElementDecl*> &elements) const {
    for (auto elt : getAllElements())
      elements.insert(elt);
  }
  
  /// Retrieve the status of circularity checking for class inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(EnumDeclBits.Circularity);
  }
  
  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    EnumDeclBits.Circularity = static_cast<unsigned>(circularity);
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Enum;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
  
  /// Determine whether this enum declares a raw type in its inheritance clause.
  bool hasRawType() const { return (bool)RawType; }
  /// Retrieve the declared raw type of the enum from its inheritance clause,
  /// or null if it has none.
  Type getRawType() const { return RawType; }

  /// Set the raw type of the enum from its inheritance clause.
  void setRawType(Type rawType) { RawType = rawType; }
};

/// StructDecl - This is the declaration of a struct, for example:
///
///    struct Complex { var R : Double, I : Double }
///
/// The type of the decl itself is a MetaTypeType; use getDeclaredType()
/// to get the declared type ("Complex" in the above example).
class StructDecl : public NominalTypeDecl {
  SourceLoc StructLoc;

public:
  StructDecl(SourceLoc StructLoc, Identifier Name, SourceLoc NameLoc,
             MutableArrayRef<TypeLoc> Inherited,
             GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return StructLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(StructLoc, getBraces().End);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Struct;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
};

/// ClassDecl - This is the declaration of a class, for example:
///
///    class Complex { var R : Double, I : Double }
///
/// The type of the decl itself is a MetaTypeType; use getDeclaredType()
/// to get the declared type ("Complex" in the above example).
class ClassDecl : public NominalTypeDecl {
  SourceLoc ClassLoc;
  Type Superclass;

public:
  ClassDecl(SourceLoc ClassLoc, Identifier Name, SourceLoc NameLoc,
            MutableArrayRef<TypeLoc> Inherited,
            GenericParamList *GenericParams, DeclContext *DC);

  SourceLoc getStartLoc() const { return ClassLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ClassLoc, getBraces().End);
  }

  /// Determine whether this class has a superclass.
  bool hasSuperclass() const { return (bool)Superclass; }

  /// Retrieve the superclass of this class, or null if there is no superclass.
  Type getSuperclass() const { return Superclass; }

  /// Set the superclass of this class.
  void setSuperclass(Type superclass) { Superclass = superclass; }

  /// Retrieve the status of circularity checking for class inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(ClassDeclBits.Circularity);
  }

  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    ClassDeclBits.Circularity = static_cast<unsigned>(circularity);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Class;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
};


/// ProtocolDecl - A declaration of a protocol, for example:
///
///   protocol Drawable {
///     func draw()
///   }
class ProtocolDecl : public NominalTypeDecl {
  SourceLoc ProtocolLoc;

  bool requiresClassSlow();

public:
  ProtocolDecl(DeclContext *DC, SourceLoc ProtocolLoc, SourceLoc NameLoc,
               Identifier Name, MutableArrayRef<TypeLoc> Inherited);
  
  using Decl::getASTContext;

  void setMembers(MutableArrayRef<Decl *> M, SourceRange B) {
    NominalTypeDecl::setMembers(M, B);
  }

  /// \brief Determine whether this protocol inherits from the given ("super")
  /// protocol.
  bool inheritsFrom(const ProtocolDecl *Super) const;
  
  /// \brief Collect all of the inherited protocols into the given set.
  void collectInherited(llvm::SmallPtrSet<ProtocolDecl *, 4> &Inherited);
  
  ProtocolType *getDeclaredType() const {
    return reinterpret_cast<ProtocolType *>(DeclaredTy.getPointer());
  }
  
  SourceLoc getStartLoc() const { return ProtocolLoc; }
  SourceRange getSourceRange() const {
    return SourceRange(ProtocolLoc, getBraces().End);
  }

  /// \brief Retrieve the generic parameter 'Self'.
  GenericTypeParamDecl *getSelf() const;

  /// True if this protocol can only be conformed to by class types.
  bool requiresClass() {
    if (ProtocolDeclBits.RequiresClassValid)
      return ProtocolDeclBits.RequiresClass;

    return requiresClassSlow();
  }

  /// Determine whether an existential value conforming to just this protocol
  /// conforms to the protocol itself.
  ///
  /// \returns an empty optional if not yet known, true if the existential
  /// does conform to this protocol, and false otherwise.
  Optional<bool> existentialConformsToSelf() const {
    if (ProtocolDeclBits.ExistentialConformsToSelfValid)
      return ProtocolDeclBits.ExistentialConformsToSelf;

    return Nothing;
  }

  /// Set whether the existential of this protocol type conforms to this
  /// protocol.
  void setExistentialConformsToSelf(bool conforms) {
    ProtocolDeclBits.ExistentialConformsToSelfValid = true;
    ProtocolDeclBits.ExistentialConformsToSelf = conforms;
  }

  /// If this is known to be a compiler-known protocol, returns the kind.
  /// Otherwise returns Nothing.
  ///
  /// Note that this is only valid after type-checking.
  Optional<KnownProtocolKind> getKnownProtocolKind() const {
    if (ProtocolDeclBits.KnownProtocol == 0)
      return Nothing;
    return static_cast<KnownProtocolKind>(ProtocolDeclBits.KnownProtocol - 1);
  }

  /// Check whether this protocol is of a specific, known protocol kind.
  bool isSpecificProtocol(KnownProtocolKind kind) const {
    if (auto knownKind = getKnownProtocolKind())
      return *knownKind == kind;

    return false;
  }

  /// Records that this is a compiler-known protocol.
  void setKnownProtocolKind(KnownProtocolKind kind) {
    assert((!getKnownProtocolKind() || *getKnownProtocolKind() == kind) &&
           "can't reset known protocol kind");
    ProtocolDeclBits.KnownProtocol = static_cast<unsigned>(kind) + 1;
    assert(getKnownProtocolKind() && *getKnownProtocolKind() == kind &&
           "not enough bits");
  }

  /// Retrieve the status of circularity checking for protocol inheritance.
  CircularityCheck getCircularityCheck() const {
    return static_cast<CircularityCheck>(ProtocolDeclBits.Circularity);
  }

  /// Record the current stage of circularity checking.
  void setCircularityCheck(CircularityCheck circularity) {
    ProtocolDeclBits.Circularity = static_cast<unsigned>(circularity);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const NominalTypeDecl *D) {
    return D->getKind() == DeclKind::Protocol;
  }
  static bool classof(const DeclContext *C) {
    return isa<NominalTypeDecl>(C) && classof(cast<NominalTypeDecl>(C));
  }
};

/// VarDecl - 'var' declaration.
class VarDecl : public ValueDecl {
private:
  struct GetSetRecord {
    SourceRange Braces;
    FuncDecl *Get;       // User-defined getter
    FuncDecl *Set;       // User-defined setter
  };
  
  // FIXME: These fields are useless for most of the VarDecls that are created
  // for patterns. We should refactor to a new node.
  GetSetRecord *GetSet = nullptr;
  PatternBindingDecl *ParentPattern = nullptr;
  VarDecl *OverriddenDecl = nullptr;

public:
  VarDecl(SourceLoc NameLoc, Identifier Name, Type Ty, DeclContext *DC)
    : ValueDecl(DeclKind::Var, DC, Name, NameLoc) {
    setType(Ty);
  }

  SourceLoc getStartLoc() const { return getNameLoc(); }
  SourceRange getSourceRange() const { return getNameLoc(); }

  /// \brief Determine whether this variable is computed, which means it
  /// has no storage but does have a user-defined getter or setter.
  ///
  /// The opposite of "computed" is "stored".
  bool isComputed() const { return GetSet != nullptr; }
  
  /// \brief Turn this into a computed variable, providing a getter and setter.
  void setComputedAccessors(ASTContext &Context, SourceLoc LBraceLoc,
                            FuncDecl *Get, FuncDecl *Set, SourceLoc RBraceLoc);

  /// \brief Retrieve the getter used to access the value of this variable.
  FuncDecl *getGetter() const { return GetSet? GetSet->Get : nullptr; }

  /// \brief Retrieve the setter used to mutate the value of this variable.
  FuncDecl *getSetter() const { return GetSet? GetSet->Set : nullptr; }
  
  /// \brief Returns whether the var is settable, either because it is a
  /// stored var or because it has a custom setter.
  bool isSettable() const { return !GetSet || GetSet->Set; }
  
  VarDecl *getOverriddenDecl() const {
    return OverriddenDecl;
  }
  void setOverriddenDecl(VarDecl *over) {
    OverriddenDecl = over;
  }
  
  PatternBindingDecl *getParentPattern() const {
    return ParentPattern;
  }
  void setParentPattern(PatternBindingDecl *PBD) {
    ParentPattern = PBD;
  }

  /// Determine whether this declaration is an anonymous closure parameter.
  bool isAnonClosureParam() const;

  /// Given that this is an Objective-C property declaration, produce
  /// its getter selector in the given buffer (as UTF-8).
  StringRef getObjCGetterSelector(SmallVectorImpl<char> &buffer) const;

  /// Given that this is an Objective-C property declaration, produce
  /// its setter selector in the given buffer (as UTF-8).
  StringRef getObjCSetterSelector(SmallVectorImpl<char> &buffer) const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Var; }
};

/// \brief Base class for function-like declarations.
class AbstractFunctionDecl : public ValueDecl, public DeclContext {
public:
  enum class BodyKind {
    /// The function did not have a body in the source code file.
    None,

    /// Function body is delayed, to be parsed later.
    Unparsed,

    /// Function body is parsed and available as an AST subtree.
    Parsed,

    /// Function body is not available, although it was written in the source.
    Skipped
  };

  BodyKind getBodyKind() const {
    return BodyKind(AbstractFunctionDeclBits.BodyKind);
  }

protected:
  // If a function has a body at all, we have either a parsed body AST node or
  // we have saved the end location of the unparsed body.
  union {
    /// This enum member is active if getBodyKind() == BodyKind::Parsed.
    BraceStmt *Body;

    /// End location of the function body when the body is delayed or skipped.
    /// This enum member is active if getBodyKind() is BodyKind::Unparsed or
    /// BodyKind::Skipped.
    SourceLoc BodyEndLoc;
  };

  /// Pointer to the implicit 'self' decl and a bit that is set to true when
  /// this pointer is computed already.
  mutable llvm::PointerIntPair<VarDecl *, 1, bool> ImplicitSelfDeclAndIsCached;

  GenericParamList *GenericParams;

  CaptureInfo Captures;

  Type InterfaceTy;

  AbstractFunctionDecl(DeclKind Kind, DeclContext *Parent, Identifier Name,
                       SourceLoc NameLoc,
                       VarDecl *ImplicitSelfDecl,
                       GenericParamList *GenericParams)
      : ValueDecl(Kind, Parent, Name, NameLoc),
        DeclContext(DeclContextKind::AbstractFunctionDecl, Parent),
        Body(nullptr), GenericParams(GenericParams) {
    if (ImplicitSelfDecl)
      ImplicitSelfDeclAndIsCached.setPointerAndInt(ImplicitSelfDecl, true);
    else
      ImplicitSelfDeclAndIsCached.setPointerAndInt(nullptr, false);
    setBodyKind(BodyKind::None);
    AbstractFunctionDeclBits.HasSelectorStyleSignature = false;
  }

  VarDecl *getImplicitSelfDeclSlow() const;

  MutableArrayRef<Pattern *> getArgParamBuffer();
  MutableArrayRef<Pattern *> getBodyParamBuffer();

  void setBodyKind(BodyKind K) {
    AbstractFunctionDeclBits.BodyKind = unsigned(K);
  }

public:
  /// \brief If this is a method in a type extension for some type,
  /// return that type, otherwise return Type().
  Type getExtensionType() const;

  /// Returns true if the function has a body written in the source file.
  ///
  /// Note that a true return value does not imply that the body was actually
  /// parsed.
  bool hasBody() const {
    return getBodyKind() != BodyKind::None;
  }

  /// Returns the function body, if it was parsed, or nullptr otherwise.
  ///
  /// Note that a null return value does not imply that the source code did not
  /// have a body for this function.
  ///
  /// \sa hasBody()
  BraceStmt *getBody() const {
    if (getBodyKind() == BodyKind::Parsed)
      return Body;
    return nullptr;
  }
  void setBody(BraceStmt *S) {
    assert(getBodyKind() != BodyKind::Skipped &&
           "can not set a body if it was skipped");

    Body = S;
    setBodyKind(BodyKind::Parsed);
  }

  /// \brief Note that the body was skipped for this function.  Function body
  /// can not be attached after this call.
  void setBodySkipped(SourceLoc EndLoc) {
    assert(getBodyKind() == BodyKind::None);
    BodyEndLoc = EndLoc;
    setBodyKind(BodyKind::Skipped);
  }

  /// \brief Note that parsing for the body was delayed.
  void setBodyDelayed(SourceLoc EndLoc) {
    assert(getBodyKind() == BodyKind::None);
    BodyEndLoc = EndLoc;
    setBodyKind(BodyKind::Unparsed);
  }

  CaptureInfo &getCaptureInfo() { return Captures; }
  const CaptureInfo &getCaptureInfo() const { return Captures; }

  bool hasSelectorStyleSignature() const {
    return AbstractFunctionDeclBits.HasSelectorStyleSignature;
  }

  void setHasSelectorStyleSignature() {
    AbstractFunctionDeclBits.HasSelectorStyleSignature = true;
  }

  /// Retrieve the "interface" type of the function, which is the type of
  /// the declaration as viewed from an outside. For a polymorphic function,
  /// this will have generic function type using generic parameters rather than
  /// archetypes.
  ///
  /// FIXME: Eventually, this will simply become the type of the function.
  Type getInterfaceType() const { return InterfaceTy; }

  void setInterfaceType(Type type) {
    assert(InterfaceTy.isNull() && "Already have an interface type");
    InterfaceTy = type;
  }

  /// Determine the default argument kind and type for the given argument index
  /// in this declaration, which must be a function or constructor.
  ///
  /// \param Index The index of the argument for which we are querying the
  /// default argument.
  ///
  /// \returns the default argument kind and, if there is a default argument,
  /// the type of the corresponding parameter.
  std::pair<DefaultArgumentKind, Type> getDefaultArg(unsigned Index) const;

  /// \brief Returns the argument pattern(s) for the function definition
  /// that determine the function type.
  //
  /// - For a definition of the form `func foo(a:A, b:B)`, this will
  ///   be a one-element array containing the argument pattern `(a:A, b:B)`.
  /// - For a curried definition such as `func foo(a:A)(b:B)`, this will
  ///   be a multiple-element array containing a pattern for each level
  ///   of currying, in this case two patterns `(a:A)` and `(b:B)`.
  /// - For a selector-style definition such as `func foo(a:A) bar(b:B)`,
  ///   this will be a one-element array containing the argument pattern
  ///   of the keyword arguments, in this case `(_:A, bar:B)`. For selector-
  ///   style definitions, this is different from `getBodyParamPatterns`,
  ///   which would return the declared parameter names `(a:A, b:B)`.
  ///
  /// If the function expression refers to a method definition, there will
  /// be an additional first argument pattern for the `this` parameter.
  MutableArrayRef<Pattern *> getArgParamPatterns() {
    return getArgParamBuffer();
  }
  ArrayRef<const Pattern *> getArgParamPatterns() const {
    auto Patterns =
        const_cast<AbstractFunctionDecl *>(this)->getArgParamBuffer();
    return ArrayRef<const Pattern *>(Patterns.data(), Patterns.size());
  }

  /// \brief Returns the parameter pattern(s) for the function definition that
  /// determine the parameter names bound in the function body.
  ///
  /// Typically, this is the same as \c getArgParamPatterns, unless the
  /// function was defined with selector-style syntax such as:
  /// \code
  ///   func foo(a:A) bar(b:B) {}
  /// \endcode
  ///
  /// For a selector-style definition, \c getArgParamPatterns will return the
  /// pattern that describes the keyword argument names, in this case
  /// `(_:A, bar:B)`, whereas \c getBodyParamPatterns will return a pattern
  /// referencing the declared parameter names in the function body's scope,
  /// in this case `(a:A, b:B)`.
  ///
  /// In all cases `getArgParamPatterns().size()` should equal
  /// `getBodyParamPatterns().size()`, and the corresponding elements of each
  /// tuple type should have equivalent types.
  MutableArrayRef<Pattern *> getBodyParamPatterns() {
    return getBodyParamBuffer();
  }
  ArrayRef<const Pattern *> getBodyParamPatterns() const {
    auto Patterns =
        const_cast<AbstractFunctionDecl *>(this)->getBodyParamBuffer();
    return ArrayRef<const Pattern *>(Patterns.data(), Patterns.size());
  }

  /// \brief This method returns the implicit 'self' decl.
  ///
  /// Note that some functions don't have an implicit 'self' decl, for example,
  /// free functions.  In this case nullptr is returned.
  VarDecl *getImplicitSelfDecl() const {
    if (ImplicitSelfDeclAndIsCached.getInt())
      return ImplicitSelfDeclAndIsCached.getPointer();
    return getImplicitSelfDeclSlow();
  }

  /// \brief Retrieve the set of parameters to a generic function, or null if
  /// this function is not generic.
  GenericParamList *getGenericParams() const { return GenericParams; }

  /// \brief Determine whether this is a generic function, which can only be
  /// used when each of the archetypes is bound to a particular concrete type.
  bool isGeneric() const { return GenericParams != nullptr; }

  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_AbstractFunctionDecl &&
           D->getKind() <= DeclKind::Last_AbstractFunctionDecl;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::AbstractFunctionDecl;
  }

  using DeclContext::operator new;
  using Decl::getASTContext;
};

class OperatorDecl;

/// FuncDecl - 'func' declaration.
class FuncDecl : public AbstractFunctionDecl {
  friend class AbstractFunctionDecl;

  SourceLoc StaticLoc;  // Location of the 'static' token or invalid.
  SourceLoc FuncLoc;    // Location of the 'func' token.

  TypeLoc FnRetType;

  /// The result type as seen from the body of the function.
  ///
  /// \sa getBodyResultType()
  Type BodyResultType;

  llvm::PointerIntPair<Decl *, 1, bool> GetOrSetDecl;
  FuncDecl *OverriddenDecl;
  OperatorDecl *Operator;

  FuncDecl(SourceLoc StaticLoc, SourceLoc FuncLoc, Identifier Name,
           SourceLoc NameLoc, unsigned NumParamPatterns,
           GenericParamList *GenericParams, Type Ty, DeclContext *Parent)
    : AbstractFunctionDecl(DeclKind::Func, Parent, Name, NameLoc, nullptr,
                           GenericParams),
      StaticLoc(StaticLoc), FuncLoc(FuncLoc),
      OverriddenDecl(nullptr), Operator(nullptr) {
    FuncDeclBits.Static = StaticLoc.isValid() || getName().isOperator();
    assert(NumParamPatterns > 0);
    FuncDeclBits.NumParamPatterns = NumParamPatterns;
    setType(Ty);
  }

  VarDecl *getImplicitSelfDeclImpl() const;

  unsigned getNumParamPatternsImpl() const {
    return FuncDeclBits.NumParamPatterns;
  }

public:
  /// Factory function only for use by deserialization.
  static FuncDecl *createDeserialized(ASTContext &Context, SourceLoc StaticLoc,
                                      SourceLoc FuncLoc, Identifier Name,
                                      SourceLoc NameLoc,
                                      GenericParamList *GenericParams, Type Ty,
                                      unsigned NumParamPatterns,
                                      DeclContext *Parent);

  static FuncDecl *create(ASTContext &Context, SourceLoc StaticLoc,
                          SourceLoc FuncLoc, Identifier Name, SourceLoc NameLoc,
                          GenericParamList *GenericParams, Type Ty,
                          ArrayRef<Pattern *> ArgParams,
                          ArrayRef<Pattern *> BodyParams,
                          TypeLoc FnRetType, DeclContext *Parent);

  bool isStatic() const {
    return FuncDeclBits.Static;
  }
  void setStatic(bool Static = true) {
    FuncDeclBits.Static = Static;
  }

  void setDeserializedSignature(ArrayRef<Pattern *> ArgParams,
                                ArrayRef<Pattern *> BodyParams,
                                TypeLoc FnRetType);

  /// \brief Returns the "natural" number of argument clauses taken by this
  /// function.  This value is always at least one, and it may be more if the
  /// function is implicitly or explicitly curried.
  ///
  /// For example, this function:
  /// \code
  ///   func negate(x : Int) -> Int { return -x }
  /// \endcode
  /// has a natural argument count of 1 if it is freestanding.  If it is
  /// a method, it has a natural argument count of 2, as does this
  /// curried function:
  /// \code
  ///   func add(x : Int)(y : Int) -> Int { return x + y }
  /// \endcode
  ///
  /// This value never exceeds the number of chained function types
  /// in the function's type, but it can be less for functions which
  /// return a value of function type:
  /// \code
  ///   func const(x : Int) -> () -> Int { return { x } } // NAC==1
  /// \endcode
  unsigned getNaturalArgumentCount() const {
    return getNumParamPatternsImpl();
  }

  /// \brief If this is a method in a type extension for some type, compute
  /// and return the type to be used for the 'self' argument of the type
  /// (which varies based on whether the extended type is a reference type
  /// or not), or an empty Type() if no 'self' argument should exist.  This can
  /// only be used after name binding has resolved types.
  ///
  /// \param OuterGenericParams If non-NULL, and this function is an instance
  /// of a generic type, will be set to the generic parameter list of that
  /// generic type.
  Type computeSelfType(GenericParamList **OuterGenericParams = nullptr) const;

  SourceLoc getStaticLoc() const { return StaticLoc; }
  SourceLoc getFuncLoc() const { return FuncLoc; }

  SourceLoc getStartLoc() const {
    return StaticLoc.isValid() ? StaticLoc : FuncLoc;
  }
  SourceRange getSourceRange() const;

  TypeLoc &getBodyResultTypeLoc() { return FnRetType; }
  const TypeLoc &getBodyResultTypeLoc() const { return FnRetType; }

  /// Retrieve the result type of this function.
  ///
  /// \sa getBodyResultType
  Type getResultType(ASTContext &Ctx) const;

  /// Retrieve the result type of this function for use within the function
  /// definition.
  ///
  /// FIXME: The statement below is a wish, not reality.
  /// The "body" result type will only differ from the result type within the
  /// interface to the function for a polymorphic function, where the interface
  /// may contain generic parameters while the definition will contain
  /// the corresponding archetypes.
  Type getBodyResultType() const { return BodyResultType; }

  /// Set the result type as viewed from the function body.
  ///
  /// \sa getBodyResultType
  void setBodyResultType(Type bodyResultType) {
    assert(BodyResultType.isNull() && "Already set body result type");
    BodyResultType = bodyResultType;
  }

  /// Revert to an empty type.
  void revertType() {
    BodyResultType = Type();
    overwriteType(Type());
  }

  /// isUnaryOperator - Determine whether this is a unary operator
  /// implementation, in other words, the name of the function is an operator,
  /// and the argument list consists syntactically of a single-element tuple
  /// pattern. This check is syntactic rather than type-based in order to allow
  /// for the definition of unary operators on tuples, as in:
  ///   func [prefix] + (_:(a:Int, b:Int))
  /// This also allows the unary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isUnaryOperator() const;
  
  /// isBinaryOperator - Determine whether this is a binary operator
  /// implementation, in other words, the name of the function is an operator,
  /// and the argument list consists syntactically of a two-element tuple
  /// pattern. This check is syntactic rather than type-based in order to
  /// distinguish a binary operator from a unary operator on tuples, as in:
  ///   func [prefix] + (_:(a:Int, b:Int)) // unary operator +(1,2)
  ///   func [infix]  + (a:Int, b:Int)     // binary operator 1 + 2
  /// This also allows the binary-operator-ness of a func decl to be determined
  /// prior to type checking.
  bool isBinaryOperator() const;
  
  /// makeGetter - Note that this function is the getter for the given
  /// declaration, which may be either a variable or a subscript declaration.
  void makeGetter(Decl *D) {
    GetOrSetDecl.setPointer(D);
    GetOrSetDecl.setInt(false);
  }
  
  /// makeSetter - Note that this function is the setter for the given
  /// declaration, which may be either a variable or a subscript declaration.
  void makeSetter(Decl *D) {
    GetOrSetDecl.setPointer(D);
    GetOrSetDecl.setInt(true);
  }
  
  /// getGetterDecl - If this function is a getter, retrieve the declaration for
  /// which it is a getter. Otherwise, returns null.
  Decl *getGetterDecl() const {
    return GetOrSetDecl.getInt()? nullptr : GetOrSetDecl.getPointer();
  }

  /// getSetterDecl - If this function is a setter, retrieve the declaration for
  /// which it is a setter. Otherwise, returns null.
  Decl *getSetterDecl() const {
    return GetOrSetDecl.getInt()? GetOrSetDecl.getPointer() : nullptr;
  }

  /// isGetterOrSetter - Determine whether this is a getter or a setter vs.
  /// a normal function.
  bool isGetterOrSetter() const { return getGetterOrSetterDecl() != 0; }

  /// getGetterOrSetterDecl - Return the declaration for which this function
  /// is a getter or setter, if it is one.
  Decl *getGetterOrSetterDecl() const { return GetOrSetDecl.getPointer(); }

  /// Given that this is an Objective-C method declaration, produce
  /// its selector in the given buffer (as UTF-8).
  StringRef getObjCSelector(SmallVectorImpl<char> &buffer) const;

  FuncDecl *getOverriddenDecl() const { return OverriddenDecl; }
  void setOverriddenDecl(FuncDecl *over) { OverriddenDecl = over; }
  
  OperatorDecl *getOperatorDecl() const { return Operator; }
  void setOperatorDecl(OperatorDecl *o) {
    assert(isOperator() && "can't set an OperatorDecl for a non-operator");
    Operator = o;
  }

  static bool classof(const Decl *D) { return D->getKind() == DeclKind::Func; }
};
  
/// \brief This represents a 'case' declaration in an 'enum', which may declare
/// one or more individual comma-separated EnumElementDecls.
class EnumCaseDecl : public Decl {
  SourceLoc CaseLoc;
  
  /// The number of tail-allocated element pointers.
  unsigned NumElements;
  
  EnumCaseDecl(SourceLoc CaseLoc,
               ArrayRef<EnumElementDecl *> Elements,
               DeclContext *DC)
    : Decl(DeclKind::EnumCase, DC),
      CaseLoc(CaseLoc), NumElements(Elements.size())
  {
    memcpy(this + 1, Elements.begin(), NumElements * sizeof(EnumElementDecl*));
  }
  
  EnumElementDecl * const *getElementsBuf() const {
    return reinterpret_cast<EnumElementDecl * const*>(this + 1);
  }
  
public:
  static EnumCaseDecl *create(SourceLoc CaseLoc,
                              ArrayRef<EnumElementDecl*> Elements,
                              DeclContext *DC);
  
  /// Get the list of elements declared in this case.
  ArrayRef<EnumElementDecl *> getElements() const {
    return {getElementsBuf(), NumElements};
  }
  
  SourceLoc getLoc() const {
    return CaseLoc;
  }
  
  SourceRange getSourceRange() const;
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::EnumCase;
  }
};

/// \brief This represents a single case of an 'enum' declaration.
///
/// For example, the X, Y, and Z in this enum:
///
/// \code
///   enum V {
///     case X(Int), Y(Int)
///     case Z
///   }
/// \endcode
///
/// The type of an EnumElementDecl is always the EnumType for the containing
/// enum. EnumElementDecls are represented in the AST as members of their
/// parent EnumDecl, although syntactically they are subordinate to the
/// EnumCaseDecl.
class EnumElementDecl : public ValueDecl {
  /// This is the type specified with the enum element, for
  /// example 'Int' in 'case Y(Int)'.  This is null if there is no type
  /// associated with this element, as in 'case Z' or in all elements of enum
  /// definitions.
  TypeLoc ArgumentType;
  
  SourceLoc EqualsLoc;
  
  /// The raw value literal for the enum element, or null.
  LiteralExpr *RawValueExpr;
  /// The type-checked raw value expression.
  Expr *TypeCheckedRawValueExpr = nullptr;
  
public:
  EnumElementDecl(SourceLoc IdentifierLoc, Identifier Name,
                  TypeLoc ArgumentType,
                  SourceLoc EqualsLoc,
                  LiteralExpr *RawValueExpr,
                  DeclContext *DC)
  : ValueDecl(DeclKind::EnumElement, DC, Name, IdentifierLoc),
    ArgumentType(ArgumentType),
    EqualsLoc(EqualsLoc),
    RawValueExpr(RawValueExpr)
  {}

  bool hasArgumentType() const { return !ArgumentType.getType().isNull(); }
  Type getArgumentType() const { return ArgumentType.getType(); }
  TypeLoc &getArgumentTypeLoc() { return ArgumentType; }
  
  bool hasRawValueExpr() const { return RawValueExpr; }
  LiteralExpr *getRawValueExpr() const { return RawValueExpr; }
  void setRawValueExpr(LiteralExpr *e) { RawValueExpr = e; }
  
  Expr *getTypeCheckedRawValueExpr() const {
    return TypeCheckedRawValueExpr;
  }
  void setTypeCheckedRawValueExpr(Expr *e) {
    TypeCheckedRawValueExpr = e;
  }
  
  /// Return the containing EnumDecl.
  EnumDecl *getParentEnum() const {
    return cast<EnumDecl>(getDeclContext());
  }
  
  SourceLoc getStartLoc() const {
    return getNameLoc();
  }
  SourceRange getSourceRange() const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::EnumElement;
  }
};
  
inline SourceRange EnumCaseDecl::getSourceRange() const {
  auto subRange = getElements().back()->getSourceRange();
  if (subRange.isValid())
    return {CaseLoc, subRange.End};
  return {};
}

/// Describes the kind of subscripting used in Objective-C.
enum class ObjCSubscriptKind {
  /// Not an Objective-C subscripting kind.
  None,
  /// Objective-C indexed subscripting, which is based on an integral
  /// index.
  Indexed,
  /// Objective-C keyed subscripting, which is based on an object
  /// argument or metatype thereof.
  Keyed
};

/// \brief Declares a subscripting operator for a type.
///
/// A subscript declaration is defined as a get/set pair that produces a
/// specific type. For example:
///
/// \code
/// subscript (i : Int) -> String {
///   get: /* return ith String */
///   set: /* set ith string to value */
/// }
/// \endcode
///
/// A type with a subscript declaration can be used as the base of a subscript
/// expression a[i], where a is of the subscriptable type and i is the type
/// of the index. A subscript can have multiple indices:
///
/// \code
/// struct Matrix {
///   subscript (i : Int, j : Int) -> Double {
///     get: /* return element at position (i, j) */
///     set: /* set element at position (i, j) */
///   }
/// }
/// \endcode
///
/// A given type can have multiple subscript declarations, so long as the
/// signatures (indices and element type) are distinct.
///
/// FIXME: SubscriptDecl isn't naturally a ValueDecl, but it's currently useful
/// to get name lookup to find it with a bogus name.
class SubscriptDecl : public ValueDecl {
  SourceLoc ArrowLoc;
  Pattern *Indices;
  TypeLoc ElementTy;
  SourceRange Braces;
  FuncDecl *Get;
  FuncDecl *Set;
  SubscriptDecl *OverriddenDecl;

public:
  SubscriptDecl(Identifier NameHack, SourceLoc SubscriptLoc, Pattern *Indices,
                SourceLoc ArrowLoc, TypeLoc ElementTy,
                SourceRange Braces, FuncDecl *Get, FuncDecl *Set,
                DeclContext *Parent)
    : ValueDecl(DeclKind::Subscript, Parent, NameHack, SubscriptLoc),
      ArrowLoc(ArrowLoc), Indices(Indices), ElementTy(ElementTy),
      Braces(Braces), Get(Get), Set(Set), OverriddenDecl(nullptr) { }
  
  SourceLoc getSubscriptLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getSubscriptLoc(); }
  SourceRange getSourceRange() const;

  /// \brief Retrieve the indices for this subscript operation.
  Pattern *getIndices() { return Indices; }
  const Pattern *getIndices() const { return Indices; }
  void setIndices(Pattern *p) { Indices = p; }

  /// \brief Retrieve the type of the element referenced by a subscript
  /// operation.
  Type getElementType() const { return ElementTy.getType(); }
  TypeLoc &getElementTypeLoc() { return ElementTy; }

  /// \brief Retrieve the subscript getter, a function that takes the indices
  /// and produces a value of the element type.
  FuncDecl *getGetter() const { return Get; }
  
  /// \brief Retrieve the subscript setter, a function that takes the indices
  /// and a new value of the lement type and updates the corresponding value.
  ///
  /// The subscript setter is optional.
  FuncDecl *getSetter() const { return Set; }
  
  /// \brief Returns whether the subscript operation has a setter.
  bool isSettable() const { return Set; }

  /// Determine the kind of Objective-C subscripting this declaration
  /// implies.
  ObjCSubscriptKind getObjCSubscriptKind() const;

  /// Given that this is an Objective-C subscript declaration, produce
  /// its getter selector.
  StringRef getObjCGetterSelector() const;

  /// Given that this is an Objective-C subscript declaration, produce
  /// its setter selector.
  StringRef getObjCSetterSelector() const;

  SubscriptDecl *getOverriddenDecl() const { return OverriddenDecl; }
  void setOverriddenDecl(SubscriptDecl *over) { OverriddenDecl = over; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Subscript;
  }
};

/// ConstructorDecl - Declares a constructor for a type.  For example:
///
/// \code
/// struct X {
///   var x : Int
///   constructor(i : Int) {
///      x = i
///   }
/// }
/// \endcode
class ConstructorDecl : public AbstractFunctionDecl {
  friend class AbstractFunctionDecl;

  Pattern *ArgParams;
  Pattern *BodyParams;
  
  /// The type of the initializing constructor.
  Type InitializerType;

  /// The interface type of the initializing constructor.
  Type InitializerInterfaceType;

public:
  ConstructorDecl(Identifier NameHack, SourceLoc ConstructorLoc,
                  Pattern *ArgParams, Pattern *BodyParams,
                  VarDecl *ImplicitSelfDecl,
                  GenericParamList *GenericParams, DeclContext *Parent)
    : AbstractFunctionDecl(DeclKind::Constructor, Parent, NameHack,
                           ConstructorLoc, ImplicitSelfDecl, GenericParams),
      ArgParams(ArgParams), BodyParams(BodyParams) {
    assert(ImplicitSelfDecl && "constructors should have a non-null self");
  }

  SourceLoc getConstructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getConstructorLoc(); }
  SourceRange getSourceRange() const;

  Pattern *getArgParams() { return ArgParams; }
  const Pattern *getArgParams() const { return ArgParams; }

  void setArgParams(Pattern *argParams) {
    ArgParams = argParams;
  }

  Pattern *getBodyParams() { return BodyParams; }
  const Pattern *getBodyParams() const { return BodyParams; }

  void setBodyParams(Pattern *bodyParams) {
    BodyParams = bodyParams;
  }

  /// \brief Compute and return the type of 'self'.
  Type computeSelfType(GenericParamList **OuterGenericParams = nullptr) const;

  /// getArgumentType - get the type of the argument tuple
  Type getArgumentType() const;

  /// \brief Get the type of the constructed object.
  Type getResultType() const;

  /// Given that this is an Objective-C method declaration, produce
  /// its selector in the given buffer (as UTF-8).
  StringRef getObjCSelector(SmallVectorImpl<char> &buffer) const;

  /// Get the type of the initializing constructor.
  Type getInitializerType() const { return InitializerType; }
  void setInitializerType(Type t) { InitializerType = t; }

  /// Get the interface type of the initializing constructor.
  Type getInitializerInterfaceType() const { return InitializerInterfaceType; }
  void setInitializerInterfaceType(Type t) { InitializerInterfaceType = t; }

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Constructor;
  }
};

/// DestructorDecl - Declares a destructor for a type.  For example:
///
/// \code
/// struct X {
///   var fd : Int
///   destructor() {
///      close(fd)
///   }
/// }
/// \endcode
class DestructorDecl : public AbstractFunctionDecl {

public:
  DestructorDecl(Identifier NameHack, SourceLoc DestructorLoc,
                  VarDecl *ImplicitSelfDecl, DeclContext *Parent)
    : AbstractFunctionDecl(DeclKind::Destructor, Parent, NameHack,
                           DestructorLoc, ImplicitSelfDecl, nullptr) {
    assert(ImplicitSelfDecl && "destructors should have a non-null self");
  }

  SourceLoc getDestructorLoc() const { return getNameLoc(); }
  SourceLoc getStartLoc() const { return getDestructorLoc(); }
  SourceRange getSourceRange() const;

  /// \brief Compute and return the type of 'self'.
  Type computeSelfType(GenericParamList **OuterGenericParams = nullptr) const;

  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::Destructor;
  }
};

/// Abstract base class of operator declarations.
class OperatorDecl : public Decl {
  SourceLoc OperatorLoc, NameLoc, LBraceLoc, RBraceLoc;
  
  Identifier name;

public:
  OperatorDecl(DeclKind kind,
               DeclContext *DC,
               SourceLoc OperatorLoc,
               Identifier Name,
               SourceLoc NameLoc,
               SourceLoc LBraceLoc,
               SourceLoc RBraceLoc)
    : Decl(kind, DC),
      OperatorLoc(OperatorLoc), NameLoc(NameLoc),
      LBraceLoc(LBraceLoc), RBraceLoc(RBraceLoc),
      name(Name) {}
  
  SourceLoc getLoc() const { return NameLoc; }
  SourceRange getSourceRange() const { return {OperatorLoc, RBraceLoc}; }

  SourceLoc getOperatorLoc() const { return OperatorLoc; }
  SourceLoc getLBraceLoc() const { return LBraceLoc; }
  SourceLoc getRBraceLoc() const { return RBraceLoc; }
  Identifier getName() const { return name; }
  
  static bool classof(const Decl *D) {
    return D->getKind() >= DeclKind::First_OperatorDecl
      && D->getKind() <= DeclKind::Last_OperatorDecl;
  }
};

/// Declares the behavior of an infix operator. For example:
///
/// \code
/// operator infix /+/ {
///   associativity left
///   precedence 123
/// }
/// \endcode
class InfixOperatorDecl : public OperatorDecl {
  SourceLoc InfixLoc,
    AssociativityLoc, AssociativityValueLoc,
    PrecedenceLoc, PrecedenceValueLoc;

public:
  InfixOperatorDecl(DeclContext *DC,
                    SourceLoc OperatorLoc,
                    SourceLoc InfixLoc,
                    Identifier Name,
                    SourceLoc NameLoc,
                    SourceLoc LBraceLoc,
                    SourceLoc AssociativityLoc,
                    SourceLoc AssociativityValueLoc,
                    SourceLoc PrecedenceLoc,
                    SourceLoc PrecedenceValueLoc,
                    SourceLoc RBraceLoc,
                    InfixData InfixData)
    : OperatorDecl(DeclKind::InfixOperator, DC,
                   OperatorLoc,
                   Name,
                   NameLoc,
                   LBraceLoc,
                   RBraceLoc),
      InfixLoc(InfixLoc),
      AssociativityLoc(AssociativityLoc),
      AssociativityValueLoc(AssociativityValueLoc),
      PrecedenceLoc(PrecedenceLoc),
      PrecedenceValueLoc(PrecedenceValueLoc) {
    if (!InfixData.isValid()) {
      setInvalid();
    } else {
      InfixOperatorDeclBits.Precedence = InfixData.getPrecedence();
      InfixOperatorDeclBits.Associativity =
        static_cast<unsigned>(InfixData.getAssociativity());
    }
  }
  
  SourceLoc getInfixLoc() const { return InfixLoc; }
  SourceLoc getAssociativityLoc() const { return AssociativityLoc; }
  SourceLoc getAssociativityValueLoc() const { return AssociativityValueLoc; }
  SourceLoc getPrecedenceLoc() const { return PrecedenceLoc; }
  SourceLoc getPrecedenceValueLoc() const { return PrecedenceValueLoc; }

  unsigned getPrecedence() const {
    return InfixOperatorDeclBits.Precedence;
  }

  Associativity getAssociativity() const {
    return Associativity(InfixOperatorDeclBits.Associativity);
  }

  InfixData getInfixData() const {
    if (isInvalid())
      return InfixData();
    return InfixData(getPrecedence(), getAssociativity());
  }
  
  /// True if this decl's attributes conflict with those declared by another
  /// operator.
  bool conflictsWith(InfixOperatorDecl *other) {
    return getInfixData() != other->getInfixData();
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::InfixOperator;
  }
};
  
/// Declares the behavior of a prefix operator. For example:
///
/// \code
/// operator prefix /+/ {}
/// \endcode
class PrefixOperatorDecl : public OperatorDecl {
  SourceLoc PrefixLoc;
public:
  PrefixOperatorDecl(DeclContext *DC,
                     SourceLoc OperatorLoc,
                     SourceLoc PrefixLoc,
                     Identifier Name,
                     SourceLoc NameLoc,
                     SourceLoc LBraceLoc,
                     SourceLoc RBraceLoc)
    : OperatorDecl(DeclKind::PrefixOperator, DC,
                   OperatorLoc,
                   Name,
                   NameLoc,
                   LBraceLoc,
                   RBraceLoc),
      PrefixLoc(PrefixLoc) {}
  
  SourceLoc getPrefixLoc() const { return PrefixLoc; }
  
  /// True if this decl's attributes conflict with those declared by another
  /// PrefixOperatorDecl.
  bool conflictsWith(PrefixOperatorDecl *other) {
    return false;
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PrefixOperator;
  }
};
  
/// Declares the behavior of a postfix operator. For example:
///
/// \code
/// operator postfix /+/ {}
/// \endcode
class PostfixOperatorDecl : public OperatorDecl {
  SourceLoc PostfixLoc;
public:
  PostfixOperatorDecl(DeclContext *DC,
                     SourceLoc OperatorLoc,
                     SourceLoc PostfixLoc,
                     Identifier Name,
                     SourceLoc NameLoc,
                     SourceLoc LBraceLoc,
                     SourceLoc RBraceLoc)
    : OperatorDecl(DeclKind::PostfixOperator, DC,
                   OperatorLoc,
                   Name,
                   NameLoc,
                   LBraceLoc,
                   RBraceLoc),
      PostfixLoc(PostfixLoc) {}
  
  SourceLoc getPostfixLoc() const { return PostfixLoc; }

  /// True if this decl's attributes conflict with those declared by another
  /// PostfixOperatorDecl.
  bool conflictsWith(PostfixOperatorDecl *other) {
    return false;
  }
  
  static bool classof(const Decl *D) {
    return D->getKind() == DeclKind::PostfixOperator;
  }
};

inline void GenericParam::setDeclContext(DeclContext *DC) {
  TypeParam->setDeclContext(DC);
}

inline bool ValueDecl::isSettable() const {
  if (auto vd = dyn_cast<VarDecl>(this)) {
    return vd->isSettable();
  } else if (auto sd = dyn_cast<SubscriptDecl>(this)) {
    return sd->isSettable();
  } else
    return false;
}

inline bool NominalTypeDecl::isStoredProperty(VarDecl *vd) {
  return !vd->isComputed();
}

inline MutableArrayRef<Pattern *> AbstractFunctionDecl::getArgParamBuffer() {
  switch (getKind()) {
  case DeclKind::Constructor:
    return MutableArrayRef<Pattern *>(&cast<ConstructorDecl>(this)->ArgParams,
                                      1);

  case DeclKind::Destructor:
    return {};

  case DeclKind::Func: {
    auto *FD = cast<FuncDecl>(this);
    return MutableArrayRef<Pattern *>(reinterpret_cast<Pattern **>(FD + 1),
                                      FD->getNumParamPatternsImpl());
  }

  default:
    llvm_unreachable("unhandled derived decl kind");
  }
}

inline MutableArrayRef<Pattern *> AbstractFunctionDecl::getBodyParamBuffer() {
  switch (getKind()) {
  case DeclKind::Constructor:
    return MutableArrayRef<Pattern *>(&cast<ConstructorDecl>(this)->BodyParams,
                                      1);

  case DeclKind::Destructor:
    return {};

  case DeclKind::Func: {
    auto *FD = cast<FuncDecl>(this);
    unsigned NumParamPatterns = FD->getNumParamPatternsImpl();
    return MutableArrayRef<Pattern *>(
        reinterpret_cast<Pattern **>(FD + 1) + NumParamPatterns,
        NumParamPatterns);
  }

  default:
    llvm_unreachable("unhandled derived decl kind");
  }
}

} // end namespace swift

#endif
