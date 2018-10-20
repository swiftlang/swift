//===--- DeclContext.h - Swift Language Context ASTs ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the DeclContext class.  A DeclContext is the semantic
// construct that a declaration belongs to, such as the enclosing
// AbstractClosureExpr or declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLCONTEXT_H
#define SWIFT_DECLCONTEXT_H

#include "swift/AST/Identifier.h"
#include "swift/AST/LookupKinds.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/ExperimentalDependencies.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/MD5.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class AbstractFunctionDecl;
  class GenericEnvironment;
  class ASTContext;
  class ASTWalker;
  class CanType;
  class Decl;
  class DeclContext;
  class EnumDecl;
  class ExtensionDecl;
  class Expr;
  class GenericParamList;
  class LazyResolver;
  class LazyMemberLoader;
  class LazyMemberParser;
  class GenericSignature;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class ProtocolDecl;
  class Requirement;
  class SourceFile;
  class Type;
  class ModuleDecl;
  class GenericTypeDecl;
  class NominalTypeDecl;
  class ProtocolConformance;
  class ValueDecl;
  class Initializer;
  class ClassDecl;
  class SerializedAbstractClosureExpr;
  class SerializedPatternBindingInitializer;
  class SerializedDefaultArgumentInitializer;
  class SerializedTopLevelCodeDecl;
  class StructDecl;

namespace serialization {
using DeclID = llvm::PointerEmbeddedInt<unsigned, 31>;
}

enum class DeclContextKind : unsigned {
  AbstractClosureExpr,
  Initializer,
  TopLevelCodeDecl,
  SubscriptDecl,
  AbstractFunctionDecl,
  SerializedLocal,
  Last_LocalDeclContextKind = SerializedLocal,

  Module,
  FileUnit,
  GenericTypeDecl,
  ExtensionDecl,
  Last_DeclContextKind = ExtensionDecl
};

/// Kinds of DeclContexts after deserialization.
///
/// \see SerializedLocalDeclContext.
enum class LocalDeclContextKind : uint8_t {
  AbstractClosure,
  PatternBindingInitializer,
  DefaultArgumentInitializer,
  TopLevelCodeDecl
};

/// Describes the kind of a particular conformance.
///
/// The following code involves conformances of the three different kinds:
/// \code
/// protocol P { }
/// protocol P2 : P { }
///
/// class Super : P2 { }
/// class Sub : Super { }
/// \endcode
///
/// \c Super conforms to \c P2 via an explicit conformance,
/// specified on the class declaration itself.
///
/// \c Super conforms to \c P via an implied conformance, whose
/// origin is the explicit conformance to \c P2.
///
/// \c Sub conforms to \c P2 and \c P via inherited conformances,
/// which link back to the conformances described above.
///
/// The enumerators are ordered in terms of decreasing preference:
/// an inherited conformance is best, followed by explicit
/// conformances, then synthesized and implied conformances. Earlier
/// conformance kinds supersede later conformance kinds, possibly with a
/// diagnostic (e.g., if an inherited conformance supersedes an
/// explicit conformance).
enum class ConformanceEntryKind : unsigned {
  /// Inherited from a superclass conformance.
  Inherited,

  /// Explicitly specified.
  Explicit,

  /// Implicitly synthesized.
  Synthesized,

  /// Implied by an explicitly-specified conformance.
  Implied,
};

/// Describes the kind of conformance lookup desired.
enum class ConformanceLookupKind : unsigned {
  /// All conformances.
  All,
  /// Only the explicit conformance.
  OnlyExplicit,
};

/// Describes a diagnostic for a conflict between two protocol
/// conformances.
struct ConformanceDiagnostic {
  /// The protocol with conflicting conformances.
  ProtocolDecl *Protocol;

  /// The location at which the diagnostic should occur.
  SourceLoc Loc;

  /// The kind of conformance that was superseded.
  ConformanceEntryKind Kind;

  /// The explicitly-specified protocol whose conformance implied the
  /// conflicting conformance.
  ProtocolDecl *ExplicitProtocol;

  /// The declaration context that declares the existing conformance.
  DeclContext *ExistingDC;

  /// The kind of existing conformance.
  ConformanceEntryKind ExistingKind;

  /// The explicitly-specified protocol whose conformance implied the
  /// existing conflicting conformance.
  ProtocolDecl *ExistingExplicitProtocol;
};

/// A DeclContext is an AST object which acts as a semantic container
/// for declarations.  As a policy matter, we currently define
/// contexts broadly: a lambda expression in a function is a new
/// DeclContext, but a new brace statement is not.  There's no
/// particular mandate for this, though.
///
/// Please note that DeclContext assumes that it prefaces AST type hierarchies
/// and therefore can safely access trailing memory. If you need to create a
/// macro context, please see GenericContext for how to minimize new entries in
/// the ASTHierarchy enum below.
class alignas(1 << DeclContextAlignInBits) DeclContext {
  enum class ASTHierarchy : unsigned {
    Decl,
    Expr,
    FileUnit,
    Initializer,
    SerializedLocal,
    // If you add a new AST hierarchies, then update the static_assert() below.
  };
  static_assert(unsigned(ASTHierarchy::SerializedLocal) <
                (1 << DeclContextAlignInBits),
                "ASTHierarchy exceeds bits available");

  llvm::PointerIntPair<DeclContext*, 3, ASTHierarchy> ParentAndKind;

  /// Change the parent of this context.  This should only be used
  /// very carefully.
  void setParent(DeclContext *parent) { ParentAndKind.setPointer(parent); }
  friend class Initializer; // uses setParent
  friend class AutoClosureExpr; // uses setParent
  friend class AbstractClosureExpr; // uses setParent
  
  template<class A, class B, class C>
  friend struct ::llvm::cast_convert_val;
  
  // See swift/AST/Decl.h
  static DeclContext *castDeclToDeclContext(const Decl *D);

  /// If this DeclContext is a GenericType declaration or an
  /// extension thereof, return the GenericTypeDecl.
  GenericTypeDecl *getSelfTypeDecl() const;

  static ASTHierarchy getASTHierarchyFromKind(DeclContextKind Kind) {
    switch (Kind) {
    case DeclContextKind::AbstractClosureExpr:
      return ASTHierarchy::Expr;
    case DeclContextKind::Initializer:
      return ASTHierarchy::Initializer;
    case DeclContextKind::SerializedLocal:
      return ASTHierarchy::SerializedLocal;
    case DeclContextKind::FileUnit:
      return ASTHierarchy::FileUnit;
    case DeclContextKind::Module:
    case DeclContextKind::TopLevelCodeDecl:
    case DeclContextKind::AbstractFunctionDecl:
    case DeclContextKind::SubscriptDecl:
    case DeclContextKind::GenericTypeDecl:
    case DeclContextKind::ExtensionDecl:
      return ASTHierarchy::Decl;
    }
    llvm_unreachable("Unhandled DeclContextKind");
  }

public:
  LLVM_READONLY
  Decl *getAsDecl() {
    return ParentAndKind.getInt() == ASTHierarchy::Decl ?
      reinterpret_cast<Decl*>(this + 1) : nullptr;
  }
  const Decl *getAsDecl() const {
    return const_cast<DeclContext*>(this)->getAsDecl();
  }

  DeclContext(DeclContextKind Kind, DeclContext *Parent)
      : ParentAndKind(Parent, getASTHierarchyFromKind(Kind)) {
    if (Kind != DeclContextKind::Module)
      assert(Parent != nullptr && "DeclContext must have a parent context");
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const;
  
  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() <= DeclContextKind::Last_LocalDeclContextKind;
  }
  
  /// isModuleContext - Return true if this is a subclass of Module.
  LLVM_READONLY
  bool isModuleContext() const; // see swift/AST/Module.h

  /// \returns true if this is a context with module-wide scope, e.g. a module
  /// or a source file.
  LLVM_READONLY
  bool isModuleScopeContext() const; // see swift/AST/Module.h

  /// \returns true if this is a type context, e.g., a struct, a class, an
  /// enum, a protocol, or an extension.
  LLVM_READONLY
  bool isTypeContext() const;

  /// \brief Determine whether this is an extension context.
  LLVM_READONLY
  bool isExtensionContext() const; // see swift/AST/Decl.h

  /// If this DeclContext is a NominalType declaration or an
  /// extension thereof, return the NominalTypeDecl.
  LLVM_READONLY
  NominalTypeDecl *getSelfNominalTypeDecl() const;

  /// If this DeclContext is a class, or an extension on a class, return the
  /// ClassDecl, otherwise return null.
  LLVM_READONLY
  ClassDecl *getSelfClassDecl() const;

  /// If this DeclContext is an enum, or an extension on an enum, return the
  /// EnumDecl, otherwise return null.
  LLVM_READONLY
  EnumDecl *getSelfEnumDecl() const;

  /// If this DeclContext is a struct, or an extension on a struct, return the
  /// StructDecl, otherwise return null.
  LLVM_READONLY
  StructDecl *getSelfStructDecl() const;

  /// If this DeclContext is a protocol, or an extension on a
  /// protocol, return the ProtocolDecl, otherwise return null.
  LLVM_READONLY
  ProtocolDecl *getSelfProtocolDecl() const;

  /// If this DeclContext is a protocol extension, return the extended protocol.
  LLVM_READONLY
  ProtocolDecl *getExtendedProtocolDecl() const;

  /// \brief Retrieve the generic parameter 'Self' from a protocol or
  /// protocol extension.
  ///
  /// Only valid if \c getSelfProtocolDecl().
  GenericTypeParamType *getProtocolSelfType() const;

  /// Gets the type being declared by this context.
  ///
  /// - Generic types return a bound generic type using archetypes.
  /// - Non-type contexts return a null type.
  Type getDeclaredTypeInContext() const;
  
  /// Gets the type being declared by this context.
  ///
  /// - Generic types return a bound generic type using interface types.
  /// - Non-type contexts return a null type.
  Type getDeclaredInterfaceType() const;

  /// Get the type of `self` in this context.
  ///
  /// - Protocol types return the `Self` archetype.
  /// - Everything else falls back on getDeclaredTypeInContext().
  Type getSelfTypeInContext() const;

  /// Get the type of `self` in this context.
  ///
  /// - Protocol types return the `Self` interface type.
  /// - Everything else falls back on getDeclaredInterfaceType().
  Type getSelfInterfaceType() const;

  /// \brief Retrieve the innermost generic parameters of this context or any
  /// of its parents.
  ///
  /// FIXME: Remove this
  GenericParamList *getGenericParamsOfContext() const;

  /// \brief Retrieve the innermost generic signature of this context or any
  /// of its parents.
  GenericSignature *getGenericSignatureOfContext() const;

  /// \brief Retrieve the innermost archetypes of this context or any
  /// of its parents.
  GenericEnvironment *getGenericEnvironmentOfContext() const;

  /// Whether the context has a generic environment that will be constructed
  /// on first access (but has not yet been constructed).
  bool contextHasLazyGenericEnvironment() const;

  /// Map an interface type to a contextual type within this context.
  Type mapTypeIntoContext(Type type) const;

  /// Returns this or the first local parent context, or nullptr if it is not
  /// contained in one.
  LLVM_READONLY
  DeclContext *getLocalContext();
  const DeclContext *getLocalContext() const {
    return const_cast<DeclContext*>(this)->getLocalContext();
  }

  /// Returns the innermost method context.
  ///
  /// This routine looks through closure, initializer, and local function
  /// contexts to find the innermost method (including constructors and
  /// destructors).
  ///
  /// \returns the innermost method, or null if there is no such method.
  LLVM_READONLY
  AbstractFunctionDecl *getInnermostMethodContext();
  const AbstractFunctionDecl *getInnermostMethodContext() const {
    return const_cast<DeclContext*>(this)->getInnermostMethodContext();
  }

  /// Returns the innermost type context.
  ///
  /// This routine looks through closure, initializer, and local function
  /// contexts to find the innermost type context -- nominal type or
  /// extension.
  LLVM_READONLY
  DeclContext *getInnermostTypeContext();
  const DeclContext *getInnermostTypeContext() const {
    return const_cast<DeclContext *>(this)->getInnermostTypeContext();
  }

  /// Returns the innermost context that is a declaration.
  ///
  /// This routine looks through contexts to find the innermost
  /// declaration context that is itself a declaration.
  LLVM_READONLY
  Decl *getInnermostDeclarationDeclContext();
  const Decl *getInnermostDeclarationDeclContext() const {
    return
        const_cast<DeclContext *>(this)->getInnermostDeclarationDeclContext();
  }

  /// Returns the semantic parent of this context.  A context has a
  /// parent if and only if it is not a module context.
  DeclContext *getParent() const {
    return ParentAndKind.getPointer();
  }

  /// Returns the semantic parent for purposes of name lookup.
  DeclContext *getParentForLookup() const;

  /// Return true if this is a child of the specified other decl context.
  bool isChildContextOf(const DeclContext *Other) const {
    if (this == Other) return false;

    for (const DeclContext *CurContext = this; CurContext;
         CurContext = CurContext->getParent())
      if (CurContext == Other)
        return true;
    return false;
  }

  /// Returns the module context that contains this context.
  LLVM_READONLY
  ModuleDecl *getParentModule() const;

  /// Returns the module scope context that contains this context.
  ///
  /// This is either a \c Module or a \c FileUnit.
  LLVM_READONLY
  DeclContext *getModuleScopeContext() const;

  /// Returns the source file that contains this context, or null if this
  /// is not within a source file.
  LLVM_READONLY
  SourceFile *getParentSourceFile() const;

  /// Determine whether this declaration context is generic, meaning that it or
  /// any of its parents have generic parameters.
  bool isGenericContext() const;

  /// Determine whether the innermost context is generic.
  bool isInnermostContextGeneric() const;

  /// Get the most optimal resilience expansion for code in this context.
  /// If the body is able to be inlined into functions in other resilience
  /// domains, this ensures that only sufficiently-conservative access patterns
  /// are used.
  ResilienceExpansion getResilienceExpansion() const;

  /// Returns true if lookups within this context could affect downstream files.
  ///
  /// \param functionsAreNonCascading If true, functions are considered non-
  /// cascading contexts. If false, functions are considered non-cascading only
  /// if implicitly or explicitly marked private. When concerned only with a
  /// function's body, pass true.
  bool isCascadingContextForLookup(bool functionsAreNonCascading) const;

  /// Look for the set of declarations with the given name within a type,
  /// its extensions and, optionally, its supertypes.
  ///
  /// This routine performs name lookup within a given type, its extensions
  /// and, optionally, its supertypes and their extensions, from the perspective
  /// of the current DeclContext. It can eliminate non-visible, hidden, and
  /// overridden declarations from the result set. It does not, however, perform
  /// any filtering based on the semantic usefulness of the results.
  ///
  /// \param type The type to look into.
  ///
  /// \param member The member to search for.
  ///
  /// \param options Options that control name lookup, based on the
  /// \c NL_* constants in \c NameLookupOptions.
  ///
  /// \param typeResolver Used to resolve types, usually for overload purposes.
  /// May be null.
  ///
  /// \param[out] decls Will be populated with the declarations found by name
  /// lookup.
  ///
  /// \returns true if anything was found.
  bool lookupQualified(Type type, DeclName member, NLOptions options,
                       LazyResolver *typeResolver,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Look for the set of declarations with the given name within the
  /// given set of nominal type declarations.
  ///
  /// \param types The type declarations to look into.
  ///
  /// \param member The member to search for.
  ///
  /// \param options Options that control name lookup, based on the
  /// \c NL_* constants in \c NameLookupOptions.
  ///
  /// \param[out] decls Will be populated with the declarations found by name
  /// lookup.
  ///
  /// \returns true if anything was found.
  bool lookupQualified(ArrayRef<NominalTypeDecl *> types, DeclName member,
                       NLOptions options,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Perform qualified lookup for the given member in the given module.
  bool lookupQualified(ModuleDecl *module, DeclName member, NLOptions options,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Perform \c AnyObject lookup for the given member.
  bool lookupAnyObject(DeclName member, NLOptions options,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Look up all Objective-C methods with the given selector visible
  /// in the enclosing module.
  void lookupAllObjCMethods(
         ObjCSelector selector,
         SmallVectorImpl<AbstractFunctionDecl *> &results) const;

  /// Return the ASTContext for a specified DeclContext by
  /// walking up to the enclosing module and returning its ASTContext.
  LLVM_READONLY
  ASTContext &getASTContext() const;

  bool getEnableExperimentalDependencies() const;

  /// Retrieve the set of protocols whose conformances will be
  /// associated with this declaration context.
  ///
  /// This function differs from \c getLocalConformances() in that it
  /// returns protocol declarations, not protocol conformances, and
  /// therefore does not require the protocol conformances to be
  /// formed.
  ///
  /// \param lookupKind The kind of lookup to perform.
  ///
  /// \param diagnostics If non-null, will be populated with the set of
  /// diagnostics that should be emitted for this declaration context.
  ///
  /// \param sorted Whether to sort the results in a canonical order.
  ///
  /// FIXME: This likely makes more sense on IterableDeclContext or
  /// something similar.
  SmallVector<ProtocolDecl *, 2>
  getLocalProtocols(ConformanceLookupKind lookupKind
                      = ConformanceLookupKind::All,
                    SmallVectorImpl<ConformanceDiagnostic> *diagnostics
                      = nullptr,
                    bool sorted = false) const;

  /// Retrieve the set of protocol conformances associated with this
  /// declaration context.
  ///
  /// \param lookupKind The kind of lookup to perform.
  ///
  /// \param diagnostics If non-null, will be populated with the set of
  /// diagnostics that should be emitted for this declaration context.
  ///
  /// \param sorted Whether to sort the results in a canonical order.
  ///
  /// FIXME: This likely makes more sense on IterableDeclContext or
  /// something similar.
  SmallVector<ProtocolConformance *, 2>
  getLocalConformances(ConformanceLookupKind lookupKind
                         = ConformanceLookupKind::All,
                       SmallVectorImpl<ConformanceDiagnostic> *diagnostics
                         = nullptr,
                       bool sorted = false) const;

  /// Retrieve the syntactic depth of this declaration context, i.e.,
  /// the number of non-module-scoped contexts.
  ///
  /// For an extension of a nested type, the extension is depth 1.
  unsigned getSyntacticDepth() const;

  /// Retrieve the semantic depth of this declaration context, i.e.,
  /// the number of non-module-scoped contexts.
  ///
  /// For an extension of a nested type, the depth of the nested type itself
  /// is also included.
  unsigned getSemanticDepth() const;

  /// \returns true if traversal was aborted, false otherwise.
  bool walkContext(ASTWalker &Walker);

  void dumpContext() const;
  unsigned printContext(llvm::raw_ostream &OS, unsigned indent = 0) const;

  // Only allow allocation of DeclContext using the allocator in ASTContext.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(DeclContext));
  
  // Some Decls are DeclContexts, but not all. See swift/AST/Decl.h
  static bool classof(const Decl *D);
  
  ExperimentalDependencies::unimpLocation_t  updateExpDepHash(llvm::MD5& hash) const;
};

/// SerializedLocalDeclContext - the base class for DeclContexts that were
/// serialized to preserve AST structure and accurate mangling after
/// deserialization.
class SerializedLocalDeclContext : public DeclContext {
private:
  unsigned LocalKind : 3;

protected:
  unsigned SpareBits : 29;

public:
  SerializedLocalDeclContext(LocalDeclContextKind LocalKind,
                             DeclContext *Parent)
    : DeclContext(DeclContextKind::SerializedLocal, Parent),
      LocalKind(static_cast<unsigned>(LocalKind)) {}

  LocalDeclContextKind getLocalDeclContextKind() const {
    return static_cast<LocalDeclContextKind>(LocalKind);
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::SerializedLocal;
  }
  ExperimentalDependencies::unimpLocation_t  updateExpDepHashInner(llvm::MD5& hash) const;
};

/// An iterator that walks through a list of declarations stored
/// within some iterable declaration context.
class DeclIterator {
  Decl *Current;

public: 
  typedef std::forward_iterator_tag iterator_category;
  typedef Decl *value_type;
  typedef Decl *reference;
  typedef Decl *pointer; // Non-standard but convenient
  typedef std::ptrdiff_t difference_type;

  DeclIterator(Decl *current = nullptr) : Current(current) { }

  reference operator*() const { return Current; }
  pointer operator->() const { return Current; }

  DeclIterator &operator++();

  DeclIterator operator++(int) {
    DeclIterator old = *this;
    ++*this;
    return old;
  }

  friend bool operator==(DeclIterator lhs, DeclIterator rhs) {
    return lhs.Current == rhs.Current; 
  }

  friend bool operator!=(DeclIterator lhs, DeclIterator rhs) {
    return !(lhs == rhs);
  }
};

/// The range of declarations stored within an iterable declaration
/// context.
typedef IteratorRange<DeclIterator> DeclRange;

/// The kind of an \c IterableDeclContext.
enum class IterableDeclContextKind : uint8_t {  
  NominalTypeDecl = 0,
  ExtensionDecl,
};

/// A declaration context that tracks the declarations it (directly)
/// owns and permits iteration over them.
///
/// Note that an iterable declaration context must inherit from both
/// \c IterableDeclContext and \c DeclContext.
class IterableDeclContext {
  enum LazyMembers : unsigned {
    Present = 1 << 0,

    /// Lazy member loading has a variety of feedback loops that need to
    /// switch to pseudo-empty-member behaviour to avoid infinite recursion;
    /// we use this flag to control them.
    InProgress = 1 << 1,
  };

  /// The first declaration in this context along with a bit indicating whether
  /// the members of this context will be lazily produced.
  mutable llvm::PointerIntPair<Decl *, 2, LazyMembers> FirstDeclAndLazyMembers;

  /// The last declaration in this context, used for efficient insertion,
  /// along with the kind of iterable declaration context.
  mutable llvm::PointerIntPair<Decl *, 1, IterableDeclContextKind>
    LastDeclAndKind;

  /// The DeclID this IDC was deserialized from, if any. Used for named lazy
  /// member loading, as a key when doing lookup in this IDC.
  serialization::DeclID SerialID;

  template<class A, class B, class C>
  friend struct ::llvm::cast_convert_val;

  static IterableDeclContext *castDeclToIterableDeclContext(const Decl *D);

  /// Retrieve the \c ASTContext in which this iterable context occurs.
  ASTContext &getASTContext() const;

public:
  IterableDeclContext(IterableDeclContextKind kind)
    : LastDeclAndKind(nullptr, kind) { }

  /// Determine the kind of iterable context we have.
  IterableDeclContextKind getIterableContextKind() const {
    return LastDeclAndKind.getInt();
  }

  /// Retrieve the set of members in this context.
  DeclRange getMembers() const;

  /// Retrieve the set of members in this context without loading any from the
  /// associated lazy loader; this should only be used as part of implementing
  /// abstractions on top of member loading, such as a name lookup table.
  DeclRange getCurrentMembersWithoutLoading() const;

  /// Add a member to this context. If the hint decl is specified, the new decl
  /// is inserted immediately after the hint.
  void addMember(Decl *member, Decl *hint = nullptr);

  /// Check whether there are lazily-loaded members.
  bool hasLazyMembers() const {
    return FirstDeclAndLazyMembers.getInt() & LazyMembers::Present;
  }

  bool isLoadingLazyMembers() {
    return FirstDeclAndLazyMembers.getInt() & LazyMembers::InProgress;
  }

  void setLoadingLazyMembers(bool inProgress) {
    LazyMembers status = FirstDeclAndLazyMembers.getInt();
    if (inProgress)
      status = LazyMembers(status | LazyMembers::InProgress);
    else
      status = LazyMembers(status & ~LazyMembers::InProgress);
    FirstDeclAndLazyMembers.setInt(status);
  }

  /// Setup the loader for lazily-loaded members.
  void setMemberLoader(LazyMemberLoader *loader, uint64_t contextData);

  /// Load all of the members of this context.
  void loadAllMembers() const;

  /// Determine whether this was deserialized (and thus SerialID is
  /// valid).
  bool wasDeserialized() const;

  /// Return 'this' as a \c Decl.
  const Decl *getDecl() const;

  /// Get the DeclID this Decl was deserialized from.
  serialization::DeclID getDeclID() const {
    assert(wasDeserialized());
    return SerialID;
  }

  /// Set the DeclID this Decl was deserialized from.
  void setDeclID(serialization::DeclID d) {
    assert(wasDeserialized());
    SerialID = d;
  }

  // Some Decls are IterableDeclContexts, but not all.
  static bool classof(const Decl *D);

  ExperimentalDependencies::unimpLocation_t  updateExpDepHash(llvm::MD5& hash) const;

private:
  /// Add a member to the list for iteration purposes, but do not notify the
  /// subclass that we have done so.
  ///
  /// This is used internally when loading members, because loading a
  /// member is an invisible addition.
  void addMemberSilently(Decl *member, Decl *hint = nullptr) const;
};
  
} // end namespace swift

namespace llvm {
  template<class FromTy>
  struct cast_convert_val< ::swift::DeclContext, FromTy, FromTy> {
    static ::swift::DeclContext *doit(const FromTy &Val) {
      return ::swift::DeclContext::castDeclToDeclContext(Val);
    }
  };

  template<class FromTy>
  struct cast_convert_val< ::swift::IterableDeclContext, FromTy, FromTy> {
    static ::swift::IterableDeclContext *doit(const FromTy &Val) {
      return ::swift::IterableDeclContext::castDeclToIterableDeclContext(Val);
    }
  };
}

#endif
