//===--- DeclContext.h - Swift Language Context ASTs ------------*- C++ -*-===//
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
// This file defines the DeclContext class.  A DeclContext is the semantic
// construct that a declaration belongs to, such as the enclosing
// AbstractClosureExpr or declaration.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DECLCONTEXT_H
#define SWIFT_DECLCONTEXT_H

#include "swift/AST/Identifier.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class AbstractFunctionDecl;
  class ASTContext;
  class ASTWalker;
  class CanType;
  class Decl;
  class DeclContext;
  class ExtensionDecl;
  class Expr;
  class GenericParamList;
  class LazyResolver;
  class LazyMemberLoader;
  class GenericSignature;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class ProtocolDecl;
  class Requirement;
  class SourceFile;
  class Type;
  class ModuleDecl;
  class NominalTypeDecl;
  class ProtocolConformance;
  class ValueDecl;
  class Initializer;
  class ClassDecl;
  class SerializedAbstractClosureExpr;
  class SerializedPatternBindingInitializer;
  class SerializedDefaultArgumentInitializer;
  class SerializedTopLevelCodeDecl;

enum class DeclContextKind : uint8_t {
  AbstractClosureExpr,
  Initializer,
  TopLevelCodeDecl,
  AbstractFunctionDecl,
  SerializedLocal,
  Last_LocalDeclContextKind = SerializedLocal,

  Module,
  FileUnit,
  NominalTypeDecl,
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
/// conformances, then implied conformances. Earlier conformance
/// kinds supersede later conformance kinds, possibly with a
/// diagnostic (e.g., if an inherited conformance supersedes an
/// explicit conformance).
enum class ConformanceEntryKind : unsigned {
  /// Inherited from a superclass conformance.
  Inherited,

  /// Explicitly specified.
  Explicit,

  /// Implied by an explicitly-specified conformance.
  Implied,

  /// Implicitly synthesized.
  Synthesized,
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
/// Note that DeclContexts have stricter alignment requirements than AST nodes
/// in general, so if an AST node class multiply inherits from DeclContext
/// and another base class, it must 'using DeclContext::operator new;' in order
/// to use an allocator with the correct alignment.
class alignas(1 << DeclContextAlignInBits) DeclContext {
  
  enum {
    KindBits = DeclContextAlignInBits
  };
  static_assert(unsigned(DeclContextKind::Last_DeclContextKind) < 1U<<KindBits,
                "Not enough KindBits for DeclContextKind");
  
  llvm::PointerIntPair<DeclContext*, KindBits, DeclContextKind> ParentAndKind;

  /// Change the parent of this context.  This should only be used
  /// very carefully.
  void setParent(DeclContext *parent) { ParentAndKind.setPointer(parent); }
  friend class Initializer; // uses setParent
  friend class AutoClosureExpr; // uses setParent
  friend class AbstractClosureExpr; // uses setParent
  
  template<class A, class B, class C>
  friend struct ::llvm::cast_convert_val;
  
  static const DeclContext *castDeclToDeclContext(const ValueDecl *D);
  
public:
  DeclContext(DeclContextKind Kind, DeclContext *Parent)
    : ParentAndKind(Parent, Kind) {
    assert((Parent != 0 || isModuleContext()) &&
           "DeclContext must have a parent unless it is a module!");
  }

  /// Returns the kind of context this is.
  DeclContextKind getContextKind() const {
    return ParentAndKind.getInt();
  }
  
  /// Determines whether this context is itself a local scope in a
  /// code block.  A context that appears in such a scope, like a
  /// local type declaration, does not itself become a local context.
  bool isLocalContext() const {
    return getContextKind() <= DeclContextKind::Last_LocalDeclContextKind;
  }
  
  /// isModuleContext - Return true if this is a subclass of Module.
  bool isModuleContext() const {
    return getContextKind() == DeclContextKind::Module;
  }

  /// \returns true if this is a context with module-wide scope, e.g. a module
  /// or a source file.
  bool isModuleScopeContext() const {
    return getContextKind() == DeclContextKind::Module ||
           getContextKind() == DeclContextKind::FileUnit;
  }

  /// \returns true if this is a type context, e.g., a struct, a class, an
  /// enum, a protocol, or an extension.
  bool isTypeContext() const {
    return getContextKind() == DeclContextKind::NominalTypeDecl ||
           getContextKind() == DeclContextKind::ExtensionDecl;
  }

  /// \brief Determine whether this is an extension context.
  bool isExtensionContext() const {
    return getContextKind() == DeclContextKind::ExtensionDecl;
  }

  /// If this DeclContext is a nominal type declaration or an
  /// extension thereof, return the nominal type declaration.
  NominalTypeDecl *isNominalTypeOrNominalTypeExtensionContext() const;

  /// If this DeclContext is a class, or an extension on a class, return the
  /// ClassDecl, otherwise return null.
  ClassDecl *isClassOrClassExtensionContext() const;

  /// If this DeclContext is a protocol, or an extension on a
  /// protocol, return the ProtocolDecl, otherwise return null.
  ProtocolDecl *isProtocolOrProtocolExtensionContext() const;

  /// If this DeclContext is a protocol extension, return the extended protocol.
  ProtocolDecl *isProtocolExtensionContext() const;

  /// \brief Retrieve the generic parameter 'Self' from a protocol or
  /// protocol extension.
  ///
  /// Only valid if \c isProtocolOrProtocolExtensionContext().
  GenericTypeParamDecl *getProtocolSelf() const;

  /// getDeclaredTypeOfContext - For a type context, retrieves the declared
  /// type of the context. Returns a null type for non-type contexts.
  Type getDeclaredTypeOfContext() const;

  /// getDeclaredTypeInContext - For a type context, retrieves the declared
  /// type of the context as visible from within the context. Returns a null
  /// type for non-type contexts.
  Type getDeclaredTypeInContext() const;
  
  /// getDeclaredInterfaceType - For a type context, retrieves the interface
  /// type of the context as seen from outside the context. Returns a null
  /// type for non-type contexts.
  Type getDeclaredInterfaceType() const;

  /// \brief Retrieve the innermost generic parameters introduced by this
  /// context or one of its parent contexts, or null if this context is not
  /// directly dependent on any generic parameters.
  GenericParamList *getGenericParamsOfContext() const;

  /// \brief Retrieve the interface generic type parameters and requirements
  /// exposed by this context.
  GenericSignature *getGenericSignatureOfContext() const;
  
  /// Returns this or the first local parent context, or nullptr if it is not
  /// contained in one.
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
  AbstractFunctionDecl *getInnermostMethodContext();
  const AbstractFunctionDecl *getInnermostMethodContext() const {
    return const_cast<DeclContext*>(this)->getInnermostMethodContext();
  }

  /// Returns the innermost type context.
  ///
  /// This routine looks through closure, initializer, and local function
  /// contexts to find the innermost type context -- nominal type or
  /// extension.
  DeclContext *getInnermostTypeContext();
  const DeclContext *getInnermostTypeContext() const {
    return const_cast<DeclContext *>(this)->getInnermostTypeContext();
  }

  /// Returns the innermost context that is a declaration.
  ///
  /// This routine looks through contexts to find the innermost
  /// declaration context that is itself a declaration.
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
  ModuleDecl *getParentModule() const;

  /// Returns the module scope context that contains this context.
  ///
  /// This is either a \c Module or a \c FileUnit.
  DeclContext *getModuleScopeContext() const;

  /// Returns the source file that contains this context, or null if this
  /// is not within a source file.
  SourceFile *getParentSourceFile() const;

  /// Determine whether this declaration context is generic, meaning that it or
  /// any of its parents have generic parameters.
  bool isGenericContext() const;

  /// Determine whether the innermost context is generic.
  bool isInnermostContextGeneric() const;

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
  bool lookupQualified(Type type, DeclName member, unsigned options,
                       LazyResolver *typeResolver,
                       SmallVectorImpl<ValueDecl *> &decls) const;

  /// Return the ASTContext for a specified DeclContext by
  /// walking up to the enclosing module and returning its ASTContext.
  ASTContext &getASTContext() const;

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
  /// FIXME: This likely makes more sense on IterableDeclContext or
  /// something similar.
  SmallVector<ProtocolDecl *, 2>
  getLocalProtocols(ConformanceLookupKind lookupKind
                      = ConformanceLookupKind::All,
                    SmallVectorImpl<ConformanceDiagnostic> *diagnostics
                      = nullptr) const;

  /// Retrieve the set of protocol conformances associated with this
  /// declaration context.
  ///
  /// \param lookupKind The kind of lookup to perform.
  ///
  /// \param diagnostics If non-null, will be populated with the set of
  /// diagnostics that should be emitted for this declaration context.
  ///
  /// FIXME: This likely makes more sense on IterableDeclContext or
  /// something similar.
  SmallVector<ProtocolConformance *, 2>
  getLocalConformances(ConformanceLookupKind lookupKind
                         = ConformanceLookupKind::All,
                       SmallVectorImpl<ConformanceDiagnostic> *diagnostics
                         = nullptr) const;

  /// \returns true if traversal was aborted, false otherwise.
  bool walkContext(ASTWalker &Walker);

  void dumpContext() const;
  unsigned printContext(llvm::raw_ostream &OS, unsigned indent = 0) const;
  
  // Only allow allocation of DeclContext using the allocator in ASTContext.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(DeclContext));
  
  // Some Decls are DeclContexts, but not all.
  static bool classof(const ValueDecl *D);
};

/// SerializedLocalDeclContext - the base class for DeclContexts that were
/// serialized to preserve AST structure and accurate mangling after
/// deserialization.
class SerializedLocalDeclContext : public DeclContext {
private:
  const LocalDeclContextKind LocalKind;

public:
  SerializedLocalDeclContext(LocalDeclContextKind LocalKind,
                             DeclContext *Parent)
    : DeclContext(DeclContextKind::SerializedLocal, Parent),
      LocalKind(LocalKind) {}

  LocalDeclContextKind getLocalDeclContextKind() const {
    return LocalKind;
  }

  static bool classof(const DeclContext *DC) {
    return DC->getContextKind() == DeclContextKind::SerializedLocal;
  }
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
  NominalTypeDecl,
  ExtensionDecl,
};

/// A declaration context that tracks the declarations it (directly)
/// owns and permits iteration over them.
///
/// Note that an iterable declaration context must inherit from both
/// \c IterableDeclContext and \c DeclContext.
class IterableDeclContext {
  /// The first declaration in this context.
  mutable Decl *FirstDecl = nullptr;

  /// The last declaration in this context, used for efficient insertion,
  /// along with the kind of iterable declaration context.
  mutable llvm::PointerIntPair<Decl *, 2, IterableDeclContextKind> 
    LastDeclAndKind;

  /// Lazy member loader, if any.
  ///
  /// FIXME: Can we collapse this storage into something?
  mutable LazyMemberLoader *LazyLoader = nullptr;

  /// Lazy member loader context data.
  uint64_t LazyLoaderContextData = 0;

public:
  IterableDeclContext(IterableDeclContextKind kind)
    : LastDeclAndKind(nullptr, kind) { }

  /// Determine the kind of iterable context we have.
  IterableDeclContextKind getIterableContextKind() const {
    return LastDeclAndKind.getInt();
  }

  /// Retrieve the set of members in this context.
  DeclRange getMembers() const;

  /// Add a member to this context. If the hint decl is specified, the new decl
  /// is inserted immediately after the hint.
  void addMember(Decl *member, Decl *hint = nullptr);

  /// Retrieve the lazy member loader.
  LazyMemberLoader *getLoader() const {
    assert(isLazy());
    return LazyLoader;
  }

  /// Retrieve the context data for the lazy member loader.
  uint64_t getLoaderContextData() const {
    assert(isLazy());
    return LazyLoaderContextData;
  }

  /// Check whether there are lazily-loaded members.
  bool isLazy() const {
    return LazyLoader != nullptr;
  }  

  /// Set the loader for lazily-loaded members.
  void setLoader(LazyMemberLoader *loader, uint64_t contextData);

  /// Load all of the members of this context.
  void loadAllMembers() const;

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
    static const ::swift::DeclContext *doit(const FromTy &Val) {
      return ::swift::DeclContext::castDeclToDeclContext(Val);
    }
  };
}

#endif
