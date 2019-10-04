//===--- GenericSignature.h - Generic Signature AST -------------*- C++ -*-===//
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
// This file defines the GenericSignature class and its related classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_GENERIC_SIGNATURE_H
#define SWIFT_AST_GENERIC_SIGNATURE_H

#include "swift/AST/PrintOptions.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Type.h"
#include "swift/Basic/AnyValue.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"
#include <utility>

namespace swift {

class GenericSignatureBuilder;
class ProtocolConformanceRef;
class ProtocolType;
class SubstitutionMap;
class GenericEnvironment;

/// An access path used to find a particular protocol conformance within
/// a generic signature.
///
/// One can follow a conformance path to extract any conformance that is
/// derivable within the generic signature. For example, given:
///
/// \code
///   func f<C: Collection>(_: C) where C.Iterator.Element: Hashable { }
/// \endcode
///
/// One can extract conformances for various types and protocols, including
/// those written directly (\c C: Collection, \c C.Iterator.Element: Hashable),
/// and others that can be derived (\c C: Sequence,
/// \c C.Iterator: IteratorProtocol, \c C.Iterator.Element: Equatable).
///
/// A conformance access path is a sequence of (dependent type, protocol decl)
/// pairs that starts at an explicit requirement in the generic signature
/// (e.g., \c C: Collection). Each subsequent step names a dependent
/// type and protocol that refers to an explicit requirement in the requirement
/// signature of the previous step's protocol. For example, consider the
/// derived conformance \c C.Iterator: IteratorProtocol, which has the
/// following path:
///
/// \code
/// (C, Collection) -> (Self, Sequence) -> (Self.Iterator, IteratorProtocol)
/// \endcode
///
/// Therefore, the path starts at \c C: Collection. It then retrieves the
/// \c Sequence conformance of \c C (because \c Collection inherits
/// \c Sequence). Finally, it extracts the conformance of the associated type
/// \c Iterator to \c IteratorProtocol from the \c Sequence protocol.
class ConformanceAccessPath {
public:
  /// An entry in the conformance access path, which is described by the
  /// dependent type on which the conformance is stated as the protocol to
  /// which.
  typedef std::pair<Type, ProtocolDecl *> Entry;

private:
  ArrayRef<Entry> path;

  ConformanceAccessPath(ArrayRef<Entry> path) : path(path) {}

  friend class GenericSignatureImpl;

public:
  typedef const Entry *const_iterator;
  typedef const_iterator iterator;

  const_iterator begin() const { return path.begin(); }
  const_iterator end() const { return path.end(); }

  void print(raw_ostream &OS) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in a debugger");
};

class GenericSignatureImpl;
class GenericTypeParamType;

/// Describes the generic signature of a particular declaration, including
/// both the generic type parameters and the requirements placed on those
/// generic parameters.
class GenericSignature {
  GenericSignatureImpl *Ptr;

public:
  /// Create a new generic signature with the given type parameters and
  /// requirements.
  static GenericSignature get(ArrayRef<GenericTypeParamType *> params,
                              ArrayRef<Requirement> requirements,
                              bool isKnownCanonical = false);
  static GenericSignature get(TypeArrayView<GenericTypeParamType> params,
                              ArrayRef<Requirement> requirements,
                              bool isKnownCanonical = false);

public:
  static ASTContext &getASTContext(TypeArrayView<GenericTypeParamType> params,
                                   ArrayRef<Requirement> requirements);

public:
  /*implicit*/ GenericSignature(GenericSignatureImpl *P = 0) : Ptr(P) {}

  GenericSignatureImpl *getPointer() const { return Ptr; }

  bool isNull() const { return Ptr == 0; }

  GenericSignatureImpl *operator->() const { return Ptr; }

  explicit operator bool() const { return Ptr != 0; }

  /// Whether the given set of requirements involves a type variable.
  static bool hasTypeVariable(ArrayRef<Requirement> requirements);

  friend llvm::hash_code hash_value(GenericSignature sig) {
    using llvm::hash_value;
    return hash_value(sig.getPointer());
  }

  void print(raw_ostream &OS, PrintOptions Options = PrintOptions()) const;
  void print(ASTPrinter &Printer, PrintOptions Opts = PrintOptions()) const;
  void dump() const;
  std::string getAsString() const;

  // Support for FoldingSet.
  void Profile(llvm::FoldingSetNodeID &id) const;

  static void Profile(llvm::FoldingSetNodeID &ID,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      ArrayRef<Requirement> requirements);
public:
  using ConformsToArray = SmallVector<ProtocolDecl *, 2>;

private:
  // Direct comparison is disabled for generic signatures.  Canonicalize them
  // first, or use isEqual.
  void operator==(GenericSignature T) const = delete;
  void operator!=(GenericSignature T) const = delete;
};

/// A reference to a canonical generic signature.
class CanGenericSignature : public GenericSignature {
  bool isActuallyCanonicalOrNull() const;

public:
  /// Create a new generic signature with the given type parameters and
  /// requirements, first canonicalizing the types.
  static CanGenericSignature
  getCanonical(TypeArrayView<GenericTypeParamType> params,
               ArrayRef<Requirement> requirements, bool skipValidation = false);

public:
  CanGenericSignature(std::nullptr_t) : GenericSignature(nullptr) {}

  explicit CanGenericSignature(GenericSignatureImpl *P = 0)
      : GenericSignature(P) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanGenericSignature out of a non-canonical signature!");
  }
  explicit CanGenericSignature(GenericSignature S) : GenericSignature(S) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanGenericSignature out of a non-canonical signature!");
  }

  ArrayRef<CanTypeWrapper<GenericTypeParamType>> getGenericParams() const;

  bool operator==(CanGenericSignature S) const {
    return getPointer() == S.getPointer();
  }
  bool operator!=(CanGenericSignature S) const { return !operator==(S); }
};

/// The underlying implementation of generic signatures.
class alignas(1 << TypeAlignInBits) GenericSignatureImpl final
  : public llvm::FoldingSetNode,
    private llvm::TrailingObjects<GenericSignatureImpl, Type, Requirement> {
  friend class ASTContext;
  friend GenericSignature;
  friend TrailingObjects;

  GenericSignatureImpl(const GenericSignatureImpl&) = delete;
  void operator=(const GenericSignatureImpl&) = delete;

  unsigned NumGenericParams;
  unsigned NumRequirements;

  GenericEnvironment *GenericEnv = nullptr;

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return NumGenericParams;
  }
  size_t numTrailingObjects(OverloadToken<Requirement>) const {
    return NumRequirements;
  }

  /// Retrieve a mutable version of the generic parameters.
  MutableArrayRef<Type> getGenericParamsBuffer() {
    return {getTrailingObjects<Type>(), NumGenericParams};
  }

  /// Retrieve a mutable version of the requirements.
  MutableArrayRef<Requirement> getRequirementsBuffer() {
    return {getTrailingObjects<Requirement>(), NumRequirements};
  }

  GenericSignatureImpl(TypeArrayView<GenericTypeParamType> params,
                       ArrayRef<Requirement> requirements,
                       bool isKnownCanonical);

  // FIXME: Making this a CanGenericSignature reveals callers are violating
  // the interface's invariants.
  mutable llvm::PointerUnion<GenericSignatureImpl *, ASTContext *>
    CanonicalSignatureOrASTContext;

  void buildConformanceAccessPath(
      SmallVectorImpl<ConformanceAccessPath::Entry> &path,
      ArrayRef<Requirement> reqs,
      const void /*GenericSignatureBuilder::RequirementSource*/ *source,
      ProtocolDecl *conformingProto, Type rootType,
      ProtocolDecl *requirementSignatureProto);

  friend class ArchetypeType;

public:
  /// Retrieve the generic parameters.
  TypeArrayView<GenericTypeParamType> getGenericParams() const {
    auto temp = const_cast<GenericSignatureImpl *>(this);
    return TypeArrayView<GenericTypeParamType>(temp->getGenericParamsBuffer());
  }

  /// Retrieve the innermost generic parameters.
  ///
  /// Given a generic signature for a nested generic type, produce an
  /// array of the generic parameters for the innermost generic type.
  TypeArrayView<GenericTypeParamType> getInnermostGenericParams() const;

  /// Retrieve the requirements.
  ArrayRef<Requirement> getRequirements() const {
    return const_cast<GenericSignatureImpl *>(this)->getRequirementsBuffer();
  }

  /// Only allow allocation by doing a placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  /// Look up a stored conformance in the generic signature. These are formed
  /// from same-type constraints placed on associated types of generic
  /// parameters which have conformance constraints on them.
  Optional<ProtocolConformanceRef>
  lookupConformance(CanType depTy, ProtocolDecl *proto) const;

  /// Iterate over all generic parameters, passing a flag to the callback
  /// indicating if the generic parameter is canonical or not.
  void forEachParam(
    llvm::function_ref<void(GenericTypeParamType *, bool)> callback) const;

  /// Check if the generic signature makes all generic parameters
  /// concrete.
  bool areAllParamsConcrete() const;

  /// Compute the number of conformance requirements in this signature.
  unsigned getNumConformanceRequirements() const {
    unsigned result = 0;
    for (const auto &req : getRequirements()) {
      if (req.getKind() == RequirementKind::Conformance)
        ++result;
    }

    return result;
  }

  /// Return true if these two generic signatures are equal.
  bool isEqual(GenericSignature Other);

  /// Determines whether this GenericSignature is canonical.
  bool isCanonical() const;
  
  ASTContext &getASTContext() const;
  
  /// Canonicalize the components of a generic signature.
  CanGenericSignature getCanonicalSignature() const;

  /// Retrieve the generic signature builder for the given generic signature.
  GenericSignatureBuilder *getGenericSignatureBuilder();

  /// Returns the generic environment that provides fresh contextual types
  /// (archetypes) that correspond to the interface types in this generic
  /// signature.
  GenericEnvironment *getGenericEnvironment();

  /// Uniquing for the ASTContext.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getRequirements());
  }
  
  /// Determine whether the given dependent type is required to be a class.
  bool requiresClass(Type type);

  /// Determine the superclass bound on the given dependent type.
  Type getSuperclassBound(Type type);

  /// Determine the set of protocols to which the given dependent type
  /// must conform.
  GenericSignature::ConformsToArray getConformsTo(Type type);

  /// Determine whether the given dependent type conforms to this protocol.
  bool conformsToProtocol(Type type, ProtocolDecl *proto);

  /// Determine whether the given dependent type is equal to a concrete type.
  bool isConcreteType(Type type);

  /// Return the concrete type that the given dependent type is constrained to,
  /// or the null Type if it is not the subject of a concrete same-type
  /// constraint.
  Type getConcreteType(Type type);

  /// Return the layout constraint that the given dependent type is constrained
  /// to, or the null LayoutConstraint if it is not the subject of layout
  /// constraint.
  LayoutConstraint getLayoutConstraint(Type type);

  /// Return whether two type parameters represent the same type under this
  /// generic signature.
  ///
  /// The type parameters must be known to not be concrete within the context.
  bool areSameTypeParameterInContext(Type type1, Type type2);

  /// Determine if \c sig can prove \c requirement, meaning that it can deduce
  /// T: Foo or T == U (etc.) with the information it knows. This includes
  /// checking against global state, if any/all of the types in the requirement
  /// are concrete, not type parameters.
  bool isRequirementSatisfied(Requirement requirement);

  /// Return the requirements of this generic signature that are not also
  /// satisfied by \c otherSig.
  ///
  /// \param otherSig Another generic signature whose generic parameters are
  /// equivalent to or a subset of the generic parameters in this signature.
  SmallVector<Requirement, 4> requirementsNotSatisfiedBy(
                                               GenericSignature otherSig);

  /// Return the canonical version of the given type under this generic
  /// signature.
  CanType getCanonicalTypeInContext(Type type);
  bool isCanonicalTypeInContext(Type type);

  /// Return the canonical version of the given type under this generic
  /// signature.
  CanType getCanonicalTypeInContext(Type type,
                                    GenericSignatureBuilder &builder);
  bool isCanonicalTypeInContext(Type type, GenericSignatureBuilder &builder);

  /// Retrieve the conformance access path used to extract the conformance of
  /// interface \c type to the given \c protocol.
  ///
  /// \param type The interface type whose conformance access path is to be
  /// queried.
  /// \param protocol A protocol to which \c type conforms.
  ///
  /// \returns the conformance access path that starts at a requirement of
  /// this generic signature and ends at the conformance that makes \c type
  /// conform to \c protocol.
  ///
  /// \seealso ConformanceAccessPath
  ConformanceAccessPath getConformanceAccessPath(Type type,
                                                 ProtocolDecl *protocol);

  /// Get the ordinal of a generic parameter in this generic signature.
  ///
  /// For example, if you have a generic signature for a nested context like:
  ///   <t_0_0, t_0_1, t_1_0>
  /// then this will return 0 for t_0_0, 1 for t_0_1, and 2 for t_1_0.
  unsigned getGenericParamOrdinal(GenericTypeParamType *param);
      
  /// Get a substitution map that maps all of the generic signature's
  /// generic parameters to themselves.
  SubstitutionMap getIdentitySubstitutionMap() const;

  /// Whether this generic signature involves a type variable.
  bool hasTypeVariable() const;

  static void Profile(llvm::FoldingSetNodeID &ID,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      ArrayRef<Requirement> requirements);
  
  void print(raw_ostream &OS, PrintOptions Options = PrintOptions()) const;
  void print(ASTPrinter &Printer, PrintOptions Opts = PrintOptions()) const;
  void dump() const;
  std::string getAsString() const;
};

void simple_display(raw_ostream &out, GenericSignature sig);

inline bool CanGenericSignature::isActuallyCanonicalOrNull() const {
  return getPointer() == nullptr ||
         getPointer() ==
             llvm::DenseMapInfo<GenericSignatureImpl *>::getEmptyKey() ||
         getPointer() ==
             llvm::DenseMapInfo<GenericSignatureImpl *>::getTombstoneKey() ||
         getPointer()->isCanonical();
}

} // end namespace swift

namespace llvm {
static inline raw_ostream &operator<<(raw_ostream &OS,
                                      swift::GenericSignature Sig) {
  Sig.print(OS);
  return OS;
}

// A GenericSignature casts like a GenericSignatureImpl*.
template <> struct simplify_type<const ::swift::GenericSignature> {
  typedef ::swift::GenericSignatureImpl *SimpleType;
  static SimpleType getSimplifiedValue(const ::swift::GenericSignature &Val) {
    return Val.getPointer();
  }
};
template <>
struct simplify_type<::swift::GenericSignature>
    : public simplify_type<const ::swift::GenericSignature> {};

template <> struct DenseMapInfo<swift::GenericSignature> {
  static swift::GenericSignature getEmptyKey() {
    return llvm::DenseMapInfo<swift::GenericSignatureImpl *>::getEmptyKey();
  }
  static swift::GenericSignature getTombstoneKey() {
    return llvm::DenseMapInfo<swift::GenericSignatureImpl *>::getTombstoneKey();
  }
  static unsigned getHashValue(swift::GenericSignature Val) {
    return DenseMapInfo<swift::GenericSignatureImpl *>::getHashValue(
        Val.getPointer());
  }
  static bool isEqual(swift::GenericSignature LHS,
                      swift::GenericSignature RHS) {
    return LHS.getPointer() == RHS.getPointer();
  }
};

// A GenericSignature is "pointer like".
template <> struct PointerLikeTypeTraits<swift::GenericSignature> {
public:
  static inline swift::GenericSignature getFromVoidPointer(void *P) {
    return (swift::GenericSignatureImpl *)P;
  }
  static inline void *getAsVoidPointer(swift::GenericSignature S) {
    return (void *)S.getPointer();
  }
  enum { NumLowBitsAvailable = swift::TypeAlignInBits };
};

template <> struct PointerLikeTypeTraits<swift::CanGenericSignature> {
public:
  static inline swift::CanGenericSignature getFromVoidPointer(void *P) {
    return swift::CanGenericSignature((swift::GenericSignatureImpl *)P);
  }
  static inline void *getAsVoidPointer(swift::CanGenericSignature S) {
    return (void *)S.getPointer();
  }
  enum { NumLowBitsAvailable = swift::TypeAlignInBits };
};
} // end namespace llvm

#endif // SWIFT_AST_GENERIC_SIGNATURE_H
