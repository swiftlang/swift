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

  friend class GenericSignature;

public:
  typedef const Entry *const_iterator;
  typedef const_iterator iterator;

  const_iterator begin() const { return path.begin(); }
  const_iterator end() const { return path.end(); }

  void print(raw_ostream &OS) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const, "only for use in a debugger");
};

/// Describes the generic signature of a particular declaration, including
/// both the generic type parameters and the requirements placed on those
/// generic parameters.
class alignas(1 << TypeAlignInBits) GenericSignature final
  : public llvm::FoldingSetNode,
    private llvm::TrailingObjects<GenericSignature, Type, Requirement> {
  friend TrailingObjects;

  unsigned NumGenericParams;
  unsigned NumRequirements;

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

  GenericSignature(TypeArrayView<GenericTypeParamType> params,
                   ArrayRef<Requirement> requirements,
                   bool isKnownCanonical);

  mutable llvm::PointerUnion<GenericSignature *, ASTContext *>
    CanonicalSignatureOrASTContext;
  
  static ASTContext &getASTContext(TypeArrayView<GenericTypeParamType> params,
                                   ArrayRef<Requirement> requirements);

  /// Retrieve the generic signature builder for the given generic signature.
  GenericSignatureBuilder *getGenericSignatureBuilder();

  void buildConformanceAccessPath(
      SmallVectorImpl<ConformanceAccessPath::Entry> &path,
      ArrayRef<Requirement> reqs,
      const void /*GenericSignatureBuilder::RequirementSource*/ *source,
      ProtocolDecl *conformingProto, Type rootType,
      ProtocolDecl *requirementSignatureProto);

  friend class ArchetypeType;

public:
  /// Create a new generic signature with the given type parameters and
  /// requirements.
  static GenericSignature *get(ArrayRef<GenericTypeParamType *> params,
                               ArrayRef<Requirement> requirements,
                               bool isKnownCanonical = false);
  static GenericSignature *get(TypeArrayView<GenericTypeParamType> params,
                               ArrayRef<Requirement> requirements,
                               bool isKnownCanonical = false);

  /// Create a new generic signature with the given type parameters and
  /// requirements, first canonicalizing the types.
  static CanGenericSignature getCanonical(
                                     TypeArrayView<GenericTypeParamType> params,
                                     ArrayRef<Requirement> requirements,
                                     bool skipValidation = false);

  /// Retrieve the generic parameters.
  TypeArrayView<GenericTypeParamType> getGenericParams() const {
    auto temp = const_cast<GenericSignature*>(this);
    return TypeArrayView<GenericTypeParamType>(temp->getGenericParamsBuffer());
  }

  /// Retrieve the innermost generic parameters.
  ///
  /// Given a generic signature for a nested generic type, produce an
  /// array of the generic parameters for the innermost generic type.
  TypeArrayView<GenericTypeParamType> getInnermostGenericParams() const;

  /// Retrieve the requirements.
  ArrayRef<Requirement> getRequirements() const {
    return const_cast<GenericSignature *>(this)->getRequirementsBuffer();
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

  /// Return a vector of all generic parameters that are not subject to
  /// a concrete same-type constraint.
  SmallVector<GenericTypeParamType *, 2> getSubstitutableParams() const;

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

  /// Determines whether this GenericSignature is canonical.
  bool isCanonical() const;
  
  ASTContext &getASTContext() const;
  
  /// Canonicalize the components of a generic signature.
  CanGenericSignature getCanonicalSignature() const;

  /// Create a new generic environment that provides fresh contextual types
  /// (archetypes) that correspond to the interface types in this generic
  /// signature.
  GenericEnvironment *createGenericEnvironment();

  /// Uniquing for the ASTContext.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getRequirements());
  }
  
  /// Determine whether the given dependent type is required to be a class.
  bool requiresClass(Type type);

  /// Determine the superclass bound on the given dependent type.
  Type getSuperclassBound(Type type);

  using ConformsToArray = SmallVector<ProtocolDecl *, 2>;
  /// Determine the set of protocols to which the given dependent type
  /// must conform.
  ConformsToArray getConformsTo(Type type);

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
                                               GenericSignature *otherSig);

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

  static void Profile(llvm::FoldingSetNodeID &ID,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      ArrayRef<Requirement> requirements);
  
  void print(raw_ostream &OS, PrintOptions Options = PrintOptions()) const;
  void dump() const;
  std::string getAsString() const;
};
  
inline
CanGenericSignature::CanGenericSignature(GenericSignature *Signature)
  : Signature(Signature)
{
  assert(!Signature || Signature->isCanonical());
}

} // end namespace swift

#endif // SWIFT_AST_GENERIC_SIGNATURE_H
