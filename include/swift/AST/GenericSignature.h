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

#include "swift/AST/Requirement.h"
#include "swift/AST/SubstitutionList.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"
#include <utility>

namespace swift {

class GenericSignatureBuilder;
class ProtocolConformanceRef;
class ProtocolType;
class Substitution;
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
  llvm::SmallVector<Entry, 2> path;

  friend class GenericSignature;

public:
  typedef llvm::SmallVector<Entry, 2>::const_iterator iterator;
  typedef llvm::SmallVector<Entry, 2>::const_iterator const_iterator;

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
    private llvm::TrailingObjects<GenericSignature, GenericTypeParamType *,
                                  Requirement> {
  friend TrailingObjects;

  unsigned NumGenericParams;
  unsigned NumRequirements;

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  size_t numTrailingObjects(OverloadToken<GenericTypeParamType *>) const {
    return NumGenericParams;
  }
  size_t numTrailingObjects(OverloadToken<Requirement>) const {
    return NumRequirements;
  }

  /// Retrieve a mutable version of the generic parameters.
  MutableArrayRef<GenericTypeParamType *> getGenericParamsBuffer() {
    return {getTrailingObjects<GenericTypeParamType *>(), NumGenericParams};
  }

  /// Retrieve a mutable version of the requirements.
  MutableArrayRef<Requirement> getRequirementsBuffer() {
    return {getTrailingObjects<Requirement>(), NumRequirements};
  }

  GenericSignature(ArrayRef<GenericTypeParamType *> params,
                   ArrayRef<Requirement> requirements,
                   bool isKnownCanonical);

  mutable llvm::PointerUnion<GenericSignature *, ASTContext *>
    CanonicalSignatureOrASTContext;
  
  static ASTContext &getASTContext(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements);

  /// Retrieve the generic signature builder for the given generic signature.
  GenericSignatureBuilder *getGenericSignatureBuilder(ModuleDecl &mod);

  void populateParentMap(SubstitutionMap &subMap) const;

  friend class ArchetypeType;

public:
  /// Create a new generic signature with the given type parameters and
  /// requirements.
  static GenericSignature *get(ArrayRef<GenericTypeParamType *> params,
                               ArrayRef<Requirement> requirements,
                               bool isKnownCanonical = false);

  /// Create a new generic signature with the given type parameters and
  /// requirements, first canonicalizing the types.
  static CanGenericSignature getCanonical(
                                      ArrayRef<GenericTypeParamType *> params,
                                      ArrayRef<Requirement> requirements);

  /// Retrieve the generic parameters.
  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return const_cast<GenericSignature *>(this)->getGenericParamsBuffer();
  }

  /// Retrieve the innermost generic parameters.
  ///
  /// Given a generic signature for a nested generic type, produce an
  /// array of the generic parameters for the innermost generic type.
  ArrayRef<GenericTypeParamType *> getInnermostGenericParams() const;

  /// Create a text string that describes the bindings of generic parameters
  /// that are relevant to the given set of types, e.g.,
  /// "[with T = Bar, U = Wibble]".
  ///
  /// \param types The types that will be scanned for generic type parameters,
  /// which will be used in the resulting type.
  ///
  /// \param substitutions The generic parameter -> generic argument
  /// substitutions that will have been applied to these types.
  /// These are used to produce the "parameter = argument" bindings in the test.
  std::string
  gatherGenericParamBindingsText(ArrayRef<Type> types,
                                 TypeSubstitutionFn substitutions) const;

  /// Retrieve the requirements.
  ArrayRef<Requirement> getRequirements() const {
    return const_cast<GenericSignature *>(this)->getRequirementsBuffer();
  }

  /// Only allow allocation by doing a placement new.
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  /// Build an interface type substitution map from a vector of Substitutions
  /// that correspond to the generic parameters in this generic signature.
  SubstitutionMap getSubstitutionMap(SubstitutionList args) const;

  /// Build an interface type substitution map from a type substitution function
  /// and conformance lookup function.
  SubstitutionMap
  getSubstitutionMap(TypeSubstitutionFn subs,
                     LookupConformanceFn lookupConformance) const;

  /// Look up a stored conformance in the generic signature. These are formed
  /// from same-type constraints placed on associated types of generic
  /// parameters which have conformance constraints on them.
  Optional<ProtocolConformanceRef>
  lookupConformance(CanType depTy, ProtocolDecl *proto) const;

  using GenericFunction = auto(CanType canType, Type conformingReplacementType,
    ProtocolType *conformedProtocol)
    ->Optional<ProtocolConformanceRef>;
  using LookupConformanceFn = llvm::function_ref<GenericFunction>;

  /// Build an array of substitutions from an interface type substitution map,
  /// using the given function to look up conformances.
  void getSubstitutions(TypeSubstitutionFn substitution,
                        LookupConformanceFn lookupConformance,
                        SmallVectorImpl<Substitution> &result) const;

  /// Build an array of substitutions from an interface type substitution map,
  /// using the given function to look up conformances.
  void getSubstitutions(const TypeSubstitutionMap &subMap,
                        LookupConformanceFn lookupConformance,
                        SmallVectorImpl<Substitution> &result) const;

  /// Build an array of substitutions from an interface type substitution map,
  /// using the given function to look up conformances.
  void getSubstitutions(const SubstitutionMap &subMap,
                        SmallVectorImpl<Substitution> &result) const;

  /// Enumerate all of the dependent types in the type signature that will
  /// occur in substitution lists (in order), along with the set of
  /// conformance requirements placed on that dependent type.
  ///
  /// \param fn Callback function that will receive each (type, requirements)
  /// pair, in the order they occur within a list of substitutions. If this
  /// returns \c true, the enumeration will be aborted.
  ///
  /// \returns true if any call to \c fn returned \c true, otherwise \c false.
  bool enumeratePairedRequirements(
         llvm::function_ref<bool(Type, ArrayRef<Requirement>)> fn) const;

  /// Return a vector of all generic parameters that are not subject to
  /// a concrete same-type constraint.
  SmallVector<GenericTypeParamType *, 2> getSubstitutableParams() const;

  /// Check if the generic signature makes all generic parameters
  /// concrete.
  bool areAllParamsConcrete() const {
    return !enumeratePairedRequirements(
      [](Type, ArrayRef<Requirement>) -> bool {
        return true;
      });
  }

  /// Return the size of a SubstitutionList built from this signature.
  ///
  /// Don't add new calls of this -- the representation of SubstitutionList
  /// will be changing soon.
  unsigned getSubstitutionListSize() const {
    unsigned result = 0;
    enumeratePairedRequirements(
      [&](Type, ArrayRef<Requirement>) -> bool {
        result++;
        return false;
      });
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
  GenericEnvironment *createGenericEnvironment(ModuleDecl &mod);

  /// Uniquing for the ASTContext.
  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getRequirements());
  }
  
  /// Determine whether the given dependent type is required to be a class.
  bool requiresClass(Type type, ModuleDecl &mod);

  /// Determine the superclass bound on the given dependent type.
  Type getSuperclassBound(Type type, ModuleDecl &mod);

  using ConformsToArray = SmallVector<ProtocolDecl *, 2>;
  /// Determine the set of protocols to which the given dependent type
  /// must conform.
  ConformsToArray getConformsTo(Type type, ModuleDecl &mod);

  /// Determine whether the given dependent type is equal to a concrete type.
  bool isConcreteType(Type type, ModuleDecl &mod);

  /// Return the concrete type that the given dependent type is constrained to,
  /// or the null Type if it is not the subject of a concrete same-type
  /// constraint.
  Type getConcreteType(Type type, ModuleDecl &mod);

  /// Return the layout constraint that the given dependent type is constrained
  /// to, or the null LayoutConstraint if it is not the subject of layout
  /// constraint.
  LayoutConstraint getLayoutConstraint(Type type, ModuleDecl &mod);

  /// Return whether two type parameters represent the same type under this
  /// generic signature.
  ///
  /// The type parameters must be known to not be concrete within the context.
  bool areSameTypeParameterInContext(Type type1, Type type2, ModuleDecl &mod);

  /// Return the canonical version of the given type under this generic
  /// signature.
  CanType getCanonicalTypeInContext(Type type, ModuleDecl &mod);
  bool isCanonicalTypeInContext(Type type, ModuleDecl &mod);

  /// Return the canonical version of the given type under this generic
  /// signature.
  CanType getCanonicalTypeInContext(Type type, GenericSignatureBuilder &builder);
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
                                                 ProtocolDecl *protocol,
                                                 ModuleDecl &mod);

  static void Profile(llvm::FoldingSetNodeID &ID,
                      ArrayRef<GenericTypeParamType *> genericParams,
                      ArrayRef<Requirement> requirements);
  
  void print(raw_ostream &OS) const;
  void dump() const;
  std::string getAsString() const;
};
  
inline
CanGenericSignature::CanGenericSignature(GenericSignature *Signature)
  : Signature(Signature)
{
  assert(!Signature || Signature->isCanonical());
}
  
inline ArrayRef<CanTypeWrapper<GenericTypeParamType>>
CanGenericSignature::getGenericParams() const{
  ArrayRef<GenericTypeParamType*> params = Signature->getGenericParams();
  auto base = reinterpret_cast<const CanTypeWrapper<GenericTypeParamType>*>(
                                                                params.data());
  return {base, params.size()};
}

} // end namespace swift

#endif // SWIFT_AST_GENERIC_SIGNATURE_H
