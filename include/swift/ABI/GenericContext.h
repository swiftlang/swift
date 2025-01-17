//===--- GenericContext.h - ABI for generic signatures ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file describes runtime metadata structures for representing
// generic signatures.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_GENERICCONTEXT_H
#define SWIFT_ABI_GENERICCONTEXT_H

#include "swift/ABI/TargetLayout.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/MetadataRef.h"
#include "swift/ABI/InvertibleProtocols.h"
#include "swift/ABI/TrailingObjects.h"
#include "swift/Basic/MathUtils.h"
#include "swift/Demangling/Demangle.h"

namespace swift {
template <typename Runtime>
struct TargetProtocolConformanceDescriptor;
template <typename Runtime>
struct TargetGenericContext;

template <typename Runtime>
struct TargetGenericContextDescriptorHeader {
  /// The number of (source-written) generic parameters, and thus
  /// the number of GenericParamDescriptors associated with this
  /// context.  The parameter descriptors appear in the order in
  /// which they were given in the source.
  ///
  /// A GenericParamDescriptor corresponds to a type metadata pointer
  /// in the arguments layout when isKeyArgument() is true.
  /// isKeyArgument() will be false if the parameter has been made
  /// equivalent to a different parameter or a concrete type.
  uint16_t NumParams;

  /// The number of GenericRequirementDescriptors in this generic
  /// signature.
  ///
  /// A GenericRequirementDescriptor of kind Protocol corresponds
  /// to a witness table pointer in the arguments layout when
  /// isKeyArgument() is true.  isKeyArgument() will be false if
  /// the protocol is an Objective-C protocol.  (Unlike generic
  /// parameters, redundant conformance requirements can simply be
  /// eliminated, and so that case is not impossible.)
  uint16_t NumRequirements;

  /// The size of the "key" area of the argument layout, in words.
  /// Key arguments include shape classes, generic parameters and
  /// conformance requirements which are part of the identity of
  /// the context.
  ///
  /// The key area of the argument layout consists of:
  ///
  /// - a sequence of pack lengths, in the same order as the parameter
  ///   descriptors which satisfy getKind() == GenericParamKind::TypePack
  ///   and hasKeyArgument();
  ///
  /// - a sequence of metadata or metadata pack pointers, in the same
  ///   order as the parameter descriptors which satisfy hasKeyArgument();
  ///
  /// - a sequence of witness table or witness table pack pointers, in the
  ///   same order as the requirement descriptors which satisfy
  ///   hasKeyArgument().
  ///
  ///   a sequence of values, in the same order as the parameter descriptors
  ///   which satisify getKind() == GenericParamKind::Value and
  ///   hasKeyArgument();
  ///
  /// The elements above which are packs are precisely those appearing
  /// in the sequence of trailing GenericPackShapeDescriptors.
  uint16_t NumKeyArguments;

  /// Originally this was the size of the "extra" area of the argument
  /// layout, in words.  The idea was that extra arguments would
  /// include generic parameters and conformances that are not part
  /// of the identity of the context; however, it's unclear why we
  /// would ever want such a thing.  As a result, in pre-5.8 runtimes
  /// this field is always zero.  New flags can only be added as long
  /// as they remains zero in code which must be compatible with
  /// older Swift runtimes.
  GenericContextDescriptorFlags Flags;
  
  uint32_t getNumArguments() const {
    // Note: this used to be NumKeyArguments + NumExtraArguments,
    // and flags was named NumExtraArguments, which is why Flags
    // must remain zero when backward deploying to Swift 5.7 or
    // earlier.
    return NumKeyArguments;
  }

  /// Return the total size of the argument layout, in words.
  /// The alignment of the argument layout is the word alignment.
  uint32_t getArgumentLayoutSizeInWords() const {
    return getNumArguments();
  }

  bool hasArguments() const {
    return getNumArguments() > 0;
  }

  bool hasConditionalInvertedProtocols() const {
    return Flags.hasConditionalInvertedProtocols();
  }
};
using GenericContextDescriptorHeader =
  TargetGenericContextDescriptorHeader<InProcess>;

template<typename Runtime>
class TargetGenericRequirementDescriptor {
public:
  GenericRequirementFlags Flags;

  /// The type that's constrained, described as a mangled name.
  RelativeDirectPointer<const char, /*nullable*/ false> Param;

  union {
    /// A mangled representation of the same-type or base class the param is
    /// constrained to.
    ///
    /// Only valid if the requirement has SameType or BaseClass kind.
    RelativeDirectPointer<const char, /*nullable*/ false> Type;
    
    /// The protocol the param is constrained to.
    ///
    /// Only valid if the requirement has Protocol kind.
    RelativeTargetProtocolDescriptorPointer<Runtime> Protocol;
    
    /// The conformance the param is constrained to use.
    ///
    /// Only valid if the requirement has SameConformance kind.
    RelativeIndirectablePointer<TargetProtocolConformanceDescriptor<Runtime>,
                                /*nullable*/ false> Conformance;
    
    /// The kind of layout constraint.
    ///
    /// Only valid if the requirement has Layout kind.
    GenericRequirementLayoutKind Layout;

    /// The set of invertible protocols whose check is disabled, along
    /// with the index of the generic parameter to which this applies.
    ///
    /// The index is technically redundant with the subject type, but its
    /// storage is effectively free because this union is 32 bits anyway. The
    /// index 0xFFFF is reserved for "not a generic parameter", in which case
    /// the constraints are on the subject type.
    ///
    /// Only valid if the requirement has InvertedProtocols kind.
    struct {
      uint16_t GenericParamIndex;
      InvertibleProtocolSet Protocols;
    } InvertedProtocols;
  };

  constexpr GenericRequirementFlags getFlags() const {
    return Flags;
  }

  constexpr GenericRequirementKind getKind() const {
    return getFlags().getKind();
  }

  /// Retrieve the generic parameter that is the subject of this requirement,
  /// as a mangled type name.
  llvm::StringRef getParam() const {
    return swift::Demangle::makeSymbolicMangledNameStringRef(Param.get());
  }

  /// Retrieve the protocol for a Protocol requirement.
  TargetProtocolDescriptorRef<Runtime> getProtocol() const {
    assert(getKind() == GenericRequirementKind::Protocol);
    return Protocol;
  }

  /// Retreive the raw value of the Protocol requirement pointer.
  int32_t getUnresolvedProtocolAddress() const {
    assert(getKind() == GenericRequirementKind::Protocol);
    return Protocol.getUnresolvedProtocolAddress();
  }

  /// Retreive the offset to the Protocol field
  constexpr inline auto
  getProtocolOffset() const -> typename Runtime::StoredSize {
    return offsetof(typename std::remove_reference<decltype(*this)>::type, Protocol);
  }

  /// Retreive the offset to the Type field
  constexpr inline auto
  getSameTypeNameOffset() const -> typename Runtime::StoredSize {
    return offsetof(typename std::remove_reference<decltype(*this)>::type, Type);
  }

  /// Retreive the offset to the Param field
  constexpr inline auto
  getParamOffset() const -> typename Runtime::StoredSize {
    return offsetof(typename std::remove_reference<decltype(*this)>::type, Param);
  }

  /// Retrieve the right-hand type for a SameType, BaseClass or SameShape requirement.
  llvm::StringRef getMangledTypeName() const {
    assert(getKind() == GenericRequirementKind::SameType ||
           getKind() == GenericRequirementKind::BaseClass ||
           getKind() == GenericRequirementKind::SameShape);
    return swift::Demangle::makeSymbolicMangledNameStringRef(Type.get());
  }

  /// Retrieve the protocol conformance record for a SameConformance
  /// requirement.
  const TargetProtocolConformanceDescriptor<Runtime> *getConformance() const {
    assert(getKind() == GenericRequirementKind::SameConformance);
    return Conformance;
  }

  /// Retrieve the layout constraint.
  GenericRequirementLayoutKind getLayout() const {
    assert(getKind() == GenericRequirementKind::Layout);
    return Layout;
  }

  /// Retrieve the set of inverted protocols.
  InvertibleProtocolSet getInvertedProtocols() const {
    assert(getKind() == GenericRequirementKind::InvertedProtocols);
    return InvertedProtocols.Protocols;
  }

  /// Retrieve the invertible protocol kind.
  uint16_t getInvertedProtocolsGenericParamIndex() const {
    assert(getKind() == GenericRequirementKind::InvertedProtocols);
    return InvertedProtocols.GenericParamIndex;
  }

  /// Determine whether this generic requirement has a known kind.
  ///
  /// \returns \c false for any future generic requirement kinds.
  bool hasKnownKind() const {
    switch (getKind()) {
    case GenericRequirementKind::BaseClass:
    case GenericRequirementKind::Layout:
    case GenericRequirementKind::Protocol:
    case GenericRequirementKind::SameConformance:
    case GenericRequirementKind::SameType:
    case GenericRequirementKind::SameShape:
    case GenericRequirementKind::InvertedProtocols:
      return true;
    }

    return false;
  }
};
using GenericRequirementDescriptor =
  TargetGenericRequirementDescriptor<InProcess>;

struct GenericPackShapeHeader {
  /// The number of generic parameters and conformance requirements
  /// which are packs.
  ///
  /// Must equal the sum of:
  /// - the number of GenericParamDescriptors whose kind is
  ///   GenericParamKind::TypePack and isKeyArgument bits set;
  /// - the number of GenericRequirementDescriptors with the
  ///   isPackRequirement and isKeyArgument bits set
  uint16_t NumPacks;

  /// The number of equivalence classes in the same-shape relation.
  uint16_t NumShapeClasses;
};

/// The GenericPackShapeHeader is followed by an array of these descriptors,
/// whose length is given by the header's NumPacks field.
///
/// The invariant is that all pack descriptors with GenericPackKind::Metadata
/// must precede those with GenericPackKind::WitnessTable, and for each kind,
/// the pack descriptors are ordered by their Index.
///
/// This allows us to iterate over the generic arguments array in parallel
/// with the array of pack shape descriptors. We know we have a metadata
/// or witness table when we reach the generic argument whose index is
/// stored in the next descriptor; we increment the descriptor pointer in
/// this case.
struct GenericPackShapeDescriptor {
  GenericPackKind Kind;

  /// The index of this metadata pack or witness table pack in the
  /// generic arguments array.
  uint16_t Index;

  /// The equivalence class of this pack under the same-shape relation.
  ///
  /// Must be less than GenericPackShapeHeader::NumShapeClasses.
  uint16_t ShapeClass;

  uint16_t Unused;
};

/// A count for the number of requirements for the number of requirements
/// for a given conditional conformance to a invertible protocols.
struct ConditionalInvertibleProtocolsRequirementCount {
  uint16_t count;
};

/// A invertible protocol set used for the conditional conformances in a
/// generic context.
struct ConditionalInvertibleProtocolSet: InvertibleProtocolSet {
  using InvertibleProtocolSet::InvertibleProtocolSet;
};

/// A generic requirement for describing a conditional conformance to a
/// invertible protocol.
///
/// This type is equivalent to a `TargetGenericRequirementDescriptor`, and
/// differs only because it needs to occur alongside
template<typename Runtime>
struct TargetConditionalInvertibleProtocolRequirement: TargetGenericRequirementDescriptor<Runtime> { };

struct GenericValueHeader {
  /// The total number of generic parameters in this signature where
  /// getKind() == GenericParamKind::Value.
  uint32_t NumValues;
};

/// The GenericValueHeader is followed by an array of these descriptors,
/// whose length is given by the header's NumValues field.
struct GenericValueDescriptor {
  GenericValueType Type;
};

/// An array of generic parameter descriptors, all
/// GenericParamDescriptor::implicit(), which is by far
/// the most common case.  Some generic context storage can
/// avoid storing descriptors when they all match this pattern.
extern const GenericParamDescriptor
ImplicitGenericParamDescriptors[MaxNumImplicitGenericParamDescriptors];

inline const GenericParamDescriptor *
externalTargetImplicitGenericParamDescriptors() {
  static const GenericParamDescriptor
      buffer[MaxNumImplicitGenericParamDescriptors] = {
#define D GenericParamDescriptor::implicit()
          D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D,
          D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D,
          D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D, D
#undef D
      };
  return buffer;
}

template <class Runtime>
const GenericParamDescriptor *targetImplicitGenericParamDescriptors() {
  return externalTargetImplicitGenericParamDescriptors();
}
template <>
inline const GenericParamDescriptor *targetImplicitGenericParamDescriptors<InProcess>() {
  return ImplicitGenericParamDescriptors;
}

/// A runtime description of a generic signature.
template<typename Runtime>
class RuntimeGenericSignature {
  TargetGenericContextDescriptorHeader<Runtime> Header;
  const GenericParamDescriptor *Params;
  const TargetGenericRequirementDescriptor<Runtime> *Requirements;
  GenericPackShapeHeader PackShapeHeader;
  const GenericPackShapeDescriptor *PackShapeDescriptors;
  GenericValueHeader ValueHeader;
  const GenericValueDescriptor *ValueDescriptors;

public:
  RuntimeGenericSignature()
    : Header{0, 0, 0, GenericContextDescriptorFlags(false, false, false)},
      Params(nullptr), Requirements(nullptr),
      PackShapeHeader{0, 0}, PackShapeDescriptors(nullptr), ValueHeader{0},
      ValueDescriptors(nullptr) {}

  RuntimeGenericSignature(const TargetGenericContextDescriptorHeader<Runtime> &header,
                          const GenericParamDescriptor *params,
                          const TargetGenericRequirementDescriptor<Runtime> *requirements,
                          const GenericPackShapeHeader &packShapeHeader,
                          const GenericPackShapeDescriptor *packShapeDescriptors,
                          const GenericValueHeader &valueHeader,
                          const GenericValueDescriptor *valueDescriptors)
    : Header(header), Params(params), Requirements(requirements),
      PackShapeHeader(packShapeHeader), PackShapeDescriptors(packShapeDescriptors),
      ValueHeader(valueHeader), ValueDescriptors(valueDescriptors) {}

  llvm::ArrayRef<GenericParamDescriptor> getParams() const {
    return llvm::ArrayRef(Params, Header.NumParams);
  }

  llvm::ArrayRef<TargetGenericRequirementDescriptor<Runtime>> getRequirements() const {
    return llvm::ArrayRef(Requirements, Header.NumRequirements);
  }

  const GenericPackShapeHeader &getGenericPackShapeHeader() const {
    return PackShapeHeader;
  }

  llvm::ArrayRef<GenericPackShapeDescriptor> getGenericPackShapeDescriptors() const {
    return llvm::ArrayRef(PackShapeDescriptors, PackShapeHeader.NumPacks);
  }

  const GenericValueHeader &getGenericValueHeader() const {
    return ValueHeader;
  }

  llvm::ArrayRef<GenericValueDescriptor> getGenericValueDescriptors() const {
    return llvm::ArrayRef(ValueDescriptors, ValueHeader.NumValues);
  }

  size_t getArgumentLayoutSizeInWords() const {
    return Header.getArgumentLayoutSizeInWords();
  }
};

template<typename Runtime>
class TargetGenericEnvironment
    : public swift::ABI::TrailingObjects<TargetGenericEnvironment<Runtime>,
               uint16_t, GenericParamDescriptor,
               TargetGenericRequirementDescriptor<Runtime>> {
  using GenericRequirementDescriptor =
    TargetGenericRequirementDescriptor<Runtime>;
  using TrailingObjects =
     swift::ABI::TrailingObjects<TargetGenericEnvironment<Runtime>,
       uint16_t, GenericParamDescriptor, GenericRequirementDescriptor>;
  friend TrailingObjects;

#if !defined(_MSC_VER) || _MSC_VER >= 1920
  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;
#else
// MSVC 2017 trips parsing an using of an using, of a variadic template
#define OverloadToken typename TrailingObjects::template OverloadToken
#endif

  size_t numTrailingObjects(OverloadToken<uint16_t>) const {
    return Flags.getNumGenericParameterLevels();
  }

  size_t numTrailingObjects(OverloadToken<GenericParamDescriptor>) const {
    return getGenericParameterCounts().back();
  }

  size_t numTrailingObjects(OverloadToken<GenericRequirementDescriptor>) const {
    return Flags.getNumGenericRequirements();
  }

#if defined(_MSC_VER) && _MSC_VER < 1920
#undef OverloadToken
#endif

  GenericEnvironmentFlags Flags;

public:
  /// Retrieve the cumulative generic parameter counts at each level of genericity.
  llvm::ArrayRef<uint16_t> getGenericParameterCounts() const {
    return llvm::ArrayRef(this->template getTrailingObjects<uint16_t>(),
                          Flags.getNumGenericParameterLevels());
  }

  /// Retrieve the generic parameters descriptors.
  llvm::ArrayRef<GenericParamDescriptor> getGenericParameters() const {
    return llvm::ArrayRef(
        this->template getTrailingObjects<GenericParamDescriptor>(),
        getGenericParameterCounts().back());
  }

  /// Retrieve the generic requirements.
  llvm::ArrayRef<GenericRequirementDescriptor> getGenericRequirements() const {
    return llvm::ArrayRef(
        this->template getTrailingObjects<GenericRequirementDescriptor>(),
        Flags.getNumGenericRequirements());
  }
};

using GenericEnvironmentDescriptor =
TargetGenericEnvironment<InProcess>;

/// CRTP class for a context descriptor that includes trailing generic
/// context description.
template<class Self,
         template <typename> class TargetGenericContextHeaderType =
           TargetGenericContextDescriptorHeader,
         typename... FollowingTrailingObjects>
class TrailingGenericContextObjects;

// This oddity with partial specialization is necessary to get
// reasonable-looking code while also working around various kinds of
// compiler bad behavior with injected class names.
template<class Runtime,
         template <typename> class TargetSelf,
         template <typename> class TargetGenericContextHeaderType,
         typename... FollowingTrailingObjects>
class TrailingGenericContextObjects<TargetSelf<Runtime>,
                                    TargetGenericContextHeaderType,
                                    FollowingTrailingObjects...> :
  protected swift::ABI::TrailingObjects<TargetSelf<Runtime>,
      TargetGenericContextHeaderType<Runtime>,
      GenericParamDescriptor,
      TargetGenericRequirementDescriptor<Runtime>,
      GenericPackShapeHeader,
      GenericPackShapeDescriptor,
      ConditionalInvertibleProtocolSet,
      ConditionalInvertibleProtocolsRequirementCount,
      TargetConditionalInvertibleProtocolRequirement<Runtime>,
      GenericValueHeader,
      GenericValueDescriptor,
      FollowingTrailingObjects...>
{
protected:
  using Self = TargetSelf<Runtime>;
  using GenericContextHeaderType = TargetGenericContextHeaderType<Runtime>;
  using GenericRequirementDescriptor =
    TargetGenericRequirementDescriptor<Runtime>;
  using GenericConditionalInvertibleProtocolRequirement =
    TargetConditionalInvertibleProtocolRequirement<Runtime>;
  using TrailingObjects = swift::ABI::TrailingObjects<Self,
    GenericContextHeaderType,
    GenericParamDescriptor,
    GenericRequirementDescriptor,
    GenericPackShapeHeader,
    GenericPackShapeDescriptor,
    ConditionalInvertibleProtocolSet,
    ConditionalInvertibleProtocolsRequirementCount,
    GenericConditionalInvertibleProtocolRequirement,
    GenericValueHeader,
    GenericValueDescriptor,
    FollowingTrailingObjects...>;
  friend TrailingObjects;

#if !defined(_MSC_VER) || _MSC_VER >= 1920
  template<typename T>
  using OverloadToken = typename TrailingObjects::template OverloadToken<T>;
#else
// MSVC 2017 trips parsing an using of an using, of a variadic template
#define OverloadToken typename TrailingObjects::template OverloadToken
#endif
  
  const Self *asSelf() const {
    return static_cast<const Self *>(this);
  }
public:
  using StoredSize = typename Runtime::StoredSize;
  using StoredPointer = typename Runtime::StoredPointer;

  const GenericContextHeaderType &getFullGenericContextHeader() const {
    assert(asSelf()->isGeneric());
    return *this->template getTrailingObjects<GenericContextHeaderType>();
  }

  const TargetGenericContextDescriptorHeader<Runtime> &
  getGenericContextHeader() const {
    /// HeaderType ought to be convertible to GenericContextDescriptorHeader.
    return getFullGenericContextHeader();
  }

  bool hasConditionalInvertedProtocols() const {
    if (!asSelf()->isGeneric())
      return false;

    return getGenericContextHeader().hasConditionalInvertedProtocols();
  }

  const InvertibleProtocolSet &
  getConditionalInvertedProtocols() const {
    assert(hasConditionalInvertedProtocols());
    return *this->template
        getTrailingObjects<ConditionalInvertibleProtocolSet>();
  }

  /// Retrieve the counts for # of conditional invertible protocols for each
  /// conditional conformance to a invertible protocol.
  ///
  /// The counts are cumulative, so the first entry in the array is the
  /// number of requirements for the first conditional conformance. The
  /// second entry in the array is the number of requirements in the first
  /// and second conditional conformances. The last entry is, therefore, the
  /// total count of requirements in the structure.
  llvm::ArrayRef<ConditionalInvertibleProtocolsRequirementCount>
  getConditionalInvertibleProtocolRequirementCounts() const {
    if (!asSelf()->hasConditionalInvertedProtocols())
      return {};

    return {
      this->template
        getTrailingObjects<ConditionalInvertibleProtocolsRequirementCount>(),
      getNumConditionalInvertibleProtocolsRequirementCounts()
    };
  }

  /// Retrieve the array of requirements for conditional conformances to
  /// the ith conditional conformance to a invertible protocol.
  llvm::ArrayRef<GenericConditionalInvertibleProtocolRequirement>
  getConditionalInvertibleProtocolRequirementsAt(unsigned i) const {
    auto counts = getConditionalInvertibleProtocolRequirementCounts();
    assert(i < counts.size());

    unsigned startIndex = (i == 0) ? 0 : counts[i-1].count;
    unsigned endIndex = counts[i].count;

    auto basePtr =
      this->template
        getTrailingObjects<GenericConditionalInvertibleProtocolRequirement>();
    return { basePtr + startIndex, basePtr + endIndex };
  }

  /// Retrieve the array of requirements for conditional conformances to
  /// the ith conditional conformance to a invertible protocol.
  llvm::ArrayRef<GenericConditionalInvertibleProtocolRequirement>
  getConditionalInvertibleProtocolRequirementsFor(
      InvertibleProtocolKind kind
  ) const {
    if (!asSelf()->hasConditionalInvertedProtocols())
      return { };

    auto conditionallyInverted = getConditionalInvertedProtocols();
    if (!conditionallyInverted.contains(kind))
      return { };

    // Count the number of "set" bits up to (but not including) the
    // bit we're looking at.
    unsigned targetBit = static_cast<uint8_t>(kind);
    auto invertedBits = conditionallyInverted.rawBits();
    unsigned priorBits = 0;
    for (unsigned i = 0; i != targetBit; ++i) {
      if (invertedBits & 0x01)
        ++priorBits;
      invertedBits = invertedBits >> 1;
    }

    return getConditionalInvertibleProtocolRequirementsAt(priorBits);
  }

  const TargetGenericContext<Runtime> *getGenericContext() const {
    if (!asSelf()->isGeneric())
      return nullptr;
    // The generic context header should always be immediately followed in
    // memory by trailing parameter and requirement descriptors.
    auto *header = reinterpret_cast<const char *>(&getGenericContextHeader());
    return reinterpret_cast<const TargetGenericContext<Runtime> *>(
      header - sizeof(TargetGenericContext<Runtime>));
  }

  llvm::ArrayRef<GenericParamDescriptor> getGenericParams() const {
    if (!asSelf()->isGeneric())
      return {};

    return {this->template getTrailingObjects<GenericParamDescriptor>(),
            getGenericContextHeader().NumParams};
  }
  
  llvm::ArrayRef<GenericRequirementDescriptor> getGenericRequirements() const {
    if (!asSelf()->isGeneric())
      return {};
    return {this->template getTrailingObjects<GenericRequirementDescriptor>(),
            getGenericContextHeader().NumRequirements};
  }
  
  GenericPackShapeHeader getGenericPackShapeHeader() const {
    if (!asSelf()->isGeneric())
      return {0, 0};
    if (!getGenericContextHeader().Flags.hasTypePacks())
      return {0, 0};
    return *this->template getTrailingObjects<GenericPackShapeHeader>();
  }

  llvm::ArrayRef<GenericPackShapeDescriptor> getGenericPackShapeDescriptors() const {
    auto header = getGenericPackShapeHeader();
    if (header.NumPacks == 0)
      return {};

    return {this->template getTrailingObjects<GenericPackShapeDescriptor>(),
            header.NumPacks};
  }

  GenericValueHeader getGenericValueHeader() const {
    if (!asSelf()->isGeneric())
      return {0};
    if (!getGenericContextHeader().Flags.hasValues())
      return {0};
    return *this->template getTrailingObjects<GenericValueHeader>();
  }

  llvm::ArrayRef<GenericValueDescriptor> getGenericValueDescriptors() const {
    auto header = getGenericValueHeader();

    if (header.NumValues == 0)
      return {};

    return {this->template getTrailingObjects<GenericValueDescriptor>(),
            header.NumValues};
  }

  RuntimeGenericSignature<Runtime> getGenericSignature() const {
    if (!asSelf()->isGeneric()) return RuntimeGenericSignature<Runtime>();
    return {getGenericContextHeader(),
            getGenericParams().data(),
            getGenericRequirements().data(),
            getGenericPackShapeHeader(),
            getGenericPackShapeDescriptors().data(),
            getGenericValueHeader(),
            getGenericValueDescriptors().data()};
  }

protected:
  size_t numTrailingObjects(OverloadToken<GenericContextHeaderType>) const {
    return asSelf()->isGeneric() ? 1 : 0;
  }
  
  size_t numTrailingObjects(OverloadToken<GenericParamDescriptor>) const {
    return asSelf()->isGeneric() ? getGenericContextHeader().NumParams : 0;
  }

  size_t numTrailingObjects(OverloadToken<GenericRequirementDescriptor>) const {
    return asSelf()->isGeneric() ? getGenericContextHeader().NumRequirements : 0;
  }

  size_t numTrailingObjects(OverloadToken<GenericPackShapeHeader>) const {
    if (!asSelf()->isGeneric())
      return 0;

    return getGenericContextHeader().Flags.hasTypePacks() ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<GenericPackShapeDescriptor>) const {
    if (!asSelf()->isGeneric())
      return 0;

    if (!getGenericContextHeader().Flags.hasTypePacks())
      return 0;

    return getGenericPackShapeHeader().NumPacks;
  }

  size_t numTrailingObjects(
      OverloadToken<ConditionalInvertibleProtocolSet>
  ) const {
    return asSelf()->hasConditionalInvertedProtocols() ? 1 : 0;
  }

  unsigned getNumConditionalInvertibleProtocolsRequirementCounts() const {
    if (!asSelf()->hasConditionalInvertedProtocols())
      return 0;

    return popcount(getConditionalInvertedProtocols().rawBits());
  }

  size_t numTrailingObjects(
      OverloadToken<ConditionalInvertibleProtocolsRequirementCount>
  ) const {
    return getNumConditionalInvertibleProtocolsRequirementCounts();
  }

  size_t numTrailingObjects(
      OverloadToken<GenericConditionalInvertibleProtocolRequirement>
  ) const {
    auto counts = getConditionalInvertibleProtocolRequirementCounts();
    return counts.empty() ? 0 : counts.back().count;
  }

  size_t numTrailingObjects(OverloadToken<GenericValueHeader>) const {
    if (!asSelf()->isGeneric())
      return 0;

    return getGenericContextHeader().Flags.hasValues() ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<GenericValueDescriptor>) const {
    if (!asSelf()->isGeneric())
      return 0;

    if (!getGenericContextHeader().Flags.hasValues())
      return 0;

    return getGenericValueHeader().NumValues;
  }

#if defined(_MSC_VER) && _MSC_VER < 1920
#undef OverloadToken
#endif

};

/// Description of a generic context.
template<typename Runtime>
struct TargetGenericContext final
  : TrailingGenericContextObjects<TargetGenericContext<Runtime>,
                                  TargetGenericContextDescriptorHeader>
{
  // This struct is supposed to be empty, but TrailingObjects respects the
  // unique-address-per-object C++ rule, so even if this type is empty, the
  // trailing objects will come after one byte of padding. This dummy field
  // takes up space to make the offset of the trailing objects portable.
  unsigned _dummy;

  bool isGeneric() const { return true; }
};

} // end namespace swift

#endif
