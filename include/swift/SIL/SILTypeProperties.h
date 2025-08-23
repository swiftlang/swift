//===--- SILTypeProperties.h - Properties of SIL types ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILTYPEPROPERTIES_H
#define SWIFT_SIL_SILTYPEPROPERTIES_H

namespace swift {

/// Is a lowered SIL type trivial?  That is, are copies ultimately just
/// bit-copies, and it takes no work to destroy a value?
enum IsTrivial_t : bool {
  IsNotTrivial = false,
  IsTrivial = true
};

/// Is a lowered SIL type the Builtin.RawPointer or a struct/tuple/enum which
/// contains a Builtin.RawPointer?
/// HasRawPointer is true only for types that are known to contain
/// Builtin.RawPointer. It is not assumed true for generic or resilient types.
enum HasRawPointer_t : bool {
  DoesNotHaveRawPointer = false,
  HasRawPointer = true
};

/// Is a lowered SIL type fixed-ABI?  That is, can the current context
/// assign it a fixed size and alignment and perform value operations on it
/// (such as copies, destroys, constructions, and projections) without
/// metadata?
///
/// Note that a fully concrete type can be non-fixed-ABI without being
/// itself resilient if it contains a subobject which is not fixed-ABI.
///
/// Also note that we're only concerned with the external value ABI here:
/// resilient class types are still fixed-ABI, indirect enum cases do not
/// affect the fixed-ABI-ness of the enum, and so on.
enum IsFixedABI_t : bool {
  IsNotFixedABI = false,
  IsFixedABI = true
};

/// Is a lowered SIL type address-only?  That is, is the current context
/// required to keep the value in memory for some reason?
///
/// A type might be address-only because:
///
///   - it is not fixed-size (e.g. because it is a resilient type) and
///     therefore cannot be loaded into a statically-boundable set of
///     registers; or
///
///   - it is semantically bound to memory, either because its storage
///     address is used by the language runtime to implement its semantics
///     (as with a weak reference) or because its representation is somehow
///     address-dependent (as with something like a relative pointer).
///
/// An address-only type can be fixed-layout and/or trivial.
/// A non-fixed-layout type is always address-only.
enum IsAddressOnly_t : bool {
  IsNotAddressOnly = false,
  IsAddressOnly = true
};

/// Is this type somewhat like a reference-counted type?
enum IsReferenceCounted_t : bool {
  IsNotReferenceCounted = false,
  IsReferenceCounted = true
};

/// Is this type address only because it's resilient?
enum IsResilient_t : bool {
  IsNotResilient = false,
  IsResilient = true
};

/// Does this type contain an opaque result type that affects type lowering?
enum IsTypeExpansionSensitive_t : bool {
  IsNotTypeExpansionSensitive = false,
  IsTypeExpansionSensitive = true
};

/// Is the type infinitely defined in terms of itself? (Such types can never
/// be concretely instantiated, but may still arise from generic specialization.)
enum IsInfiniteType_t : bool {
  IsNotInfiniteType = false,
  IsInfiniteType = true,
};

/// Does this type contain any pack-like thing.
enum HasPack_t : bool {
  HasNoPack = false,
  HasPack = true,
};

/// Is the type addressable-for-dependencies?
///
/// Values of an addressable-for-dependency type are passed indirectly into
/// functions that specify a return value lifetime dependency on the value.
/// This allows the dependent value to safely contain pointers to the in-memory
/// representation of the source of the dependency.
enum IsAddressableForDependencies_t : bool {
  IsNotAddressableForDependencies = false,
  IsAddressableForDependencies = true,
};

/// Is a lowered SIL type a struct with `@_rawLayout` or contains such a struct?
enum HasRawLayout_t : bool {
  DoesNotHaveRawLayout = false,
  HasRawLayout = true
};

class SILTypeProperties {
  // These are chosen so that bitwise-or merges the flags properly.
  //
  // clang-format off
  enum : unsigned {
    NonTrivialFlag                 = 1 << 0,
    NonFixedABIFlag                = 1 << 1,
    AddressOnlyFlag                = 1 << 2,
    ResilientFlag                  = 1 << 3,
    TypeExpansionSensitiveFlag     = 1 << 4,
    InfiniteFlag                   = 1 << 5,
    HasRawPointerFlag              = 1 << 6,
    LexicalFlag                    = 1 << 7,
    HasPackFlag                    = 1 << 8,
    AddressableForDependenciesFlag = 1 << 9,
    HasRawLayoutFlag               = 1 << 10,
  };
  // clang-format on

  uint16_t Flags;

public:
  /// Construct a default SILTypeProperties, which corresponds to
  /// a trivial, loadable, fixed-layout type.
  constexpr SILTypeProperties() : Flags(0) {}

  constexpr SILTypeProperties(
      IsTrivial_t isTrivial, IsFixedABI_t isFixedABI,
      IsAddressOnly_t isAddressOnly, IsResilient_t isResilient,
      IsTypeExpansionSensitive_t isTypeExpansionSensitive =
          IsNotTypeExpansionSensitive,
      HasRawPointer_t hasRawPointer = DoesNotHaveRawPointer,
      IsLexical_t isLexical = IsNotLexical, HasPack_t hasPack = HasNoPack,
      IsAddressableForDependencies_t isAFD = IsNotAddressableForDependencies,
      HasRawLayout_t hasRawLayout = DoesNotHaveRawLayout)
      : Flags((isTrivial ? 0U : NonTrivialFlag) |
              (isFixedABI ? 0U : NonFixedABIFlag) |
              (isAddressOnly ? AddressOnlyFlag : 0U) |
              (isResilient ? ResilientFlag : 0U) |
              (isTypeExpansionSensitive ? TypeExpansionSensitiveFlag : 0U) |
              (hasRawPointer ? HasRawPointerFlag : 0U) |
              (isLexical ? LexicalFlag : 0U) |
              (hasPack ? HasPackFlag : 0U) |
              (isAFD ? AddressableForDependenciesFlag : 0U) |
              (hasRawLayout ? HasRawLayoutFlag : 0U)) {}

  constexpr bool operator==(SILTypeProperties p) const {
    return Flags == p.Flags;
  }

  static constexpr SILTypeProperties forTrivial() {
    return {IsTrivial, IsFixedABI, IsNotAddressOnly, IsNotResilient};
  }

  static constexpr SILTypeProperties forTrivialOpaque() {
    return {IsTrivial, IsFixedABI, IsNotAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, HasRawPointer, IsNotLexical,
            HasNoPack, IsAddressableForDependencies};
  }

  static constexpr SILTypeProperties forRawPointer() {
    return {IsTrivial, IsFixedABI, IsNotAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, HasRawPointer};
  }

  static constexpr SILTypeProperties forReference() {
    return {IsNotTrivial, IsFixedABI, IsNotAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, DoesNotHaveRawPointer, IsLexical};
  }

  static constexpr SILTypeProperties forOpaque() {
    return {IsNotTrivial, IsNotFixedABI, IsAddressOnly, IsNotResilient,
            IsNotTypeExpansionSensitive, HasRawPointer, IsLexical,
            HasNoPack, IsAddressableForDependencies, HasRawLayout};
  }

  static constexpr SILTypeProperties forResilient() {
    return {IsTrivial, IsFixedABI, IsNotAddressOnly, IsResilient,
            IsNotTypeExpansionSensitive, HasRawPointer, IsNotLexical,
            HasNoPack, IsNotAddressableForDependencies};
  }

  void addSubobject(SILTypeProperties other) {
    Flags |= other.Flags;
  }

  IsTrivial_t isTrivial() const {
    return IsTrivial_t((Flags & NonTrivialFlag) == 0);
  }
  HasRawPointer_t isOrContainsRawPointer() const {
    return HasRawPointer_t((Flags & HasRawPointerFlag) != 0);
  }
  IsFixedABI_t isFixedABI() const {
    return IsFixedABI_t((Flags & NonFixedABIFlag) == 0);
  }
  IsAddressOnly_t isAddressOnly() const {
    return IsAddressOnly_t((Flags & AddressOnlyFlag) != 0);
  }
  bool isLoadable() const {
    return !isAddressOnly();
  }
  IsResilient_t isResilient() const {
    return IsResilient_t((Flags & ResilientFlag) != 0);
  }
  IsTypeExpansionSensitive_t isTypeExpansionSensitive() const {
    return IsTypeExpansionSensitive_t(
        (Flags & TypeExpansionSensitiveFlag) != 0);
  }
  IsInfiniteType_t isInfinite() const {
    return IsInfiniteType_t((Flags & InfiniteFlag) != 0);
  }
  IsLexical_t isLexical() const {
    return IsLexical_t((Flags & LexicalFlag) != 0);
  }
  HasPack_t isOrContainsPack() const {
    return HasPack_t((Flags & HasPackFlag) != 0);
  }
  IsAddressableForDependencies_t isAddressableForDependencies() const {
    return IsAddressableForDependencies_t(
                              (Flags & AddressableForDependenciesFlag) != 0);
  }
  HasRawLayout_t isOrContainsRawLayout() const {
    return HasRawLayout_t((Flags & HasRawLayoutFlag) != 0);
  }

  void setNonTrivial() { Flags |= NonTrivialFlag; }
  void setIsOrContainsRawPointer() { Flags |= HasRawPointerFlag; }

  void setNonFixedABI() { Flags |= NonFixedABIFlag; }
  void setAddressOnly() { Flags |= AddressOnlyFlag; }
  void setTypeExpansionSensitive(
      IsTypeExpansionSensitive_t isTypeExpansionSensitive) {
    Flags = (Flags & ~TypeExpansionSensitiveFlag) |
            (isTypeExpansionSensitive ? TypeExpansionSensitiveFlag : 0);
  }
  void setInfinite() { Flags |= InfiniteFlag; }
  void setLexical(IsLexical_t isLexical) {
    Flags = (Flags & ~LexicalFlag) | (isLexical ? LexicalFlag : 0);
  }
  void setHasPack() { Flags |= HasPackFlag; }
  void setAddressableForDependencies() {
    Flags |= AddressableForDependenciesFlag;
  }
  void setHasRawLayout() {
    Flags |= HasRawLayoutFlag;
  }
};

} // end namespace swift

#endif
