//===--- SILValueProjection.h -----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file defines SILValueProjection, a class containing a SILValue base
/// and a ProjectionPath. It is used as the base class for LSLocation and
/// LSValue.
///
/// In the case of LSLocation, the base represents the base of the allocated
/// objects and the ProjectionPath tells which field in the object the
/// LSLocation represents.
///
/// In the case of LSValue, the base represents the root of loaded or stored
/// value it represents. And the ProjectionPath represents the field in the
/// loaded/store value the LSValue represents.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VALUEPROJECTION_H
#define SWIFT_SIL_VALUEPROJECTION_H

#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/TypeExpansionAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// forward declarations.
class SILValueProjection;
class LSLocation;
class LSValue;

//===----------------------------------------------------------------------===//
//                           SILValue Projection
//===----------------------------------------------------------------------===//

class SILValueProjection {
public:
  enum KeyKind : uint8_t { Empty = 0, Tombstone, Normal };

protected:
  /// The base of the object.
  SILValue Base;
  /// Empty key, tombstone key or normal key.
  KeyKind Kind;
  /// The path to reach the accessed field of the object.
  Optional<ProjectionPath> Path;

public:
  /// Constructors.
  SILValueProjection() : Base(), Kind(Normal) {}
  SILValueProjection(KeyKind Kind) : Base(), Kind(Kind) {}
  SILValueProjection(SILValue B) : Base(B), Kind(Normal) {}
  SILValueProjection(SILValue B, const Optional<ProjectionPath> &P,
                     KeyKind Kind = Normal)
      : Base(B), Kind(Kind), Path(P) {}

  /// Virtual destructor.
  virtual ~SILValueProjection() {}

  /// Copy constructor.
  SILValueProjection(const SILValueProjection &RHS) {
    Base = RHS.Base;
    Kind = RHS.Kind;
    Path = RHS.Path;
  }

  /// Assignment operator.
  SILValueProjection &operator=(const SILValueProjection &RHS) {
    Base = RHS.Base;
    Kind = RHS.Kind;
    Path = RHS.Path;
    return *this;
  }

  /// Getters for SILValueProjection.
  KeyKind getKind() const { return Kind; }
  SILValue getBase() const { return Base; }
  const Optional<ProjectionPath> &getPath() const { return Path; }

  /// Reset the SILValueProjection, i.e. clear base and path.
  void reset() {
    Base = SILValue();
    Kind = Normal;
    Path.reset();
  }

  /// Returns whether the SILValueProjection has been initialized properly.
  virtual bool isValid() const { return Base && Path.hasValue(); }

  /// Returns true if the SILValueProjection has a non-empty projection path.
  bool hasEmptyProjectionPath() const { return !Path.getValue().size(); }

  /// return true if that the two objects have the same base but access different
  /// fields of the base object.
  bool hasNonEmptySymmetricPathDifference(const SILValueProjection &RHS) const {
    const ProjectionPath &P = RHS.Path.getValue();
    return Path.getValue().hasNonEmptySymmetricDifference(P);
  }

  /// Subtract the given path from the ProjectionPath.
  void removePathPrefix(Optional<ProjectionPath> &P) {
    if (!P.hasValue())
      return;
    // Remove prefix does not modify the Path in-place.
    Path = ProjectionPath::removePrefix(Path.getValue(), P.getValue());
  }

  /// Return true if the RHS have identical projection paths.
  /// If both SILValueProjection have empty paths, they are treated as having
  /// identical projection path.
  bool hasIdenticalProjectionPath(const SILValueProjection &RHS) const {
    // If both Paths have no value, then the 2 SILValueProjections are
    // different.
    if (!Path.hasValue() && !RHS.Path.hasValue())
      return false;
    // If 1 Path has value while the other does not, then the 2
    // SILValueProjections are different.
    if (Path.hasValue() != RHS.Path.hasValue())
      return false;
    // If both Paths are empty, then the 2 SILValueProjections are the same.
    if (Path.getValue().empty() && RHS.Path.getValue().empty())
      return true;
    // If both Paths have different values, then the 2 SILValueProjections are
    // different.
    if (Path.getValue() != RHS.Path.getValue())
      return false;
    // Otherwise, the 2 SILValueProjections are the same.
    return true;
  }

  /// Comparisons.
  bool operator!=(const SILValueProjection &RHS) const {
    return !(*this == RHS);
  }
  bool operator==(const SILValueProjection &RHS) const {
    // If type is not the same, then SILValueProjections different.
    if (Kind != RHS.Kind)
      return false;
    // Return true if this is a Tombstone or Empty.
    if (Kind == Empty || Kind == Tombstone)
      return true;
    // If Base is different, then SILValueProjections different.
    if (Base != RHS.Base)
      return false;
    // If the projection paths are different, then SILValueProjections are
    // different.
    if (!hasIdenticalProjectionPath(RHS))
      return false;
    // These SILValueProjections represent the same memory location.
    return true;
  }

  /// Print the SILValueProjection.
  virtual void print(SILModule *Mod); 

  /// Create a path of AddrProjection or ValueProjection with the given VA
  /// and Path.
  static SILValue createExtract(SILValue VA,
                                const Optional<ProjectionPath> &Path,
                                SILInstruction *Inst, bool IsValExt);
};

static inline llvm::hash_code hash_value(const SILValueProjection &S) {
  const SILValue Base = S.getBase();
  const Optional<ProjectionPath> &Path = S.getPath();
  llvm::hash_code HC = llvm::hash_combine(Base.getOpaqueValue());
  if (!Path.hasValue())
    return HC;
  return llvm::hash_combine(HC, hash_value(Path.getValue()));
}

//===----------------------------------------------------------------------===//
//                            Load Store Value
//===----------------------------------------------------------------------===//

using LSLocationValueMap = llvm::DenseMap<LSLocation, LSValue>;
using LSValueList = llvm::SmallVector<LSValue, 8>;
using LSValueIndexMap = llvm::SmallDenseMap<LSValue, unsigned, 32>;
using ValueTableMap = llvm::SmallMapVector<unsigned, unsigned, 8>;

/// A LSValue is an abstraction of an object field value in program. It
/// consists of a base that is the tracked SILValue, and a projection path to
/// the represented field.
///
/// In this example below, 2 LSValues will be created for the 2 stores,
/// they will have %6 and %7 as their Bases and empty projection paths.
///
///  struct A {
///    var a: Int
///    var b: Int
///  }
///
/// sil hidden @test_1 : $@convention(thin) () -> () {
///   %0 = alloc_stack $A  // var x                   // users: %4, %7
///   %5 = integer_literal $Builtin.Int64, 19         // user: %6
///   %6 = struct $Int (%5 : $Builtin.Int64)          // user: %8
///   %7 = struct_element_addr %0 : $*A, #A.a         // user: %8
///   store %6 to %7 : $*Int                          // id: %8
///   %9 = integer_literal $Builtin.Int64, 20         // user: %10
///   %10 = struct $Int (%9 : $Builtin.Int64)         // user: %12
///   %11 = struct_element_addr %0 : $*A, #A.b        // user: %12
///   store %10 to %11 : $*Int                        // id: %12
/// }
///
/// In this example below, 2 LSValues will be created with %3 as their
/// bases and #a and #b as their projection paths respectively.
///
/// sil hidden @test_1 : $@convention(thin) () -> () {
///   %0 = alloc_stack $A  // var x                   // users: %4, %6
///   // function_ref a.A.init (a.A.Type)() -> a.A
///   %1 = function_ref @a.A.init : $@convention(thin) (@thin A.Type) -> A
///   %2 = metatype $@thin A.Type                     // user: %3
///   %3 = apply %1(%2) : $@convention(thin) (@thin A.Type) -> A // user: %4
///   store %3 to %0 : $*A                            // id: %4
/// }
///
/// LSValue can take 2 forms.
///
/// 1. It can take a concrete value, i.e. with a valid Base and ProjectionPath.
/// using the extract function, it can be materialized in IR.
///
/// 2. It can represent a covering set of LSValues from all predecessor
/// blocks. and to get the forwardable SILValue, we need to go to its
/// predecessors to materialize each one of them and create the forwarding
/// SILValue through a SILArgument.
///
/// Given a set of LSLocations and their available LSValues,
/// reduce will create the forwarding SILValue by merging them while
/// creating as few value extraction and aggregation as possible.
///
/// NOTE: ProjectionPath in LSValue could be replaced by the
/// ProjectionPath in the LSLocation, but that would require we break the
/// ProjectionPath into 2 parts, 1 for the Base to the accessed (aggregate) node
/// and another 1 for the accessed (aggregate) node to the leaf node the
/// LSLocation represents.
///
/// This makes implementing the LSLocationVault more difficult, as there are
/// multiple ways to represent the same LSLocations (even they have the same
/// base).
///
class LSValue : public SILValueProjection {
  /// If this is a covering value, we need to go to each predecessor to
  /// materialize the value.
  bool IsCoveringValue;
public:
  /// Constructors.
  LSValue() : SILValueProjection(), IsCoveringValue(false) {}
  LSValue(SILValue B, const ProjectionPath &P)
      : SILValueProjection(B, P), IsCoveringValue(false) {
    assert(isValid() && "Trying to create invalid LSValue");
  }
  LSValue(KeyKind Kind) : SILValueProjection(Kind), IsCoveringValue(false) {}
  LSValue(bool CSVal) : SILValueProjection(Normal), IsCoveringValue(CSVal) {}

  /// Copy constructor.
  LSValue(const LSValue &RHS) : SILValueProjection(RHS) {
    IsCoveringValue = RHS.IsCoveringValue;
  }

  /// Assignment operator.
  LSValue &operator=(const LSValue &RHS) {
    SILValueProjection::operator=(RHS);
    IsCoveringValue = RHS.IsCoveringValue;
    return *this;
  }

  /// Comparisons.
  bool operator!=(const LSValue &RHS) const { return !(*this == RHS); }
  bool operator==(const LSValue &RHS) const {
    if (IsCoveringValue && RHS.isCoveringValue())
      return true;
    if (IsCoveringValue != RHS.isCoveringValue())
      return false;
    return SILValueProjection::operator==(RHS);
  }

  /// Returns whether the LSValue has been initialized properly.
  bool isValid() const {
    if (IsCoveringValue)
      return true;
    return SILValueProjection::isValid();
  }

  /// Take the last level projection off. Return the modified LSValue.
  LSValue &stripLastLevelProjection() {
    Path.getValue().pop_back();
    return *this;
  }

  /// Returns true if this LSValue is a covering value.
  bool isCoveringValue() const { return IsCoveringValue; }

  /// Materialize the SILValue that this LSValue represents.
  ///
  /// In the case where we have a single value this can be materialized by
  /// applying Path to the Base.
  SILValue materialize(SILInstruction *Inst) {
    if (IsCoveringValue)
      return SILValue();
    return SILValueProjection::createExtract(Base, Path, Inst, true);
  }

  void print(SILModule *Mod) {
    if (IsCoveringValue) {
      llvm::outs() << "Covering Value";
      return;
    }
    SILValueProjection::print(Mod);
  }

  //============================//
  //       static functions.    //
  //============================//

  /// Expand this SILValue to all individual fields it contains.
  static void expand(SILValue Base, SILModule *Mod, LSValueList &Vals,
                     TypeExpansionAnalysis *TE);

  /// Given a memory location and a map between the expansions of the location
  /// and their corresponding values, try to come up with a single SILValue this
  /// location holds. This may involve extracting and aggregating available
  /// values.
  ///
  /// NOTE: reduce assumes that every component of the location has a concrete
  /// (i.e. not coverings set) available value in LocAndVal.
  ///
  /// TODO: we do not really need the llvm::DenseMap<LSLocation, LSValue> here
  /// we only need a map between the projection tree of a SILType and the value
  /// each leaf node takes. This will be implemented once ProjectionPath memory
  /// cost is reduced and made copyable (its copy constructor is deleted at the
  /// moment).
  static SILValue reduce(LSLocation &Base, SILModule *Mod,
                         LSLocationValueMap &LocAndVal,
                         SILInstruction *InsertPt,
                         TypeExpansionAnalysis *TE);
};

static inline llvm::hash_code hash_value(const LSValue &V) {
  llvm::hash_code HC = llvm::hash_combine(V.isCoveringValue());
  if (V.isCoveringValue())
    return HC;
  return llvm::hash_combine(HC, hash_value((SILValueProjection)V));
}

//===----------------------------------------------------------------------===//
//                              Memory Location
//===----------------------------------------------------------------------===//
/// Type declarations.
using LSLocationSet = llvm::DenseSet<LSLocation>;
using LSLocationList = llvm::SmallVector<LSLocation, 8>;
using LSLocationIndexMap = llvm::SmallDenseMap<LSLocation, unsigned, 32>;
using LSLocationBaseMap = llvm::DenseMap<SILValue, LSLocation>;

/// This class represents a field in an allocated object. It consists of a
/// base that is the tracked SILValue, and a projection path to the
/// represented field.
///
/// The base here will point to the actual object this inst is accessing,
/// not this particular field. We call this LSLocation canonicalization.
///
/// e.g. %1 = alloc_stack $S
///      %2 = struct_element_addr %1, #a
///      store %3 to %2 : $*Int
///
/// Base will point to %1, but not %2. Projection path will indicate which
/// field is accessed.
///
/// Canonicalizing LSLocation reduces the # of the LSLocations we keep in
/// the LSLocationVault, and this in turn reduces the # of bits each basic
/// block keeps.
///
/// Moreover, without canonicalization, it's more difficult to implement the
/// intersection operator in DSE/RLE?
///
/// We basically need to compare every pair of LSLocation and find the ones
/// that must alias O(n^2). Or we need to go through every LSLocation in the
/// vault and turn on/off the bits for the MustAlias ones whenever we want to
/// turn a LSLocation bit on/off. Both are expensive.
///
/// Canonicalizing suffers from the same problem, but to a lesser extend. i.e.
/// 2 LSLocations with different bases but happen to be the same object and
/// field. By doing canonicalization, the intersection is simply a bitwise AND
/// (No AA involved).
///
class LSLocation : public SILValueProjection {
public:
  /// Constructors.
  LSLocation() {}
  LSLocation(SILValue B, const Optional<ProjectionPath> &P, KeyKind K = Normal)
      : SILValueProjection(B, P, K) {}
  LSLocation(KeyKind Kind) : SILValueProjection(Kind) {}
  /// Use the concatenation of the 2 ProjectionPaths as the Path.
  LSLocation(SILValue B, const ProjectionPath &BP, const ProjectionPath &AP)
      : SILValueProjection(B) {
    ProjectionPath P((*Base).getType());
    P.append(BP);
    P.append(AP);
    Path = P;
  }

  /// Initialize a location with a new set of base, projectionpath and kind.
  void init(SILValue B, const Optional<ProjectionPath> &P, KeyKind K= Normal) {
    Base = B;
    Path = P;
    Kind = K;
  }

  /// Copy constructor.
  LSLocation(const LSLocation &RHS) : SILValueProjection(RHS) {}

  /// Assignment operator.
  LSLocation &operator=(const LSLocation &RHS) {
    SILValueProjection::operator=(RHS);
    return *this;
  }

  /// Returns the type of the object the LSLocation represents.
  SILType getType(SILModule *M) {
     return Path.getValue().getMostDerivedType(*M);
  }

  /// Get the first level locations based on this location's first level
  /// projection.
  void getFirstLevelLSLocations(LSLocationList &Locs, SILModule *Mod);

  /// Check whether the 2 LSLocations may alias each other or not.
  bool isMayAliasLSLocation(const LSLocation &RHS, AliasAnalysis *AA);

  /// Check whether the 2 LSLocations must alias each other or not.
  bool isMustAliasLSLocation(const LSLocation &RHS, AliasAnalysis *AA);

  /// Check whether the LSLocation can escape the current function.
  bool isNonEscapingLocalLSLocation(SILFunction *Fn, EscapeAnalysis *EA);

  //============================//
  //       static functions.    //
  //============================//

  /// Expand this location to all individual fields it contains.
  ///
  /// In SIL, we can have a store to an aggregate and loads from its individual
  /// fields. Therefore, we expand all the operations on aggregates onto
  /// individual fields and process them separately.
  static void expand(LSLocation Base, SILModule *Mod, LSLocationList &Locs,
                     TypeExpansionAnalysis *TE);

  /// Given a set of locations derived from the same base, try to merge/reduce
  /// them into smallest number of LSLocations possible.
  static void reduce(LSLocation Base, SILModule *Mod, LSLocationSet &Locs,
                     TypeExpansionAnalysis *TE);

  /// Enumerate the given Mem LSLocation.
  static void enumerateLSLocation(SILModule *M, SILValue Mem,
                                  std::vector<LSLocation> &LSLocationVault,
                                  LSLocationIndexMap &LocToBit,
                                  LSLocationBaseMap &BaseToLoc,
                                  TypeExpansionAnalysis *TE);

  /// Enumerate all the locations in the function.
  static void enumerateLSLocations(SILFunction &F,
                                   std::vector<LSLocation> &LSLocationVault,
                                   LSLocationIndexMap &LocToBit,
                                   LSLocationBaseMap &BaseToLoc,
                                   TypeExpansionAnalysis *TE);
};

static inline llvm::hash_code hash_value(const LSLocation &L) {
  return llvm::hash_combine(hash_value((SILValueProjection)L));
}

} // end swift namespace

/// LSLocation and LSValue are used in DenseMap.
namespace llvm {
using swift::SILValueProjection;
using swift::LSLocation;
using swift::LSValue;

template <> struct DenseMapInfo<LSValue> {
  static inline LSValue getEmptyKey() {
    return LSValue(SILValueProjection::Empty);
  }
  static inline LSValue getTombstoneKey() {
    return LSValue(SILValueProjection::Tombstone);
  }
  static inline unsigned getHashValue(const LSValue &Val) {
    return hash_value(Val);
  }
  static bool isEqual(const LSValue &LHS, const LSValue &RHS) {
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<LSLocation> {
  static inline LSLocation getEmptyKey() {
    return LSLocation(SILValueProjection::Empty);
  }
  static inline LSLocation getTombstoneKey() {
    return LSLocation(SILValueProjection::Tombstone);
  }
  static inline unsigned getHashValue(const LSLocation &Loc) {
    return hash_value(Loc);
  }
  static bool isEqual(const LSLocation &LHS, const LSLocation &RHS) {
    return LHS == RHS;
  }
};

} // namespace llvm

#endif // SWIFT_SIL_VALUEPROJECTION_H
