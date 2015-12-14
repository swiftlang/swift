//===------------------------ SILValueProjection.h ---------------------- -===//
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
///
/// This file defines SILValueProjection, a class containing a SILValue base
/// and a ProjectionPath. It is used as the base class for LSLocation and
/// LSValue.
///
/// In the case of LSLocation, the base represents the base of the allocated
/// objects and the ProjectionPath tells which field in the object the
/// LSLocation represents.
///
/// In the case of LSValue, the base represents the root of loaded or
/// stored value it represents. And the ProjectionPath represents the field in
/// the loaded/store value the LSValue represents.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_VALUEPROJECTION_H
#define SWIFT_SIL_VALUEPROJECTION_H

#include "swift/SIL/Projection.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
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
  enum KeyKind : uint8_t { EmptyKey = 0, TombstoneKey, NormalKey };

protected:
  /// The base of the object.
  SILValue Base;
  /// Empty key, tombstone key or normal key.
  KeyKind Kind;
  /// The path to reach the accessed field of the object.
  Optional<ProjectionPath> Path;

public:
  /// Constructors.
  SILValueProjection() : Base(), Kind(NormalKey) {}
  SILValueProjection(KeyKind Kind) : Base(), Kind(Kind) {}
  SILValueProjection(SILValue B) : Base(B), Kind(NormalKey) {}
  SILValueProjection(SILValue B, const ProjectionPath &P,
                     KeyKind Kind = NormalKey)
      : Base(B), Kind(Kind) {
    ProjectionPath T;
    T.append(P);
    Path = std::move(T);
  }

  /// Destructors.
  virtual ~SILValueProjection() {}

  /// Copy constructor.
  SILValueProjection(const SILValueProjection &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
  }

  SILValueProjection &operator=(const SILValueProjection &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return *this;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
    return *this;
  }

  /// Getters and setters for SILValueProjection.
  KeyKind getKind() const { return Kind; }
  void setKind(KeyKind K) { Kind = K; }
  SILValue getBase() const { return Base; }
  Optional<ProjectionPath> &getPath() { return Path; }

  /// Reset the SILValueProjection, i.e. clear base and path.
  void reset() {
    Base = SILValue();
    Path.reset();
    Kind = NormalKey;
  }

  /// Returns whether the SILValueProjection has been initialized properly.
  virtual bool isValid() const { return Base && Path.hasValue(); }

  /// Returns true if the LSValue has a non-empty projection path.
  bool hasEmptyProjectionPath() const { return !Path.getValue().size(); }

  /// Return false if one projection path is a prefix of another. false
  /// otherwise.
  bool hasNonEmptySymmetricPathDifference(const SILValueProjection &RHS) const {
    const ProjectionPath &P = RHS.Path.getValue();
    return Path.getValue().hasNonEmptySymmetricDifference(P);
  }

  /// Substract the given path from the ProjectionPath.
  void subtractPaths(Optional<ProjectionPath> &P) {
    if (!P.hasValue())
      return;
    ProjectionPath::subtractPaths(Path.getValue(), P.getValue());
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
    // Return true if this is a TombstoneKey or EmptyKey.
    if (Kind == EmptyKey || Kind == TombstoneKey)
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

  /// Returns the hashcode for the location.
  llvm::hash_code getHashCode() const {
    llvm::hash_code HC = llvm::hash_combine(
        Base.getDef(), Base.getResultNumber(), Base.getType());
    if (!Path.hasValue())
      return HC;
    HC = llvm::hash_combine(HC, hash_value(Path.getValue()));
    return HC;
  }

  virtual void print() const;

  /// Create a path of ValueProjection with the given VA and Path.
  static SILValue createExtract(SILValue VA, Optional<ProjectionPath> &Path,
                                SILInstruction *Inst, bool IsValExt);
};

//===----------------------------------------------------------------------===//
//                            Load Store Value
//===----------------------------------------------------------------------===//

using LSLocationValueMap = llvm::DenseMap<LSLocation, LSValue>;
using LSValueList = llvm::SmallVector<LSValue, 8>;
using LSValueIndexMap = llvm::DenseMap<LSValue, unsigned>;
using ValueTableMap = llvm::SmallMapVector<unsigned, unsigned, 8>;

/// This class represents either a single SILValue or a covering of values that
/// we can forward from via the introduction of a SILArgument. This enables us
/// to treat the case of having one value or multiple values and load and store
/// cases all at once abstractly and cleanly.
///
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
///   %7 = struct_element_addr %0#1 : $*A, #A.a       // user: %8
///   store %6 to %7 : $*Int                          // id: %8
///   %9 = integer_literal $Builtin.Int64, 20         // user: %10
///   %10 = struct $Int (%9 : $Builtin.Int64)         // user: %12
///   %11 = struct_element_addr %0#1 : $*A, #A.b      // user: %12
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
///   store %3 to %0#1 : $*A                          // id: %4
/// }
///
/// NOTE: LSValue can take 2 forms.
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
  LSValue(SILValue B) : SILValueProjection(B), IsCoveringValue(false) {}
  LSValue(SILValue B, const ProjectionPath &P)
      : SILValueProjection(B, P), IsCoveringValue(false) {}
  LSValue(KeyKind Kind) : SILValueProjection(Kind), IsCoveringValue(false) {}
  LSValue(bool CSVal) : SILValueProjection(NormalKey), IsCoveringValue(CSVal) {}

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
    return Base && Path.hasValue();
  }

  /// Take the last level projection off. Return the modified LSValue.
  LSValue &stripLastLevelProjection() {
    Path.getValue().remove_front();
    return *this;
  }

  /// Returns true if this LSValue is a covering value.
  bool isCoveringValue() const { return IsCoveringValue; }
  /// Mark this LSValue as a covering value.
  void setCoveringValue() {
    Base = SILValue();
    Path.reset();
    IsCoveringValue = true;
  }

  /// Materialize the SILValue that this LSValue represents.
  ///
  /// In the case where we have a single value this can be materialized by
  /// applying Path to the Base.
  ///
  /// In the case where we are handling a covering set, this is initially null
  /// and when we insert the PHI node, this is set to the SILArgument which
  /// represents the PHI node.
  SILValue materialize(SILInstruction *Inst) {
    if (IsCoveringValue)
      return SILValue();
    return SILValueProjection::createExtract(Base, Path, Inst, true);
  }

  void print() const {
    if (IsCoveringValue) {
      llvm::outs() << "Covering Value";
      return;
    }
    SILValueProjection::print();
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
  /// NOTE: reduce assumes that every component of the location has an concrete
  /// (i.e. not coverings set) available value in LocAndVal.
  ///
  /// TODO: we do not really need the llvm::DenseMap<LSLocation, LSValue> here
  /// we only need a map between the projection tree of a SILType and the value
  /// each leaf node takes. This will be implemented once ProjectionPath memory
  /// cost is reduced and made copyable (its copy constructor is deleted at the
  /// momemt). 
  static SILValue reduce(LSLocation &Base, SILModule *Mod,
                         LSLocationValueMap &LocAndVal,
                         SILInstruction *InsertPt,
                         TypeExpansionAnalysis *TE);

  /// Enumerate the given LSValue.
  static void enumerateLSValue(SILModule *M, SILValue Val,
                               std::vector<LSValue> &Vault,
                               LSValueIndexMap &ValToBit,
                               TypeExpansionAnalysis *TE);

  /// Enumerate all the LSValues in the function.
  static void enumerateLSValues(SILFunction &F, std::vector<LSValue> &Vault,
                                LSValueIndexMap &ValToBit,
                                TypeExpansionAnalysis *TE);
};

static inline llvm::hash_code hash_value(const LSValue &V) {
  if (V.isCoveringValue())
    return llvm::hash_combine(V.isCoveringValue());
  return llvm::hash_combine(V.getBase().getDef(), V.getBase().getResultNumber(),
                            V.getBase().getType());
}

//===----------------------------------------------------------------------===//
//                              Memory Location
//===----------------------------------------------------------------------===//
/// Type declarations.
using LSLocationSet = llvm::DenseSet<LSLocation>;
using LSLocationList = llvm::SmallVector<LSLocation, 8>;
using LSLocationIndexMap = llvm::DenseMap<LSLocation, unsigned>;

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
  LSLocation(SILValue B) : SILValueProjection(B) { initialize(B); }
  LSLocation(SILValue B, ProjectionPath &P, KeyKind Kind = NormalKey)
      : SILValueProjection(B, P, Kind) {}
  LSLocation(KeyKind Kind) : SILValueProjection(Kind) {}
  /// Use the concatenation of the 2 ProjectionPaths as the Path.
  LSLocation(SILValue B, const ProjectionPath &P1, const ProjectionPath &P2)
      : SILValueProjection(B) {
    ProjectionPath T;
    T.append(P1);
    T.append(P2);
    Path = std::move(T);
  }

  /// Copy constructor.
  LSLocation(const LSLocation &RHS) : SILValueProjection(RHS) {}

  /// Assignment operator.
  LSLocation &operator=(const LSLocation &RHS) {
    SILValueProjection::operator=(RHS);
    return *this;
  }

  /// Returns the type of the object the LSLocation represents.
  SILType getType() const {
    // Base might be a address type, e.g. from alloc_stack of struct,
    // enum or tuples.
    if (Path.getValue().empty())
      return Base.getType().getObjectType();
    return Path.getValue().front().getType().getObjectType();
  }

  /// Trace the given SILValue till the base of the accessed object. Also
  /// construct the projection path to the field accessed.
  void initialize(SILValue val);

  /// Get the first level locations based on this location's first level
  /// projection.
  void getFirstLevelLSLocations(LSLocationList &Locs, SILModule *Mod);

  /// Returns true if the LSLocation is local to this function, i.e.
  /// does not escape.
  ///
  /// TODO: we should look at the projection path as well. i.e. one field
  /// might escape but the object itself does not.
  ///
  bool isNonEscapingLocalLSLocation() {
    assert(isValid() && "Invalid memory location");
    // TODO: this does not have to be limited to allocstack.
    return isa<AllocStackInst>(Base) && isNonEscapingLocalObject(Base);
  }

  /// Check whether the 2 LSLocations may alias each other or not.
  bool isMayAliasLSLocation(const LSLocation &RHS, AliasAnalysis *AA);

  /// Check whether the 2 LSLocations must alias each other or not.
  bool isMustAliasLSLocation(const LSLocation &RHS, AliasAnalysis *AA);

  //============================//
  //       static functions.    //
  //============================//

  /// Expand this location to all individual fields it contains.
  ///
  /// In SIL, we can have a store to an aggregate and loads from its individual
  /// fields. Therefore, we expand all the operations on aggregates onto
  /// individual fields and process them separately.
  static void expand(LSLocation &Base, SILModule *Mod, LSLocationList &Locs,
                     TypeExpansionAnalysis *TE);

  /// Given a set of locations derived from the same base, try to merge/reduce
  /// them into smallest number of LSLocations possible.
  static void reduce(LSLocation &Base, SILModule *Mod, LSLocationSet &Locs,
                     TypeExpansionAnalysis *TE);

  /// Enumerate the given Mem LSLocation.
  static void enumerateLSLocation(SILModule *M, SILValue Mem,
                                  std::vector<LSLocation> &LSLocationVault,
                                  LSLocationIndexMap &LocToBit,
                                  TypeExpansionAnalysis *TE);

  /// Enumerate all the locations in the function.
  static void enumerateLSLocations(SILFunction &F,
                                   std::vector<LSLocation> &LSLocationVault,
                                   LSLocationIndexMap &LocToBit,
                                   TypeExpansionAnalysis *TE);
};

static inline llvm::hash_code hash_value(const LSLocation &L) {
  return llvm::hash_combine(L.getBase().getDef(), L.getBase().getResultNumber(),
                            L.getBase().getType());
}

} // end swift namespace

/// LSLocation is used in DenseMap, define functions required by DenseMap.
namespace llvm {
using swift::SILValueProjection;
using swift::LSLocation;
using swift::LSValue;

template <> struct DenseMapInfo<LSLocation> {
  static inline LSLocation getEmptyKey() {
    return LSLocation(SILValueProjection::EmptyKey);
  }
  static inline LSLocation getTombstoneKey() {
    return LSLocation(SILValueProjection::TombstoneKey);
  }
  static inline unsigned getHashValue(const LSLocation &Loc) {
    return hash_value(Loc);
  }
  static bool isEqual(const LSLocation &LHS, const LSLocation &RHS) {
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<LSValue> {
  static inline LSValue getEmptyKey() {
    return LSValue(SILValueProjection::EmptyKey);
  }
  static inline LSValue getTombstoneKey() {
    return LSValue(SILValueProjection::TombstoneKey);
  }
  static inline unsigned getHashValue(const LSValue &Val) {
    return hash_value(Val);
  }
  static bool isEqual(const LSValue &LHS, const LSValue &RHS) {
    return LHS == RHS;
  }
};

} // namespace llvm

#endif // SWIFT_SIL_MEMLOCATION_H
