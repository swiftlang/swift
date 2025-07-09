//===--- SILIsolationInfo.h -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_SILISOLATIONINFO_H
#define SWIFT_SILOPTIMIZER_UTILS_SILISOLATIONINFO_H

#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

#include <algorithm>
#include <variant>

namespace swift {

/// Represents a generalized actor instance reference.
///
/// Used to generalize over cases where we actually have an actor instance
/// associated with a SILValue and other cases where we know that the actor
/// instance is but we don't have a value.
class ActorInstance {
public:
  enum class Kind : uint8_t {
    /// An actor instance where we have an actual SILValue for the actor
    /// instance.
    Value,

    /// An actor instance in an actor accessor init where we do not have direct
    /// access to the self value and instead have access indirectly to the
    /// storage associated with the accessor.
    ActorAccessorInit = 0x1,

    /// An actor instance that is represented by "self" being captured in a
    /// closure of some sort. In such a case, we do not know which of the
    /// parameters are the true "self" (since the closure is a thin
    /// function)... so we just use an artificial ActorInstance to represent
    /// self in this case.
    CapturedActorSelf = 0x2,
  };

  /// Set to (SILValue(), $KIND) if we have an ActorAccessorInit|CapturedSelf.
  /// Is null if we have (SILValue(), Kind::Value).
  llvm::PointerIntPair<SILValue, 2> value;

  ActorInstance(SILValue value, Kind kind)
      : value(value, std::underlying_type<Kind>::type(kind)) {}

  /// We want to look through certain instructions like end_init_ref that have
  /// the appropriate actor type but could disguise the actual underlying value
  /// that we want to represent our actor.
  static SILValue lookThroughInsts(SILValue value);

public:
  ActorInstance() : ActorInstance(SILValue(), Kind::Value) {}

  static ActorInstance getForValue(SILValue value) {
    if (!value)
      return ActorInstance();
    value = lookThroughInsts(value);
    if (!value->getType()
             .getASTType()
             ->lookThroughAllOptionalTypes()
             ->isAnyActorType())
      return ActorInstance();
    return ActorInstance(value, Kind::Value);
  }

  /// See Kind::ActorAccessorInit for explanation on what a ActorAccessorInit
  /// is.
  static ActorInstance getForActorAccessorInit() {
    return ActorInstance(SILValue(), Kind::ActorAccessorInit);
  }

  /// See Kind::CapturedActorSelf for explanation on what a CapturedActorSelf
  /// is.
  static ActorInstance getForCapturedSelf() {
    return ActorInstance(SILValue(), Kind::CapturedActorSelf);
  }

  explicit operator bool() const { return bool(value.getOpaqueValue()); }

  Kind getKind() const { return Kind(value.getInt()); }

  SILValue getValue() const {
    assert(getKind() == Kind::Value);
    return value.getPointer();
  }

  LLVM_ATTRIBUTE_USED SILValue maybeGetValue() const {
    if (getKind() != Kind::Value)
      return SILValue();
    return getValue();
  }

  bool isValue() const { return getKind() == Kind::Value; }

  bool isAccessorInit() const { return getKind() == Kind::ActorAccessorInit; }

  bool isCapturedActorSelf() const {
    return getKind() == Kind::CapturedActorSelf;
  }

  bool operator==(const ActorInstance &other) const {
    // If both are null, return true.
    if (!bool(*this) && !bool(other))
      return true;

    // Otherwise, check if the kinds match.
    if (getKind() != other.getKind())
      return false;

    // Now that we know that the kinds match, perform the kind specific check.
    switch (getKind()) {
    case Kind::Value:
      return getValue() == other.getValue();
    case Kind::ActorAccessorInit:
    case Kind::CapturedActorSelf:
      return true;
    }
  }

  bool operator!=(const ActorInstance &other) const {
    return !(*this == other);
  }

  void print(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

/// The isolation info inferred for a specific SILValue. Use
/// SILIsolationInfo::get() to compute these. It is intended to be a
/// conservatively correct model that we expand over time with more pattern
/// matching.
class SILIsolationInfo {
public:
  /// This forms a lattice of semantics. The lattice progresses from left ->
  /// right below:
  ///
  /// Unknown -> Disconnected -> Task -> Actor.
  ///
  enum Kind : uint8_t {
    /// Unknown means no information. We error when merging on it.
    Unknown,

    /// An entity with disconnected isolation can be freely sent into another
    /// isolation domain. These are associated with "use after send"
    /// diagnostics.
    Disconnected,

    /// An entity that is in the same region as a task-isolated value. Cannot be
    /// sent into another isolation domain.
    Task,

    /// An entity that is in the same region as an actor-isolated value. Cannot
    /// be sent into another isolation domain.
    Actor,
  };

  enum class Flag : uint8_t {
    None,

    /// If set, this means that the element that we derived this from was marked
    /// with nonisolated(unsafe).
    UnsafeNonIsolated = 0x1,

    /// If set, this means that this actor isolation is from an isolated
    /// parameter and should be allowed to merge into a self parameter.
    UnappliedIsolatedAnyParameter = 0x2,

    /// If set, this was a TaskIsolated value from a nonisolated(nonsending)
    /// parameter.
    NonisolatedNonsendingTaskIsolated = 0x4,

    /// The maximum number of bits used by a Flag.
    MaxNumBits = 3,
  };

  using Options = OptionSet<Flag>;

private:
  /// The actor isolation if this value has one. The default unspecified case
  /// otherwise.
  ActorIsolation actorIsolation;

  /// This is the value that we got isolation from if we were able to find
  /// one. Used for isolation history.
  SILValue isolatedValue;

  /// If set this is the SILValue that represents the actor instance that we
  /// derived isolatedValue from.
  ActorInstance actorInstance;

  /// When the isolation is introduced due to a (potentially) isolated
  /// conformance, the protocol whose conformance might be isolated.
  ProtocolDecl *isolatedConformance = nullptr;

  unsigned kind : 8;
  unsigned options : 8;

  SILIsolationInfo(SILValue isolatedValue, SILValue actorInstance,
                   ActorIsolation actorIsolation, Options options = Options(),
                   ProtocolDecl *isolatedConformance = nullptr)
      : actorIsolation(actorIsolation), isolatedValue(isolatedValue),
        actorInstance(ActorInstance::getForValue(actorInstance)),
        isolatedConformance(isolatedConformance), kind(Actor),
        options(options.toRaw()) {
    assert((!actorInstance ||
            (actorIsolation.getKind() == ActorIsolation::ActorInstance &&
             actorInstance->getType()
                 .getASTType()
                 ->lookThroughAllOptionalTypes()
                 ->getAnyActor())) &&
           "actorInstance must be an actor if it is non-empty");
  }

  SILIsolationInfo(SILValue isolatedValue, ActorInstance actorInstance,
                   ActorIsolation actorIsolation, Options options = Options(),
                   ProtocolDecl *isolatedConformance = nullptr)
      : actorIsolation(actorIsolation), isolatedValue(isolatedValue),
        actorInstance(actorInstance), isolatedConformance(isolatedConformance),
        kind(Actor), options(options.toRaw())
  {
    assert(actorInstance);
    assert(actorIsolation.getKind() == ActorIsolation::ActorInstance);
  }

  SILIsolationInfo(Kind kind, SILValue isolatedValue,
                   ProtocolDecl *isolatedConformance = nullptr)
      : actorIsolation(), isolatedValue(isolatedValue),
        isolatedConformance(isolatedConformance), kind(kind), options(0) {
  }

  SILIsolationInfo(Kind kind, Options options = Options())
      : actorIsolation(), kind(kind), options(options.toRaw()) {}

public:
  SILIsolationInfo() : actorIsolation(), kind(Kind::Unknown), options(0) {}

  operator bool() const { return kind != Kind::Unknown; }

  operator Kind() const { return getKind(); }

  Kind getKind() const { return Kind(kind); }

  bool isDisconnected() const { return kind == Kind::Disconnected; }
  bool isActorIsolated() const { return kind == Kind::Actor; }
  bool isTaskIsolated() const { return kind == Kind::Task; }

  Options getOptions() const { return Options(options); }

  void setOptions(Options newOptions) { options = newOptions.toRaw(); }

  bool isUnsafeNonIsolated() const {
    return getOptions().contains(Flag::UnsafeNonIsolated);
  }

  // Retrieve the protocol to which there is (or could be) an isolated
  // conformance.
  ProtocolDecl *getIsolatedConformance() const {
    return isolatedConformance;
  }

  SILIsolationInfo withUnsafeNonIsolated(bool newValue = true) const {
    assert(*this && "Cannot be unknown");
    auto self = *this;
    if (newValue) {
      self.options = (self.getOptions() | Flag::UnsafeNonIsolated).toRaw();
    } else {
      self.options =
          self.getOptions().toRaw() & ~Options(Flag::UnsafeNonIsolated).toRaw();
    }
    return self;
  }

  bool isNonisolatedNonsendingTaskIsolated() const {
    return getOptions().contains(Flag::NonisolatedNonsendingTaskIsolated);
  }

  SILIsolationInfo
  withNonisolatedNonsendingTaskIsolated(bool newValue = true) const {
    assert(*this && "Cannot be unknown");
    assert(isTaskIsolated() && "Can only be task isolated");
    auto self = *this;
    if (newValue) {
      self.options =
          (self.getOptions() | Flag::NonisolatedNonsendingTaskIsolated).toRaw();
    } else {
      self.options = self.getOptions().toRaw() &
                     ~Options(Flag::NonisolatedNonsendingTaskIsolated).toRaw();
    }
    return self;
  }

  /// Produce a new isolation info value that merges in the given isolated
  /// conformance value.
  ///
  /// If both isolation infos have an isolation conformance, pick one
  /// arbitrarily. Otherwise, the result has no isolated conformance.
  SILIsolationInfo
  withMergedIsolatedConformance(ProtocolDecl *newIsolatedConformance) const {
    SILIsolationInfo result(*this);
    if (!isolatedConformance || !newIsolatedConformance) {
      result.isolatedConformance = nullptr;
      return result;
    }

    result.isolatedConformance =
      ProtocolDecl::compare(isolatedConformance, newIsolatedConformance) <= 0
        ? isolatedConformance
        : newIsolatedConformance;
    return result;
  }

  /// Returns true if this actor isolation is derived from an unapplied
  /// isolation parameter. When merging, we allow for this to be merged with a
  /// more specific isolation kind.
  bool isUnappliedIsolatedAnyParameter() const {
    return getOptions().contains(Flag::UnappliedIsolatedAnyParameter);
  }

  SILIsolationInfo withUnappliedIsolatedParameter(bool newValue = true) const {
    assert(*this && "Cannot be unknown");
    auto self = *this;
    if (newValue) {
      self.options =
          (self.getOptions() | Flag::UnappliedIsolatedAnyParameter).toRaw();
    } else {
      self.options = self.getOptions().toRaw() &
                     ~Options(Flag::UnappliedIsolatedAnyParameter).toRaw();
    }
    return self;
  }

  void print(SILFunction *fn, llvm::raw_ostream &os) const;

  /// Print a textual representation of the text info that is meant to be
  /// included in other logging output for types that compose with
  /// SILIsolationInfo. As a result, we only print state that can fit on
  /// one line.
  void printForOneLineLogging(SILFunction *fn, llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMPER(dump(SILFunction *fn)) {
    print(fn, llvm::dbgs());
    llvm::dbgs() << '\n';
  }

  /// Prints out the message for a diagnostic that states that the value is
  /// exposed to a specific code.
  ///
  /// We do this programatically since task-isolated code needs a very different
  /// form of diagnostic than other cases.
  void printForCodeDiagnostic(SILFunction *fn, llvm::raw_ostream &os) const;

  /// Overload of printForCodeDiagnostics that returns an interned StringRef
  /// owned by the AST.
  StringRef printForCodeDiagnostic(SILFunction *fn) const;

  void printForDiagnostics(SILFunction *fn, llvm::raw_ostream &os) const;

  /// Overload of printForDiagnostics that returns an interned StringRef owned
  /// by the AST.
  StringRef printForDiagnostics(SILFunction *fn) const;

  SWIFT_DEBUG_DUMPER(dumpForDiagnostics(SILFunction *fn)) {
    printForDiagnostics(fn, llvm::dbgs());
    llvm::dbgs() << '\n';
  }

  ActorIsolation getActorIsolation() const {
    assert(kind == Actor);
    return actorIsolation;
  }

  /// If we are actor or task isolated and could find a specific value that
  /// caused the isolation, put it here. Used for isolation history.
  SILValue getIsolatedValue() const {
    assert(kind == Task || kind == Actor);
    return isolatedValue;
  }

  /// Return the specific SILValue for the actor that our isolated value is
  /// isolated to if one exists.
  ActorInstance getActorInstance() const {
    assert(kind == Actor);
    return actorInstance;
  }

  bool hasActorIsolation() const { return kind == Actor; }

  bool hasIsolatedValue() const {
    return (kind == Task || kind == Actor) && bool(isolatedValue);
  }

  SILValue maybeGetIsolatedValue() const {
    if (!hasIsolatedValue())
      return {};
    return getIsolatedValue();
  }

  static SILIsolationInfo getDisconnected(bool isUnsafeNonIsolated) {
    return {Kind::Disconnected,
            isUnsafeNonIsolated ? Flag::UnsafeNonIsolated : Flag::None};
  }

  /// Create an actor isolation for a value that we know is actor isolated to a
  /// specific actor, but we do not know the specific instance yet.
  ///
  /// This can occur when closing over a closure with an isolated parameter or
  /// if we are determining isolation of a function_ref that takes an isolated
  /// parameter. In both cases, we cannot know what the actual isolation is
  /// until we invoke the closure or function.
  ///
  /// TODO: This is just a stub currently until I implement the flow sensitive
  /// part. We just treat all instances the same. There are tests that validate
  /// this behavior.
  static SILIsolationInfo
  getFlowSensitiveActorIsolated(SILValue isolatedValue,
                                ActorIsolation actorIsolation) {
    return {isolatedValue, SILValue(), actorIsolation,
            Flag::UnappliedIsolatedAnyParameter};
  }

  static SILIsolationInfo getActorInstanceIsolated(SILValue isolatedValue,
                                                   SILValue actorInstance,
                                                   NominalTypeDecl *typeDecl) {
    assert(actorInstance);
    if (!typeDecl->isAnyActor()) {
      assert(!swift::getActorIsolation(typeDecl).isGlobalActor() &&
             "Should have called getGlobalActorIsolated");
      return {};
    }
    return {isolatedValue, actorInstance,
            ActorIsolation::forActorInstanceSelf(typeDecl)};
  }

  static SILIsolationInfo
  getActorInstanceIsolated(SILValue isolatedValue,
                           const SILFunctionArgument *actorInstance) {
    assert(actorInstance);
    auto *varDecl =
        llvm::dyn_cast_if_present<VarDecl>(actorInstance->getDecl());
    if (!varDecl)
      return {};
    return {isolatedValue, actorInstance,
            actorInstance->isSelf()
                ? ActorIsolation::forActorInstanceSelf(varDecl)
                : ActorIsolation::forActorInstanceParameter(
                      varDecl, actorInstance->getIndex())};
  }

  static SILIsolationInfo getActorInstanceIsolated(SILValue isolatedValue,
                                                   ActorInstance actorInstance,
                                                   NominalTypeDecl *typeDecl) {
    assert(actorInstance);
    if (!typeDecl->isAnyActor()) {
      assert(!swift::getActorIsolation(typeDecl).isGlobalActor() &&
             "Should have called getGlobalActorIsolated");
      return {};
    }
    return {isolatedValue, actorInstance,
            ActorIsolation::forActorInstanceSelf(typeDecl)};
  }

  /// A special actor instance isolated for partial apply cases where we do not
  /// close over the isolated parameter and thus do not know the actual actor
  /// instance that we are going to use.
  static SILIsolationInfo
  getPartialApplyActorInstanceIsolated(SILValue isolatedValue,
                                       NominalTypeDecl *typeDecl) {
    if (!typeDecl->isAnyActor()) {
      assert(!swift::getActorIsolation(typeDecl).isGlobalActor() &&
             "Should have called getGlobalActorIsolated");
      return {};
    }
    return {isolatedValue, SILValue(),
            ActorIsolation::forActorInstanceSelf(typeDecl),
            Flag::UnappliedIsolatedAnyParameter};
  }

  static SILIsolationInfo getGlobalActorIsolated(
      SILValue value,
      Type globalActorType,
      ProtocolDecl *isolatedConformance = nullptr) {
    return {value, SILValue() /*no actor instance*/,
            ActorIsolation::forGlobalActor(globalActorType),
            Options(), isolatedConformance};
  }

  static SILIsolationInfo getGlobalActorIsolated(SILValue value,
                                                 ValueDecl *decl) {
    auto isolation = swift::getActorIsolation(decl);
    if (!isolation.isGlobalActor())
      return {};
    return SILIsolationInfo::getGlobalActorIsolated(value,
                                                    isolation.getGlobalActor());
  }

  static SILIsolationInfo getTaskIsolated(
      SILValue value, ProtocolDecl *isolatedConformance = nullptr) {
    return {Kind::Task, value, isolatedConformance};
  }

  /// Attempt to infer the isolation region info for \p inst.
  static SILIsolationInfo get(SILInstruction *inst);

  /// Attempt to infer the isolation region info for \p arg.
  static SILIsolationInfo get(SILArgument *arg);

  static SILIsolationInfo get(SILValue value) {
    if (auto *arg = dyn_cast<SILArgument>(value))
      return get(arg);
    if (auto *inst = dyn_cast<SingleValueInstruction>(value))
      return get(inst);
    return {};
  }

  /// A helper that is used to ensure that we treat certain builtin values as
  /// non-Sendable that the AST level otherwise thinks are non-Sendable.
  ///
  /// E.x.: Builtin.RawPointer and Builtin.NativeObject
  ///
  /// TODO: Fix the type checker.
  static bool isNonSendableType(SILType type, SILFunction *fn);

  static bool isSendableType(SILType type, SILFunction *fn) {
    return !isNonSendableType(type, fn);
  }

  static bool isNonSendableType(SILValue value) {
    return isNonSendableType(value->getType(), value->getFunction());
  }

  static bool isSendableType(SILValue value) {
    return !isNonSendableType(value);
  }

  bool hasSameIsolation(ActorIsolation actorIsolation) const;

  /// Returns true if \p this and \p other have the same isolation. It allows
  /// for the isolated values if any to not match.
  ///
  /// This is useful if one has two non-Sendable values projected from the same
  /// actor or global-actor-isolated value. E.x.: two different ref_element_addr
  /// from the same actor.
  bool hasSameIsolation(const SILIsolationInfo &other) const;

  /// Returns true if this SILIsolationInfo is deeply equal to other. This means
  /// that the isolation and the isolated value match.
  bool isEqual(const SILIsolationInfo &other) const;

  /// A helper function that prints ActorIsolation like we normally do except
  /// that it prints nonisolated(nonsending) as nonisolated. This is needed in
  /// certain cases when talking about use-after-free uses in send non sendable.
  static void printActorIsolationForDiagnostics(
      SILFunction *fn, ActorIsolation iso, llvm::raw_ostream &os,
      StringRef openingQuotationMark = "'", bool asNoun = false);

  /// Overload for printActorIsolationForDiagnostics that produces a StringRef.
  static StringRef
  printActorIsolationForDiagnostics(SILFunction *fn, ActorIsolation iso,
                                    StringRef openingQuotationMark = "'",
                                    bool asNoun = false);

  void Profile(llvm::FoldingSetNodeID &id) const;

private:
  void printOptions(llvm::raw_ostream &os) const;

  /// This is used only to let through apply isolation crossings that we define
  /// in SIL just for testing. Do not use this in any other contexts!
  static SILIsolationInfo
  getWithIsolationCrossing(ApplyIsolationCrossing crossing) {
    if (!crossing.getCalleeIsolation().isActorIsolated())
      return {};

    // SIL level, just let it through without an actor instance. We assume since
    // we are using this for SIL tests that we do not need to worry about having
    // a null actor instance.
    return SILIsolationInfo(SILValue(), SILValue(),
                            crossing.getCalleeIsolation());
  }
};

/// A SILIsolationInfo that has gone through merging and represents the dynamic
/// isolation info of a value found by merging its isolation at a region
/// point. This means that nonisolated(unsafe) has been removed. It is used so
/// that in the type system we can distinguish in between isolation info that is
/// static isolation info associated with a value and dynamic isolation info
/// that can just be used for dataflow.
class SILDynamicMergedIsolationInfo {
  SILIsolationInfo innerInfo;

public:
  SILDynamicMergedIsolationInfo() : innerInfo() {}
  SILDynamicMergedIsolationInfo(SILIsolationInfo innerInfo)
      : innerInfo(innerInfo) {}

  /// Returns nullptr only if both this isolation info and \p other are actor
  /// isolated to incompatible actors.
  [[nodiscard]] std::optional<SILDynamicMergedIsolationInfo>
  merge(SILIsolationInfo other) const;

  [[nodiscard]] std::optional<SILDynamicMergedIsolationInfo>
  merge(SILDynamicMergedIsolationInfo other) const {
    return merge(other.getIsolationInfo());
  }

  operator bool() const { return bool(innerInfo); }

  SILIsolationInfo *operator->() { return &innerInfo; }
  const SILIsolationInfo *operator->() const { return &innerInfo; }

  SILIsolationInfo getIsolationInfo() const { return innerInfo; }

  bool isDisconnected() const { return innerInfo.isDisconnected(); }

  bool hasSameIsolation(SILIsolationInfo other) const {
    return innerInfo.hasSameIsolation(other);
  }

  static SILDynamicMergedIsolationInfo
  getDisconnected(bool isUnsafeNonIsolated) {
    return SILDynamicMergedIsolationInfo(
        SILIsolationInfo::getDisconnected(isUnsafeNonIsolated));
  }

  SWIFT_DEBUG_DUMPER(dump(SILFunction *fn)) { innerInfo.dump(fn); }

  void printForDiagnostics(SILFunction *fn, llvm::raw_ostream &os) const {
    innerInfo.printForDiagnostics(fn, os);
  }

  StringRef printForDiagnostics(SILFunction *fn) const {
    return innerInfo.printForDiagnostics(fn);
  }

  SWIFT_DEBUG_DUMPER(dumpForDiagnostics(SILFunction *fn)) {
    innerInfo.dumpForDiagnostics(fn);
  }

  void printForCodeDiagnostic(SILFunction *fn, llvm::raw_ostream &os) const {
    innerInfo.printForCodeDiagnostic(fn, os);
  }

  StringRef printForCodeDiagnostic(SILFunction *fn) const {
    return innerInfo.printForCodeDiagnostic(fn);
  }

  void printForOneLineLogging(SILFunction *fn, llvm::raw_ostream &os) const {
    innerInfo.printForOneLineLogging(fn, os);
  }
};

} // namespace swift

#endif
