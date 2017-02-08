//===--- LValue.h - Logical LValue Representation ---------------*- C++ -*-===//
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
// A storage structure for keeping track of logical lvalues during SILGen.
//
// In general, only the routines in SILGenLValue.cpp should actually be
// accessing LValues and their components.  Everything else should just
// pass them around opaquely.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_LVALUE_H
#define SWIFT_LOWERING_LVALUE_H

#include "SILGenFunction.h"

namespace swift {
namespace Lowering {
  class SILGenFunction;
  class ManagedValue;

class PhysicalPathComponent;
class LogicalPathComponent;
class TranslationPathComponent;

/// Information about the type of an l-value.
struct LValueTypeData {
  /// The abstraction pattern of the l-value.
  ///
  /// The type-of-rvalue should always be the substituted formal type
  /// lowered under this abstraction pattern.
  AbstractionPattern OrigFormalType = AbstractionPattern::getInvalid();

  /// The substituted formal object type of the l-value.
  ///
  /// Tn the most common case, this is the type of an l-value
  /// expression as recorded in the AST, only with the
  /// LValueType/InOutType stripped off.
  CanType SubstFormalType;

  /// The lowered type of value that should be stored in the l-value.
  /// Always an object type.
  ///
  /// On physical path components, projection yields an address of
  /// this type.  On logical path components, materialize yields an
  /// address of this type, set expects a value of this type, and
  /// get yields a value of this type.
  SILType TypeOfRValue;

  LValueTypeData() = default;
  LValueTypeData(AbstractionPattern origFormalType, CanType substFormalType,
                 SILType typeOfRValue)
    : OrigFormalType(origFormalType), SubstFormalType(substFormalType),
      TypeOfRValue(typeOfRValue) {
    assert(typeOfRValue.isObject());
    assert(substFormalType->isMaterializable());
  }
};

/// An l-value path component represents a chunk of the access path to
/// an object.  Path components may be either "physical" or "logical".
/// A physical path involves elementary address manipulations; these
/// address manipulations may be in some way dynamic, but they are
/// ultimately just pointer arithmetic.  A logical path requires
/// getter/setter logic.
///
/// This divide between physical/logical is closely related to the
/// fragile/resilient split, with two primary differences:
///   - Any sort of implementation can be fragile.  For example, a
///     computed variable can still be fragile, meaning that it is known
///     to be implemented with a getter/setter.  The known
///     implementation must be a direct offset in order to qualify as
///     physical.
///   - A path component's implementation can be resilient and yet
///     still qualify for physical access if we are in a privileged
///     component.
class PathComponent {
  LValueTypeData TypeData;

  friend class LValue;
  unsigned AllocatedSize;
public:
  enum KindTy {
    // Physical lvalue kinds
    RefElementKind,             // ref_element_addr
    TupleElementKind,           // tuple_element_addr
    StructElementKind,          // struct_element_addr
    OptionalObjectKind,         // optional projection
    OpenedExistentialKind,      // opened opaque existential
    AddressorKind,              // var/subscript addressor
    ValueKind,                  // random base pointer as an lvalue

    // Logical LValue kinds
    GetterSetterKind,           // property or subscript getter/setter
    OwnershipKind,              // weak pointer remapping
    AutoreleasingWritebackKind, // autorelease pointer on set
    WritebackPseudoKind,        // a fake component to customize writeback
    // Translation LValue kinds (a subtype of logical)
    OrigToSubstKind,            // generic type substitution
    SubstToOrigKind,            // generic type substitution

    FirstLogicalKind = GetterSetterKind,
    FirstTranslationKind = OrigToSubstKind,
  };
private:
  const KindTy Kind : 8;

  // This anchor method serves three purposes: it aligns the class to
  // a pointer boundary, it makes the class a primary base so that
  // subclasses will be at offset zero, and it anchors the v-table
  // to a specific file.
  virtual void _anchor();

  PathComponent(const PathComponent &) = delete;
  PathComponent &operator=(const PathComponent &) = delete;

protected:
  PathComponent(LValueTypeData typeData, KindTy Kind)
    : TypeData(typeData), Kind(Kind) {}
public:
  virtual ~PathComponent() {}

  /// Returns sizeof(the final type), plus any extra storage required.
  size_t allocated_size() const { return AllocatedSize; }

  /// Is this component physical or logical?  If physical, this will
  /// be a subclass of PhysicalPathComponent.  If logical, this will
  /// be a subclass of LogicalPathComponent.
  bool isPhysical() const { return Kind < FirstLogicalKind; }
  bool isLogical() const { return Kind >= FirstLogicalKind; }
  bool isTranslation() const { return Kind >= FirstTranslationKind; }

  // These are implemented inline after the respective class declarations.

  PhysicalPathComponent &asPhysical();
  const PhysicalPathComponent &asPhysical() const;

  LogicalPathComponent &asLogical();
  const LogicalPathComponent &asLogical() const;

  TranslationPathComponent &asTranslation();
  const TranslationPathComponent &asTranslation() const;

  /// Return the appropriate access kind to use when producing the
  /// base value.
  virtual AccessKind getBaseAccessKind(SILGenFunction &SGF,
                                       AccessKind accessKind) const = 0;
  
  /// Returns the logical type-as-rvalue of the value addressed by the
  /// component.  This is always an object type, never an address.
  SILType getTypeOfRValue() const { return TypeData.TypeOfRValue; }
  AbstractionPattern getOrigFormalType() const {
    return TypeData.OrigFormalType;
  }
  CanType getSubstFormalType() const { return TypeData.SubstFormalType; }

  const LValueTypeData &getTypeData() const { return TypeData; }

  KindTy getKind() const { return Kind; }

  void dump() const;
  virtual void print(raw_ostream &OS) const = 0;
};

/// An abstract class for "physical" path components, i.e. path
/// components that can be accessed as address manipulations.  See the
/// comment for PathComponent for more information.
class PhysicalPathComponent : public PathComponent {
  virtual void _anchor() override;

protected:
  PhysicalPathComponent(LValueTypeData typeData, KindTy Kind)
    : PathComponent(typeData, Kind) {
    assert(isPhysical() && "PhysicalPathComponent Kind isn't physical");
  }

public:
  /// Derive the address of this component given the address of the base.
  ///
  /// \param base - always an address, but possibly an r-value
  virtual ManagedValue offset(SILGenFunction &gen,
                              SILLocation loc,
                              ManagedValue base,
                              AccessKind accessKind) && = 0;

  AccessKind getBaseAccessKind(SILGenFunction &gen,
                               AccessKind accessKind) const override {
    return accessKind;
  }
};

inline PhysicalPathComponent &PathComponent::asPhysical() {
  assert(isPhysical());
  return static_cast<PhysicalPathComponent&>(*this);
}
inline const PhysicalPathComponent &PathComponent::asPhysical() const {
  assert(isPhysical());
  return static_cast<const PhysicalPathComponent&>(*this);
}

/// An abstract class for "logical" path components, i.e. path
/// components that require getter/setter methods to access.  See the
/// comment for PathComponent for more information.
class LogicalPathComponent : public PathComponent {
  virtual void _anchor();

protected:
  LogicalPathComponent(LValueTypeData typeData, KindTy Kind)
    : PathComponent(typeData, Kind) {
    assert(isLogical() && "LogicalPathComponent Kind isn't logical");
  }

public:
  /// Clone the path component onto the heap.
  virtual std::unique_ptr<LogicalPathComponent>
  clone(SILGenFunction &gen, SILLocation l) const = 0;
  
  /// Set the property.
  ///
  /// \param base - always an address, but possibly an r-value
  virtual void set(SILGenFunction &gen, SILLocation loc,
                   RValue &&value, ManagedValue base) && = 0;

  /// Get the property.
  ///
  /// \param base - always an address, but possibly an r-value
  virtual RValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) && = 0;

  /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
  /// the same dynamic PathComponent type as the receiver) to see if they are
  /// identical.  If so, there is a conflicting writeback happening, so emit a
  /// diagnostic.
  virtual void diagnoseWritebackConflict(LogicalPathComponent *rhs,
                                         SILLocation loc1, SILLocation loc2,
                                         SILGenFunction &gen) = 0;


  /// Materialize the storage into memory.  If the access is for
  /// mutation, ensure that modifications to the memory will
  /// eventually be reflected in the original storage.
  ///
  /// \param base - always an address, but possibly an r-value
  virtual ManagedValue getMaterialized(SILGenFunction &gen, SILLocation loc,
                                       ManagedValue base,
                                       AccessKind accessKind) &&;

  /// Perform a writeback on the property.
  ///
  /// \param base - always an address, but possibly an r-value
  virtual void writeback(SILGenFunction &gen, SILLocation loc,
                         ManagedValue base,
                         MaterializedLValue materialized,
                         bool isFinal);
};

inline LogicalPathComponent &PathComponent::asLogical() {
  assert(isLogical());
  return static_cast<LogicalPathComponent&>(*this);
}
inline const LogicalPathComponent &PathComponent::asLogical() const {
  assert(isLogical());
  return static_cast<const LogicalPathComponent&>(*this);
}

/// An abstract class for components which translate values in some way.
class TranslationPathComponent : public LogicalPathComponent {
protected:
  TranslationPathComponent(LValueTypeData typeData, KindTy kind)
      : LogicalPathComponent(typeData, kind) {
    assert(isTranslation() &&
           "TranslationPathComponent kind isn't value translation");
  }

public:
  AccessKind getBaseAccessKind(SILGenFunction &gen,
                               AccessKind kind) const override {
    // Always use the same access kind for the base.
    return kind;
  }

  void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                 SILLocation loc1, SILLocation loc2,
                                 SILGenFunction &gen) override {
    // no useful writeback diagnostics at this point
  }

  RValue get(SILGenFunction &gen, SILLocation loc,
             ManagedValue base, SGFContext c) && override;

  void set(SILGenFunction &gen, SILLocation loc,
           RValue &&value, ManagedValue base) && override;

  /// Transform from the original pattern.
  virtual RValue translate(SILGenFunction &gen, SILLocation loc,
                           RValue &&value,
                           SGFContext ctx = SGFContext()) && = 0;

  /// Transform into the original pattern.
  virtual RValue untranslate(SILGenFunction &gen, SILLocation loc,
                             RValue &&value,
                             SGFContext ctx = SGFContext()) && = 0;
  
};

inline TranslationPathComponent &PathComponent::asTranslation() {
  assert(isTranslation());
  return static_cast<TranslationPathComponent&>(*this);
}
inline const TranslationPathComponent &PathComponent::asTranslation() const {
  assert(isTranslation());
  return static_cast<const TranslationPathComponent&>(*this);
}

/// An lvalue represents a reference to storage holding a value
/// of a type, as opposed to an rvalue, which is an actual value
/// of the type.
class LValue {
  std::vector<std::unique_ptr<PathComponent>> Path;

public:
  LValue() = default;
  LValue(const LValue &other) = delete;
  LValue(LValue &&other) = default;

  LValue &operator=(const LValue &) = delete;
  LValue &operator=(LValue &&) = default;

  static LValue forValue(ManagedValue value,
                         CanType substFormalType);

  static LValue forAddress(ManagedValue address,
                           AbstractionPattern origFormalType,
                           CanType substFormalType);

  bool isValid() const { return !Path.empty(); }

  /// Is this lvalue purely physical?
  bool isPhysical() const {
    assert(isValid());
    for (auto &component : Path)
      if (!component->isPhysical())
        return false;
    return true;
  }
  
  /// Is the lvalue's final component physical?
  bool isLastComponentPhysical() const {
    assert(isValid());
    return Path.back()->isPhysical();
  }

  /// Is the lvalue's final component a translation component?
  bool isLastComponentTranslation() const {
    assert(isValid());
    return Path.back()->isTranslation();
  }

  /// Given that the last component is a translation component,
  /// return it.
  TranslationPathComponent &getLastTranslationComponent() & {
    assert(isLastComponentTranslation());
    return Path.back()->asTranslation();
  }

  /// Given that the last component is a translation component,
  /// peel it off.
  void dropLastTranslationComponent() & {
    assert(isLastComponentTranslation());
    Path.pop_back();
  }
  
  /// Add a new component at the end of the access path of this lvalue.
  template <class T, class... As>
  void add(As &&... args) {
    Path.emplace_back(new T(std::forward<As>(args)...));
  }

  /// Add a member component to the access path of this lvalue.
  void addMemberComponent(SILGenFunction &gen, SILLocation loc,
                          AbstractStorageDecl *storage,
                          SubstitutionList subs,
                          bool isSuper,
                          AccessKind accessKind,
                          AccessSemantics accessSemantics,
                          AccessStrategy accessStrategy,
                          CanType formalRValueType,
                          RValue &&indices);

  void addMemberVarComponent(SILGenFunction &gen, SILLocation loc,
                             VarDecl *var,
                             SubstitutionList subs,
                             bool isSuper,
                             AccessKind accessKind,
                             AccessSemantics accessSemantics,
                             AccessStrategy accessStrategy,
                             CanType formalRValueType);

  void addMemberSubscriptComponent(SILGenFunction &gen, SILLocation loc,
                                   SubscriptDecl *subscript,
                                   SubstitutionList subs,
                                   bool isSuper,
                                   AccessKind accessKind,
                                   AccessSemantics accessSemantics,
                                   AccessStrategy accessStrategy,
                                   CanType formalRValueType,
                                   RValue &&indices,
                                   Expr *indexExprForDiagnostics = nullptr);

  /// Add a subst-to-orig reabstraction component.  That is, given
  /// that this l-value trafficks in values following the substituted
  /// abstraction pattern, make an l-value trafficking in values
  /// following the original abstraction pattern.
  void addSubstToOrigComponent(AbstractionPattern origType,
                               SILType loweredResultType);

  /// Add an orig-to-subst reabstraction component.  That is, given
  /// that this l-value trafficks in values following the original
  /// abstraction pattern, make an l-value trafficking in values
  /// following the substituted abstraction pattern.
  void addOrigToSubstComponent(SILType loweredResultType);

  typedef std::vector<std::unique_ptr<PathComponent>>::iterator iterator;
  typedef std::vector<std::unique_ptr<PathComponent>>::const_iterator
    const_iterator;

  iterator begin() { return Path.begin(); }
  iterator end() { return Path.end(); }
  const_iterator begin() const { return Path.begin(); }
  const_iterator end() const { return Path.end(); }

  const LValueTypeData &getTypeData() const {
    return Path.back()->getTypeData();
  }

  /// Returns the type-of-rvalue of the logical object referenced by
  /// this l-value.  Note that this may differ significantly from the
  /// type of l-value.
  SILType getTypeOfRValue() const { return getTypeData().TypeOfRValue; }
  CanType getSubstFormalType() const { return getTypeData().SubstFormalType; }
  AbstractionPattern getOrigFormalType() const {
    return getTypeData().OrigFormalType;
  }

  void dump() const;
  void print(raw_ostream &OS) const;
};
  
/// RAII object to enable writebacks for logical lvalues evaluated within the
/// scope, which will be applied when the object goes out of scope.
///
/// A writeback scope is used to limit the extent of a formal access
/// to an l-value, under the rules specified in the accessors
/// proposal.  It should be entered at a point where it will conclude
/// at the appropriate instant.
///
/// For example, the rules specify that a formal access for an inout
/// argument begins immediately before the call and ends immediately
/// after it.  This can be implemented by pushing a WritebackScope
/// before the formal evaluation of the arguments and popping it
/// immediately after the call.  (It must be pushed before the formal
/// evaluation because, in some cases, the formal evaluation of a base
/// l-value will immediately begin a formal access that must end at
/// the same time as that of its projected subobject l-value.)
class WritebackScope {
  SILGenFunction *gen;
  bool wasInWritebackScope;
  size_t savedDepth;
  void popImpl();
public:
  WritebackScope(SILGenFunction &gen);
  ~WritebackScope() {
    if (gen) {
      popImpl();
    }
  }

  bool isPopped() const {
    return (gen == nullptr);
  }

  void pop() {
    assert(!isPopped() && "popping an already-popped writeback scope!");
    popImpl();
    gen = nullptr;
  }
  
  WritebackScope(const WritebackScope &) = delete;
  WritebackScope &operator=(const WritebackScope &) = delete;
  
  WritebackScope(WritebackScope &&o);
  WritebackScope &operator=(WritebackScope &&o);
};
  
/// RAII object used to enter an inout conversion scope. Writeback scopes formed
/// during the inout conversion scope will be no-ops.
class InOutConversionScope {
  SILGenFunction &gen;
public:
  InOutConversionScope(SILGenFunction &gen);
  ~InOutConversionScope();
};

} // end namespace Lowering
} // end namespace swift

#endif
