//===--- CSFix.h - Constraint Fixes ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides necessary abstractions for constraint fixes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CSFIX_H
#define SWIFT_SEMA_CSFIX_H

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"
#include <string>

namespace llvm {
class raw_ostream;
}

namespace swift {

class SourceManager;

namespace constraints {

class OverloadChoice;
class ConstraintSystem;
class ConstraintLocator;
class Solution;

/// Describes the kind of fix to apply to the given constraint before
/// visiting it.
enum class FixKind : uint8_t {
  /// Introduce a '!' to force an optional unwrap.
  ForceOptional,

  /// Unwrap an optional base when we have a member access.
  UnwrapOptionalBase,
  UnwrapOptionalBaseWithOptionalResult,

  /// Append 'as! T' to force a downcast to the specified type.
  ForceDowncast,

  /// Introduce a '&' to take the address of an lvalue.
  AddressOf,

  /// Replace a coercion ('as') with a forced checked cast ('as!').
  CoerceToCheckedCast,

  /// Mark function type as explicitly '@escaping'.
  ExplicitlyEscaping,

  /// Arguments have labeling failures - missing/extraneous or incorrect
  /// labels attached to the, fix it by suggesting proper labels.
  RelabelArguments,

  /// Treat rvalue as lvalue
  TreatRValueAsLValue,

  /// Add a new conformance to the type to satisfy a requirement.
  AddConformance,

  /// Skip same-type generic requirement constraint,
  /// and assume that types are equal.
  SkipSameTypeRequirement,

  /// Skip superclass generic requirement constraint,
  /// and assume that types are related.
  SkipSuperclassRequirement,

  /// Fix up one of the sides of conversion to make it seem
  /// like the types are aligned.
  ContextualMismatch,

  /// Fix up @autoclosure argument to the @autoclosure parameter,
  /// to for a call to be able to foward it properly, since
  /// @autoclosure conversions are unsupported starting from
  /// Swift version 5.
  AutoClosureForwarding,

  /// Remove `!` or `?` because base is not an optional type.
  RemoveUnwrap,

  /// Add explicit `()` at the end of function or member to call it.
  InsertCall,

  /// Instead of spelling out `subscript` directly, use subscript operator.
  UseSubscriptOperator,

  /// Requested name is not associated with a give base type,
  /// fix this issue by pretending that member exists and matches
  /// given arguments/result types exactly.
  DefineMemberBasedOnUse,

  /// Allow access to type member on instance or instance member on type
  AllowTypeOrInstanceMember,

  /// Allow expressions where 'mutating' method is only partially applied,
  /// which means either not applied at all e.g. `Foo.bar` or only `Self`
  /// is applied e.g. `foo.bar` or `Foo.bar(&foo)`.
  ///
  /// Allow expressions where initializer call (either `self.init` or
  /// `super.init`) is only partially applied.
  AllowInvalidPartialApplication,

  /// Non-required constructors may not be not inherited. Therefore when
  /// constructing a class object, either the metatype must be statically
  /// derived (rather than an arbitrary value of metatype type) or the
  /// referenced constructor must be required.
  AllowInvalidInitRef,

  /// If there are fewer arguments than parameters, let's fix that up
  /// by adding new arguments to the list represented as type variables.
  AddMissingArguments,

  /// Allow single tuple closure parameter destructuring into N arguments.
  AllowClosureParameterDestructuring,

  /// If there is out-of-order argument, let's fix that by re-ordering.
  MoveOutOfOrderArgument,

  /// If there is a matching inaccessible member - allow it as if there
  /// no access control.
  AllowInaccessibleMember,

  /// Allow KeyPaths to use AnyObject as root type
  AllowAnyObjectKeyPathRoot,

  /// Using subscript references in the keypath requires that each
  /// of the index arguments to be Hashable.
  TreatKeyPathSubscriptIndexAsHashable,

  /// Allow a reference to a static member as a key path component.
  AllowStaticMemberRefInKeyPath,
};

class ConstraintFix {
  ConstraintSystem &CS;
  FixKind Kind;
  ConstraintLocator *Locator;

  /// Determines whether this fix is simplify a warning which doesn't
  /// require immediate source changes.
  bool IsWarning;

public:
  ConstraintFix(ConstraintSystem &cs, FixKind kind, ConstraintLocator *locator,
                bool warning = false)
      : CS(cs), Kind(kind), Locator(locator), IsWarning(warning) {}

  virtual ~ConstraintFix();

  FixKind getKind() const { return Kind; }

  bool isWarning() const { return IsWarning; }

  virtual std::string getName() const = 0;

  /// Diagnose a failure associated with this fix given
  /// root expression and information from constraint system.
  virtual bool diagnose(Expr *root, bool asNote = false) const = 0;

  void print(llvm::raw_ostream &Out) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const
                              LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  /// Retrieve anchor expression associated with this fix.
  /// NOTE: such anchor comes directly from locator without
  /// any simplication attempts.
  Expr *getAnchor() const;
  ConstraintLocator *getLocator() const { return Locator; }

protected:
  ConstraintSystem &getConstraintSystem() const { return CS; }
};

/// Append 'as! T' to force a downcast to the specified type.
class ForceDowncast final : public ConstraintFix {
  Type DowncastTo;

  ForceDowncast(ConstraintSystem &cs, Type toType, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::ForceDowncast, locator), DowncastTo(toType) {
  }

public:
  std::string getName() const override;
  bool diagnose(Expr *root, bool asNote = false) const override;

  static ForceDowncast *create(ConstraintSystem &cs, Type toType,
                               ConstraintLocator *locator);
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
  Type BaseType;
  Type UnwrappedType;

  ForceOptional(ConstraintSystem &cs, Type baseType, Type unwrappedType,
                ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::ForceOptional, locator), BaseType(baseType),
        UnwrappedType(unwrappedType) {
    assert(baseType && "Base type must not be null");
    assert(unwrappedType && "Unwrapped type must not be null");
  }

public:
  std::string getName() const override { return "force optional"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static ForceOptional *create(ConstraintSystem &cs, Type baseType,
                               Type unwrappedType, ConstraintLocator *locator);
};

/// Unwrap an optional base when we have a member access.
class UnwrapOptionalBase final : public ConstraintFix {
  DeclName MemberName;

  UnwrapOptionalBase(ConstraintSystem &cs, FixKind kind, DeclName member,
                     ConstraintLocator *locator)
      : ConstraintFix(cs, kind, locator), MemberName(member) {
    assert(kind == FixKind::UnwrapOptionalBase ||
           kind == FixKind::UnwrapOptionalBaseWithOptionalResult);
  }

public:
  std::string getName() const override {
    return "unwrap optional base of member lookup";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static UnwrapOptionalBase *create(ConstraintSystem &cs, DeclName member,
                                    ConstraintLocator *locator);

  static UnwrapOptionalBase *
  createWithOptionalResult(ConstraintSystem &cs, DeclName member,
                           ConstraintLocator *locator);
};

/// Introduce a '&' to take the address of an lvalue.
class AddAddressOf final : public ConstraintFix {
  AddAddressOf(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AddressOf, locator) {}

public:
  std::string getName() const override { return "add address-of"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AddAddressOf *create(ConstraintSystem &cs, ConstraintLocator *locator);
};

// Treat rvalue as if it was an lvalue
class TreatRValueAsLValue final : public ConstraintFix {
  TreatRValueAsLValue(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::TreatRValueAsLValue, locator) {}

public:
  std::string getName() const override { return "treat rvalue as lvalue"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static TreatRValueAsLValue *create(ConstraintSystem &cs,
                                     ConstraintLocator *locator);
};


/// Replace a coercion ('as') with a forced checked cast ('as!').
class CoerceToCheckedCast final : public ConstraintFix {
  CoerceToCheckedCast(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::CoerceToCheckedCast, locator) {}

public:
  std::string getName() const override { return "as to as!"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static CoerceToCheckedCast *create(ConstraintSystem &cs,
                                     ConstraintLocator *locator);
};

/// Mark function type as explicitly '@escaping'.
class MarkExplicitlyEscaping final : public ConstraintFix {
  /// Sometimes function type has to be marked as '@escaping'
  /// to be converted to some other generic type.
  Type ConvertTo;

  MarkExplicitlyEscaping(ConstraintSystem &cs, ConstraintLocator *locator,
                         Type convertingTo = Type())
      : ConstraintFix(cs, FixKind::ExplicitlyEscaping, locator),
        ConvertTo(convertingTo) {}

public:
  std::string getName() const override { return "add @escaping"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static MarkExplicitlyEscaping *create(ConstraintSystem &cs,
                                        ConstraintLocator *locator,
                                        Type convertingTo = Type());
};

/// Arguments have labeling failures - missing/extraneous or incorrect
/// labels attached to the, fix it by suggesting proper labels.
class RelabelArguments final
    : public ConstraintFix,
      private llvm::TrailingObjects<RelabelArguments, Identifier> {
  friend TrailingObjects;

  unsigned NumLabels;

  RelabelArguments(ConstraintSystem &cs,
                   llvm::ArrayRef<Identifier> correctLabels,
                   ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::RelabelArguments, locator),
        NumLabels(correctLabels.size()) {
    std::uninitialized_copy(correctLabels.begin(), correctLabels.end(),
                            getLabelsBuffer().begin());
  }

public:
  std::string getName() const override { return "re-label argument(s)"; }

  ArrayRef<Identifier> getLabels() const {
    return {getTrailingObjects<Identifier>(), NumLabels};
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static RelabelArguments *create(ConstraintSystem &cs,
                                  llvm::ArrayRef<Identifier> correctLabels,
                                  ConstraintLocator *locator);

private:
  MutableArrayRef<Identifier> getLabelsBuffer() {
    return {getTrailingObjects<Identifier>(), NumLabels};
  }
};

/// Add a new conformance to the type to satisfy a requirement.
class MissingConformance final : public ConstraintFix {
  Type NonConformingType;
  ProtocolDecl *Protocol;

  MissingConformance(ConstraintSystem &cs, Type type, ProtocolDecl *protocol,
                     ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AddConformance, locator),
        NonConformingType(type), Protocol(protocol) {}

public:
  std::string getName() const override {
    return "add missing protocol conformance";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static MissingConformance *create(ConstraintSystem &cs, Type type,
                                    ProtocolDecl *protocol,
                                    ConstraintLocator *locator);

  Type getNonConformingType() { return NonConformingType; }

  ProtocolDecl *getProtocol() { return Protocol; }
};

/// Skip same-type generic requirement constraint,
/// and assume that types are equal.
class SkipSameTypeRequirement final : public ConstraintFix {
  Type LHS, RHS;

  SkipSameTypeRequirement(ConstraintSystem &cs, Type lhs, Type rhs,
                          ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::SkipSameTypeRequirement, locator), LHS(lhs),
        RHS(rhs) {}

public:
  std::string getName() const override {
    return "skip same-type generic requirement";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  Type lhsType() { return LHS; }
  Type rhsType() { return RHS; }

  static SkipSameTypeRequirement *create(ConstraintSystem &cs, Type lhs,
                                         Type rhs, ConstraintLocator *locator);
};

/// Skip 'superclass' generic requirement constraint,
/// and assume that types are equal.
class SkipSuperclassRequirement final : public ConstraintFix {
  Type LHS, RHS;

  SkipSuperclassRequirement(ConstraintSystem &cs, Type lhs, Type rhs,
                            ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::SkipSuperclassRequirement, locator),
        LHS(lhs), RHS(rhs) {}

public:
  std::string getName() const override {
    return "skip superclass generic requirement";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  Type subclassType() { return LHS; }
  Type superclassType() { return RHS; }

  static SkipSuperclassRequirement *
  create(ConstraintSystem &cs, Type lhs, Type rhs, ConstraintLocator *locator);
};

/// For example: Sometimes type returned from the body of the
/// closure doesn't match expected contextual type:
///
/// func foo(_: () -> Int) {}
/// foo { "ultimate question" }
///
/// Body of the closure produces `String` type when `Int` is expected
/// by the context.
class ContextualMismatch : public ConstraintFix {
  Type LHS, RHS;

protected:
  ContextualMismatch(ConstraintSystem &cs, Type lhs, Type rhs,
                     ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::ContextualMismatch, locator), LHS(lhs),
        RHS(rhs) {}

public:
  std::string getName() const override { return "fix contextual mismatch"; }

  Type getFromType() const { return LHS; }
  Type getToType() const { return RHS; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static ContextualMismatch *create(ConstraintSystem &cs, Type lhs, Type rhs,
                                    ConstraintLocator *locator);
};

/// Detect situations when argument of the @autoclosure parameter is itself
/// marked as @autoclosure and is not applied. Form a fix which suggests a
/// proper way to forward such arguments, e.g.:
///
/// ```swift
/// func foo(_ fn: @autoclosure () -> Int) {}
/// func bar(_ fn: @autoclosure () -> Int) {
///   foo(fn) // error - fn should be called
/// }
/// ```
class AutoClosureForwarding final : public ConstraintFix {
  AutoClosureForwarding(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AutoClosureForwarding, locator) {}

public:
  std::string getName() const override { return "fix @autoclosure forwarding"; }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AutoClosureForwarding *create(ConstraintSystem &cs,
                                       ConstraintLocator *locator);
};

class RemoveUnwrap final : public ConstraintFix {
  Type BaseType;

  RemoveUnwrap(ConstraintSystem &cs, Type baseType, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::RemoveUnwrap, locator), BaseType(baseType) {}

public:
  std::string getName() const override {
    return "remove unwrap operator `!` or `?`";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static RemoveUnwrap *create(ConstraintSystem &cs, Type baseType,
                              ConstraintLocator *locator);
};

class InsertExplicitCall final : public ConstraintFix {
  InsertExplicitCall(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::InsertCall, locator) {}

public:
  std::string getName() const override {
    return "insert explicit `()` to make a call";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static InsertExplicitCall *create(ConstraintSystem &cs,
                                    ConstraintLocator *locator);
};

class UseSubscriptOperator final : public ConstraintFix {
  UseSubscriptOperator(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::UseSubscriptOperator, locator) {}

public:
  std::string getName() const override {
    return "replace '.subscript(...)' with subscript operator";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static UseSubscriptOperator *create(ConstraintSystem &cs,
                                      ConstraintLocator *locator);
};

class DefineMemberBasedOnUse final : public ConstraintFix {
  Type BaseType;
  DeclName Name;

  DefineMemberBasedOnUse(ConstraintSystem &cs, Type baseType, DeclName member,
                         ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::DefineMemberBasedOnUse, locator),
        BaseType(baseType), Name(member) {}

public:
  std::string getName() const override {
    llvm::SmallVector<char, 16> scratch;
    auto memberName = Name.getString(scratch);
    return "define missing member named '" + memberName.str() +
           "' based on its use";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static DefineMemberBasedOnUse *create(ConstraintSystem &cs, Type baseType,
                                        DeclName member,
                                        ConstraintLocator *locator);
};
	
class AllowTypeOrInstanceMember final : public ConstraintFix {
  Type BaseType;
  DeclName Name;

public:
  AllowTypeOrInstanceMember(ConstraintSystem &cs, Type baseType, DeclName member,
                            ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowTypeOrInstanceMember, locator),
        BaseType(baseType), Name(member) {}

  std::string getName() const override {
    return "allow access to instance member on type or a type member on instance";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowTypeOrInstanceMember *create(ConstraintSystem &cs, Type baseType,
                                           DeclName member,
                                           ConstraintLocator *locator);
};

class AllowInvalidPartialApplication final : public ConstraintFix {
  AllowInvalidPartialApplication(bool isWarning, ConstraintSystem &cs,
                                 ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowInvalidPartialApplication, locator,
                      isWarning) {}

public:
  std::string getName() const override {
    return "allow partially applied 'mutating' method";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowInvalidPartialApplication *create(bool isWarning,
                                                ConstraintSystem &cs,
                                                ConstraintLocator *locator);
};

class AllowInvalidInitRef final : public ConstraintFix {
  enum class RefKind {
    DynamicOnMetatype,
    ProtocolMetatype,
    NonConstMetatype,
  } Kind;

  Type BaseType;
  const ConstructorDecl *Init;
  bool IsStaticallyDerived;
  SourceRange BaseRange;

  AllowInvalidInitRef(ConstraintSystem &cs, RefKind kind, Type baseTy,
                      ConstructorDecl *init, bool isStaticallyDerived,
                      SourceRange baseRange, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowInvalidInitRef, locator), Kind(kind),
        BaseType(baseTy), Init(init), IsStaticallyDerived(isStaticallyDerived),
        BaseRange(baseRange) {}

public:
  std::string getName() const override {
    return "allow invalid initializer reference";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowInvalidInitRef *
  dynamicOnMetatype(ConstraintSystem &cs, Type baseTy, ConstructorDecl *init,
                    SourceRange baseRange, ConstraintLocator *locator);

  static AllowInvalidInitRef *
  onProtocolMetatype(ConstraintSystem &cs, Type baseTy, ConstructorDecl *init,
                     bool isStaticallyDerived, SourceRange baseRange,
                     ConstraintLocator *locator);

  static AllowInvalidInitRef *onNonConstMetatype(ConstraintSystem &cs,
                                                 Type baseTy,
                                                 ConstructorDecl *init,
                                                 ConstraintLocator *locator);

private:
  static AllowInvalidInitRef *create(RefKind kind, ConstraintSystem &cs,
                                     Type baseTy, ConstructorDecl *init,
                                     bool isStaticallyDerived,
                                     SourceRange baseRange,
                                     ConstraintLocator *locator);
};

class AllowClosureParamDestructuring final : public ConstraintFix {
  FunctionType *ContextualType;

  AllowClosureParamDestructuring(ConstraintSystem &cs,
                                 FunctionType *contextualType,
                                 ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowClosureParameterDestructuring, locator),
        ContextualType(contextualType) {}

public:
  std::string getName() const override {
    return "allow closure parameter destructuring";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowClosureParamDestructuring *create(ConstraintSystem &cs,
                                                FunctionType *contextualType,
                                                ConstraintLocator *locator);
};

class AddMissingArguments final
    : public ConstraintFix,
      private llvm::TrailingObjects<AddMissingArguments,
                                    AnyFunctionType::Param> {
  friend TrailingObjects;

  using Param = AnyFunctionType::Param;

  FunctionType *Fn;
  unsigned NumSynthesized;

  AddMissingArguments(ConstraintSystem &cs, FunctionType *funcType,
                      llvm::ArrayRef<AnyFunctionType::Param> synthesizedArgs,
                      ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AddMissingArguments, locator), Fn(funcType),
        NumSynthesized(synthesizedArgs.size()) {
    std::uninitialized_copy(synthesizedArgs.begin(), synthesizedArgs.end(),
                            getSynthesizedArgumentsBuf().begin());
  }

public:
  std::string getName() const override { return "synthesize missing argument(s)"; }

  ArrayRef<Param> getSynthesizedArguments() const {
    return {getTrailingObjects<Param>(), NumSynthesized};
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AddMissingArguments *create(ConstraintSystem &cs, FunctionType *fnType,
                                     llvm::ArrayRef<Param> synthesizedArgs,
                                     ConstraintLocator *locator);

private:
  MutableArrayRef<Param> getSynthesizedArgumentsBuf() {
    return {getTrailingObjects<Param>(), NumSynthesized};
  }
};

class MoveOutOfOrderArgument final : public ConstraintFix {
  using ParamBinding = SmallVector<unsigned, 1>;

  unsigned ArgIdx;
  unsigned PrevArgIdx;

  SmallVector<ParamBinding, 4> Bindings;

  MoveOutOfOrderArgument(ConstraintSystem &cs, unsigned argIdx,
                         unsigned prevArgIdx, ArrayRef<ParamBinding> bindings,
                         ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::MoveOutOfOrderArgument, locator),
        ArgIdx(argIdx), PrevArgIdx(prevArgIdx),
        Bindings(bindings.begin(), bindings.end()) {}

public:
  std::string getName() const override {
    return "move out-of-order argument to correct position";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static MoveOutOfOrderArgument *create(ConstraintSystem &cs,
                                        unsigned argIdx,
                                        unsigned prevArgIdx,
                                        ArrayRef<ParamBinding> bindings,
                                        ConstraintLocator *locator);
};

class AllowInaccessibleMember final : public ConstraintFix {
  ValueDecl *Member;

  AllowInaccessibleMember(ConstraintSystem &cs, ValueDecl *member,
                          ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowInaccessibleMember, locator),
        Member(member) {}

public:
  std::string getName() const override {
    return "allow inaccessible member reference";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowInaccessibleMember *create(ConstraintSystem &cs,
                                         ValueDecl *member,
                                         ConstraintLocator *locator);
};

class AllowAnyObjectKeyPathRoot final : public ConstraintFix {

  AllowAnyObjectKeyPathRoot(ConstraintSystem &cs, ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowAnyObjectKeyPathRoot, locator) {}

public:
  std::string getName() const override {
    return "allow anyobject as root type for a keypath";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowAnyObjectKeyPathRoot *create(ConstraintSystem &cs,
                                           ConstraintLocator *locator);
};

class TreatKeyPathSubscriptIndexAsHashable final : public ConstraintFix {
  Type NonConformingType;

  TreatKeyPathSubscriptIndexAsHashable(ConstraintSystem &cs, Type type,
                                       ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::TreatKeyPathSubscriptIndexAsHashable,
                      locator),
        NonConformingType(type) {}

public:
  std::string getName() const override {
    return "treat keypath subscript index as conforming to Hashable";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static TreatKeyPathSubscriptIndexAsHashable *
  create(ConstraintSystem &cs, Type type, ConstraintLocator *locator);
};

class AllowStaticMemberRefInKeyPath final : public ConstraintFix {
  ValueDecl *Member;

  AllowStaticMemberRefInKeyPath(ConstraintSystem &cs, ValueDecl *member,
                                ConstraintLocator *locator)
      : ConstraintFix(cs, FixKind::AllowStaticMemberRefInKeyPath, locator),
        Member(member) {}

public:
  std::string getName() const override {
    return "allow reference to a static member as a key path component";
  }

  bool diagnose(Expr *root, bool asNote = false) const override;

  static AllowStaticMemberRefInKeyPath *
  create(ConstraintSystem &cs, ValueDecl *member, ConstraintLocator *locator);
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H
