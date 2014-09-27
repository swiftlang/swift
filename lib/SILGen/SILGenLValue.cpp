//===--- SILGenLValue.cpp - Constructs logical lvalues for SILGen ---------===//
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
//
// Emission of l-value expressions and basic operations on them.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "Initialization.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"
#include "ASTVisitor.h"
using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//

/// A pending writeback.
namespace swift {
namespace Lowering {

struct LLVM_LIBRARY_VISIBILITY LValueWriteback {
  SILLocation loc;
  std::unique_ptr<LogicalPathComponent> component;
  ManagedValue base;
  Materialize temp;
  SILValue ExtraInfo;

  ~LValueWriteback() {}
  LValueWriteback(LValueWriteback&&) = default;
  LValueWriteback &operator=(LValueWriteback&&) = default;

  LValueWriteback() = default;
  LValueWriteback(SILLocation loc,
                  std::unique_ptr<LogicalPathComponent> &&comp,
                  ManagedValue base, Materialize temp, SILValue extraInfo)
    : loc(loc), component(std::move(comp)), base(base), temp(temp),
      ExtraInfo(extraInfo) {
  }

  void diagnoseConflict(const LValueWriteback &rhs, SILGenFunction &SGF) const {
    // If the two writebacks we're comparing are of different kinds (e.g.
    // ownership conversion vs a computed property) then they aren't the
    // same and thus cannot conflict.
    if (component->getKind() != rhs.component->getKind())
      return;

    // If the lvalues don't have the same base value, then they aren't the same.
    // Note that this is the primary source of false negative for this
    // diagnostic.
    if (base.getValue() != rhs.base.getValue())
      return;

    component->diagnoseWritebackConflict(rhs.component.get(), loc, rhs.loc,SGF);
  }

  void performWriteback(SILGenFunction &gen) {
    component->writeback(gen, loc, base, temp, ExtraInfo);
  }
};
}
}

std::vector<LValueWriteback> &SILGenFunction::getWritebackStack() {
  if (!WritebackStack)
    WritebackStack = new std::vector<LValueWriteback>();

  return *WritebackStack;
}

void SILGenFunction::freeWritebackStack() {
  delete WritebackStack;
}

Materialize SILGenFunction::emitMaterialize(SILLocation loc, ManagedValue v) {
  // Address-only values are already materialized.
  if (v.getType().isAddress()) {
    assert(v.getType().isAddressOnly(SGM.M) && "can't materialize an l-value");
    return Materialize{v.getValue(), v.getCleanup()};
  }
  
  assert(!v.isLValue() && "materializing a non-address-only lvalue?!");
  auto &lowering = getTypeLowering(v.getType().getSwiftType());
  
  // We don't use getBufferForExprResult here because the result of a
  // materialization is *not* the value, but an address of the value.
  SILValue tmpMem = emitTemporaryAllocation(loc, v.getType());
  v.forwardInto(*this, loc, tmpMem);
  
  CleanupHandle valueCleanup = CleanupHandle::invalid();
  if (!lowering.isTrivial())
    valueCleanup = enterDestroyCleanup(tmpMem);
  
  return Materialize{tmpMem, valueCleanup};
}

//===----------------------------------------------------------------------===//

static CanType getSubstFormalRValueType(Expr *expr) {
  return expr->getType()->getRValueType()->getCanonicalType();
}

static AbstractionPattern getOrigFormalRValueType(Type formalStorageType) {
  auto type =
    formalStorageType->getReferenceStorageReferent()->getCanonicalType();
  return AbstractionPattern(type);
}

/// Return the LValueTypeData for the formal type of a declaration
/// that needs no substitutions.
static LValueTypeData getUnsubstitutedTypeData(SILGenFunction &gen,
                                               CanType formalRValueType) {
  return {
    AbstractionPattern(formalRValueType),
    formalRValueType,
    gen.getLoweredType(formalRValueType),
  };
}

static LValueTypeData getMemberTypeData(SILGenFunction &gen,
                                        Type memberStorageType,
                                        Expr *lvalueExpr) {
  auto origFormalType = getOrigFormalRValueType(memberStorageType);
  auto substFormalType = getSubstFormalRValueType(lvalueExpr);
  return {
    origFormalType,
    substFormalType,
    gen.getLoweredType(origFormalType, substFormalType)
  };
}

/// SILGenLValue - An ASTVisitor for building logical lvalues.
class LLVM_LIBRARY_VISIBILITY SILGenLValue
  : public Lowering::ExprVisitor<SILGenLValue, LValue, AccessKind>
{
public:
  SILGenFunction &gen;
  SILGenLValue(SILGenFunction &gen) : gen(gen) {}
  
  LValue visitRec(Expr *e, AccessKind accessKind);
  
  /// Dummy handler to log unimplemented nodes.
  LValue visitExpr(Expr *e, AccessKind accessKind);

  // Nodes that form the root of lvalue paths
  LValue visitDiscardAssignmentExpr(DiscardAssignmentExpr *e,
                                    AccessKind accessKind);
  LValue visitDeclRefExpr(DeclRefExpr *e, AccessKind accessKind);

  // Nodes that make up components of lvalue paths
  
  LValue visitMemberRefExpr(MemberRefExpr *e, AccessKind accessKind);
  LValue visitSubscriptExpr(SubscriptExpr *e, AccessKind accessKind);
  LValue visitTupleElementExpr(TupleElementExpr *e, AccessKind accessKind);
  LValue visitForceValueExpr(ForceValueExpr *e, AccessKind accessKind);
  LValue visitBindOptionalExpr(BindOptionalExpr *e, AccessKind accessKind);
  
  // Expressions that wrap lvalues
  
  LValue visitInOutExpr(InOutExpr *e, AccessKind accessKind);
  LValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e,
                                       AccessKind accessKind);
};

SILValue LogicalPathComponent::getMaterialized(SILGenFunction &gen,
                                               SILLocation loc,
                                               ManagedValue base) const {
  // If the writeback is disabled, just emit a load into a temporary memory
  // location.
  if (!gen.InWritebackScope) {
    ManagedValue value = get(gen, loc, base, SGFContext());
    return gen.emitMaterialize(loc, value).address;
  }

  // Otherwise, we need to emit a get and set.  The get operation will consume
  // the base's +1, so copy the base for the setter.
  ManagedValue getterBase = base;
  if (base && base.hasCleanup())
    getterBase = base.copy(gen, loc);

  ManagedValue value = get(gen, loc, getterBase, SGFContext());
  Materialize temp = gen.emitMaterialize(loc, value);
  
  gen.getWritebackStack().emplace_back(loc,
                                       clone(gen, loc), base, temp, SILValue());
  return temp.address;
}

void LogicalPathComponent::writeback(SILGenFunction &gen, SILLocation loc,
                                     ManagedValue base, Materialize temporary,
                                     SILValue otherInfo) const {
  ManagedValue mv = temporary.claim(gen, loc);
  set(gen, loc, RValue(gen, loc, getSubstFormalType(), mv), base);
}

ManagedValue Materialize::claim(SILGenFunction &gen, SILLocation loc) {
  auto &addressTL = gen.getTypeLowering(address.getType());
  if (addressTL.isAddressOnly()) {
    // We can use the temporary as an address-only rvalue directly.
    return ManagedValue(address, valueCleanup);
  }

  // A materialized temporary is always its own type-of-rvalue because
  // we did a semantic load to produce it in the first place.

  if (valueCleanup.isValid())
    gen.Cleanups.forwardCleanup(valueCleanup);
  return gen.emitLoad(loc, address, addressTL, SGFContext(), IsTake);
}

static ManagedValue claimValueInMemory(SILGenFunction &gen, SILLocation loc,
                                       SILValue address) {
  auto &addressTL = gen.getTypeLowering(address.getType());
  if (addressTL.isAddressOnly()) {
    // We can use the temporary as an address-only rvalue directly.
    return gen.emitManagedBufferWithCleanup(address, addressTL);
  }

  // A materialized temporary is always its own type-of-rvalue because
  // we did a semantic load to produce it in the first place.
  return gen.emitLoad(loc, address, addressTL, SGFContext(), IsTake);
}

WritebackScope::WritebackScope(SILGenFunction &g)
  : gen(&g), wasInWritebackScope(g.InWritebackScope),
    savedDepth(g.getWritebackStack().size())
{
  // If we're in an inout conversion scope, disable nested writeback scopes.
  if (g.InInOutConversionScope) {
    gen = nullptr;
    return;
  }
  g.InWritebackScope = true;
}

WritebackScope::~WritebackScope() {
  if (!gen)
    return;

  // Pop the InWritebackScope bit.
  gen->InWritebackScope = wasInWritebackScope;

  // Check to see if there is anything going on here.
  auto i = gen->getWritebackStack().end(),
       deepest = gen->getWritebackStack().begin() + savedDepth;
  if (i == deepest) return;

  while (i-- > deepest) {
    // Attempt to diagnose problems where obvious aliasing introduces illegal
    // code.  We do a simple N^2 comparison here to detect this because it is
    // extremely unlikely more than a few writebacks are active at once.
    if (i != deepest) {
      for (auto j = i-1; j >= deepest; --j)
        i->diagnoseConflict(*j, *gen);
    }

    // Claim the address of each and then perform the writeback from the
    // temporary allocation to the source we copied from.
    i->performWriteback(*gen);
  }
  
  gen->getWritebackStack().erase(deepest, gen->getWritebackStack().end());
}

WritebackScope::WritebackScope(WritebackScope &&o)
  : gen(o.gen),
    wasInWritebackScope(o.wasInWritebackScope),
    savedDepth(o.savedDepth)
{
  o.gen = nullptr;
}

WritebackScope &WritebackScope::operator=(WritebackScope &&o) {
  gen = o.gen;
  wasInWritebackScope = o.wasInWritebackScope;
  savedDepth = o.savedDepth;
  o.gen = nullptr;
  return *this;
}

InOutConversionScope::InOutConversionScope(SILGenFunction &gen)
  : gen(gen)
{
  assert(gen.InWritebackScope
         && "inout conversions should happen in writeback scopes");
  assert(!gen.InInOutConversionScope
         && "inout conversions should not be nested");
  gen.InInOutConversionScope = true;
}

InOutConversionScope::~InOutConversionScope() {
  assert(gen.InInOutConversionScope && "already exited conversion scope?!");
  gen.InInOutConversionScope = false;
}

void PathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}

void PathComponent::dump() const {
  print(llvm::errs());
}

/// Return the LValueTypeData for a value whose type is its own
/// lowering.
static LValueTypeData getValueTypeData(SILValue value) {
  assert(value.getType().isObject() ||
         value.getType().getSwiftRValueType()->isExistentialType() ||
         value.getType().getSwiftRValueType()->is<ArchetypeType>());
  return {
    AbstractionPattern(value.getType().getSwiftRValueType()),
    value.getType().getSwiftRValueType(),
    value.getType()
  };
}

namespace {
  class RefElementComponent : public PhysicalPathComponent {
    VarDecl *Field;
    SILType SubstFieldType;
  public:
    RefElementComponent(VarDecl *field, SILType substFieldType,
                        LValueTypeData typeData)
      : PhysicalPathComponent(typeData, RefElementKind),
        Field(field), SubstFieldType(substFieldType) {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind)
      const override {
      assert(base.getType().isObject() &&
             "base for ref element component must be an object");
      assert(base.getType().hasReferenceSemantics() &&
             "base for ref element component must be a reference type");
      auto Res = gen.B.createRefElementAddr(loc, base.getValue(), Field,
                                            SubstFieldType);
      return ManagedValue::forLValue(Res);
    }

    void print(raw_ostream &OS) const override {
      OS << "RefElementComponent(" << Field->getName() << ")\n";
    }
  };

  class TupleElementComponent : public PhysicalPathComponent {
    unsigned ElementIndex;
  public:
    TupleElementComponent(unsigned elementIndex, LValueTypeData typeData)
      : PhysicalPathComponent(typeData, TupleElementKind),
        ElementIndex(elementIndex) {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      assert(base && "invalid value for element base");
      auto Res = gen.B.createTupleElementAddr(loc, base.getUnmanagedValue(),
                                              ElementIndex,
                                              getTypeOfRValue().getAddressType());
      return ManagedValue::forLValue(Res);
    }

    void print(raw_ostream &OS) const override {
      OS << "TupleElementComponent(" << ElementIndex << ")\n";
    }
  };

  class StructElementComponent : public PhysicalPathComponent {
    VarDecl *Field;
    SILType SubstFieldType;
  public:
    StructElementComponent(VarDecl *field, SILType substFieldType,
                           LValueTypeData typeData)
      : PhysicalPathComponent(typeData, StructElementKind),
        Field(field), SubstFieldType(substFieldType) {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      assert(base && "invalid value for element base");
      auto Res = gen.B.createStructElementAddr(loc, base.getUnmanagedValue(),
                                               Field, SubstFieldType);
      return ManagedValue::forLValue(Res);
    }
    void print(raw_ostream &OS) const override {
      OS << "StructElementComponent(" << Field->getName() << ")\n";
    }
  };

  /// Abstract base class for components that project the object out of
  /// optionals.
  class OptionalObjectComponent : public PhysicalPathComponent {
    SILType SubstFieldType;
  public:
    OptionalObjectComponent(LValueTypeData typeData)
      : PhysicalPathComponent(typeData, OptionalObjectKind)
    {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      // Assert that the optional value is present.
      gen.emitPreconditionOptionalHasValue(loc, base.getValue());
      // Project out the 'Some' payload.
      OptionalTypeKind otk;
      auto objTy
        = base.getType().getSwiftRValueType()->getAnyOptionalObjectType(otk);
      assert(objTy);
      (void)objTy;
      
      EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl(otk);
      // UncheckedTakeEnumDataAddr is safe to apply to Optional, because it is
      // a single-payload enum. There will (currently) never be spare bits
      // embedded in the payload.
      SILValue someAddr = gen.B
        .createUncheckedTakeEnumDataAddr(loc, base.getValue(),
                                   someDecl,
                                   getTypeData().TypeOfRValue.getAddressType());
      return ManagedValue::forLValue(someAddr);
    }

    void print(raw_ostream &OS) const override {
      OS << "OptionalObjectComponent()\n";
    }

  protected:
    // Get the address of the object within the optional wrapper, assuming it
    // has already been validated at the current insertion point.
    ManagedValue getOffsetOfObject(SILGenFunction &gen, SILLocation loc,
                                   ManagedValue base) const {
      // Project out the 'Some' payload.
      OptionalTypeKind otk;
      auto objTy
        = base.getType().getSwiftRValueType()->getAnyOptionalObjectType(otk);
      assert(objTy);
      (void)objTy;
      
      EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl(otk);
      // UncheckedTakeEnumDataAddr is safe to apply to Optional, because it is
      // a single-payload enum. There will (currently) never be spare bits
      // embedded in the payload.
      SILValue someAddr = gen.B
        .createUncheckedTakeEnumDataAddr(loc, base.getValue(),
                                   someDecl,
                                   getTypeData().TypeOfRValue.getAddressType());
      return ManagedValue::forLValue(someAddr);
    }
  };
  
  class ForceOptionalObjectComponent : public OptionalObjectComponent {
  public:
    using OptionalObjectComponent::OptionalObjectComponent;
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      // Assert that the optional value is present.
      gen.emitPreconditionOptionalHasValue(loc, base.getValue());
      // Project out the payload.
      return getOffsetOfObject(gen, loc, base);
    }

    void print(raw_ostream &OS) const override {
      OS << "ForceOptionalObjectComponent()\n";
    }
  };
  
  class BindOptionalObjectComponent : public OptionalObjectComponent {
    unsigned Depth;
  public:
    BindOptionalObjectComponent(LValueTypeData typeData,
                                unsigned Depth)
      : OptionalObjectComponent(typeData), Depth(Depth)
    {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      // Check if the optional value is present.
      gen.emitBindOptional(loc, base.getUnmanagedValue(), Depth);
      
      // Project out the payload on the success branch.
      return getOffsetOfObject(gen, loc, base);
    }
    void print(raw_ostream &OS) const override {
      OS << "BindOptionalObjectComponent(" << Depth << ")\n";
    }
  };

  class ValueComponent : public PhysicalPathComponent {
    ManagedValue Value;
  public:
    ValueComponent(ManagedValue value, LValueTypeData typeData) :
      PhysicalPathComponent(typeData, ValueKind),
      Value(value) {
    }

    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      assert(!base && "value component must be root of lvalue path");
      return Value;
    }

    void print(raw_ostream &OS) const override {
      OS << "ValueComponent()\n";
    }
  };
} // end anonymous namespace.

static bool isReadNoneFunction(const Expr *e) {
  // If this is a curried call to an integer literal conversion operations, then
  // we can "safely" assume it is readnone (btw, yes this is totally gross).
  // This is better to be attribute driven, ala rdar://15587352.
  if (auto *dre = dyn_cast<DeclRefExpr>(e)) {
    DeclName name = dre->getDecl()->getFullName();
    return (name.getArgumentNames().size() == 1 &&
            name.getBaseName().str() == "init" &&
            !name.getArgumentNames()[0].empty() &&
            (name.getArgumentNames()[0].str() == "integerLiteral" ||
             name.getArgumentNames()[0].str() == "_builtinIntegerLiteral"));
  }
  
  // Look through DotSyntaxCallExpr, since the literal functions are curried.
  if (auto *CRCE = dyn_cast<ConstructorRefCallExpr>(e))
    return isReadNoneFunction(CRCE->getFn());
  
  return false;
}


/// Given two expressions used as indexes to the same SubscriptDecl (and thus
/// are guaranteed to have the same AST type) check to see if they are going to
/// produce the same value.
static bool areCertainlyEqualIndices(const Expr *e1, const Expr *e2) {
  if (e1->getKind() != e2->getKind()) return false;
  
  // Look through ParenExpr's.
  if (auto *pe1 = dyn_cast<ParenExpr>(e1)) {
    auto *pe2 = cast<ParenExpr>(e2);
    return areCertainlyEqualIndices(pe1->getSubExpr(), pe2->getSubExpr());
  }
  
  // Calls are identical if the callee and operands are identical and we know
  // that the call is something that is "readnone".
  if (auto *ae1 = dyn_cast<ApplyExpr>(e1)) {
    auto *ae2 = cast<ApplyExpr>(e2);
    return areCertainlyEqualIndices(ae1->getFn(), ae2->getFn()) &&
           areCertainlyEqualIndices(ae1->getArg(), ae2->getArg()) &&
           isReadNoneFunction(ae1->getFn());
  }
  
  // TypeExpr's that produce the same metatype type are identical.
  if (isa<TypeExpr>(e1))
    return true;
  
  if (auto *dre1 = dyn_cast<DeclRefExpr>(e1)) {
    auto *dre2 = cast<DeclRefExpr>(e2);
    return dre1->getDecl() == dre2->getDecl() &&
           dre1->getGenericArgs() == dre2->getGenericArgs();
  }

  // Compare a variety of literals.
  if (auto *il1 = dyn_cast<IntegerLiteralExpr>(e1))
    return il1->getValue() == cast<IntegerLiteralExpr>(e2)->getValue();
  if (auto *il1 = dyn_cast<FloatLiteralExpr>(e1))
    return il1->getValue().bitwiseIsEqual(
                                        cast<FloatLiteralExpr>(e2)->getValue());
  if (auto *bl1 = dyn_cast<BooleanLiteralExpr>(e1))
    return bl1->getValue() == cast<BooleanLiteralExpr>(e2)->getValue();
  if (auto *sl1 = dyn_cast<StringLiteralExpr>(e1))
    return sl1->getValue() == cast<StringLiteralExpr>(e2)->getValue();
  if (auto *cl1 = dyn_cast<CharacterLiteralExpr>(e1))
    return cl1->getValue() == cast<CharacterLiteralExpr>(e2)->getValue();
  
  // Compare tuple expressions.
  if (auto *te1 = dyn_cast<TupleExpr>(e1)) {
    auto *te2 = cast<TupleExpr>(e2);

    // Easy checks: # of elements, trailing closures, element names.
    if (te1->getNumElements() != te2->getNumElements() ||
        te1->hasTrailingClosure() != te2->hasTrailingClosure() ||
        te1->getElementNames() != te2->getElementNames()) {
      return false;
    }

    for (unsigned i = 0, n = te1->getNumElements(); i != n; ++i) {
      if (!areCertainlyEqualIndices(te1->getElement(i), te2->getElement(i)))
        return false;
    }

    return true;
  }

  // Otherwise, we have no idea if they are identical.
  return false;
}

namespace {
  /// A helper class for implementing a component that involves
  /// calling accessors.
  template <class Base>
  class AccessorBasedComponent : public Base {
  protected:
    // The VarDecl or SubscriptDecl being get/set.
    AbstractStorageDecl *decl;
    bool IsSuper;
    bool IsDirectAccessorUse;
    std::vector<Substitution> substitutions;
    Expr *subscriptIndexExpr;
    mutable RValue origSubscripts;

    struct AccessorArgs {
      RValueSource base;
      RValue subscripts;
    };

    /// Returns a tuple of RValues holding the accessor value, base (retained if
    /// necessary), and subscript arguments, in that order.
    AccessorArgs
    prepareAccessorArgs(SILGenFunction &gen, SILLocation loc,
                        ManagedValue base, AbstractFunctionDecl *funcDecl) const
    {
      AccessorArgs result;
      if (base)
        result.base = gen.prepareAccessorBaseArg(loc, base, funcDecl);
      
      if (subscriptIndexExpr) {
        if (!origSubscripts)
          origSubscripts = gen.emitRValue(subscriptIndexExpr);
        // TODO: use the subscript expression as the source if we're
        // only using this l-value once.
        result.subscripts = origSubscripts.copy(gen, loc);
      }
      
      return result;
    }

     AccessorBasedComponent(PathComponent::KindTy kind,
                            AbstractStorageDecl *decl,
                            bool isSuper, bool isDirectAccessorUse,
                            ArrayRef<Substitution> substitutions,
                            LValueTypeData typeData,
                            Expr *subscriptIndexExpr)
      : Base(typeData, kind), decl(decl),
        IsSuper(isSuper), IsDirectAccessorUse(isDirectAccessorUse),
        substitutions(substitutions.begin(), substitutions.end()),
        subscriptIndexExpr(subscriptIndexExpr)
    {
    }

    AccessorBasedComponent(const AccessorBasedComponent &copied,
                           SILGenFunction &gen,
                           SILLocation loc)
      : Base(copied.getTypeData(), copied.getKind()),
        decl(copied.decl),
        IsSuper(copied.IsSuper),
        IsDirectAccessorUse(copied.IsDirectAccessorUse),
        substitutions(copied.substitutions),
        subscriptIndexExpr(copied.subscriptIndexExpr),
        origSubscripts(copied.origSubscripts.copy(gen, loc)) {}

    void printBase(raw_ostream &OS, StringRef name) const {
      OS << name << "(" << decl->getName() << ")";
      if (IsSuper) OS << " isSuper";
      if (IsDirectAccessorUse) OS << " isDirectAccessorUse";
      if (subscriptIndexExpr) {
        OS << " subscript_index:\n";
        subscriptIndexExpr->print(OS, 2);
      }
      OS << '\n';
    }
  };

  class GetterSetterComponent
    : public AccessorBasedComponent<LogicalPathComponent> {
  public:

     GetterSetterComponent(AbstractStorageDecl *decl,
                           bool isSuper, bool isDirectAccessorUse,
                           ArrayRef<Substitution> substitutions,
                           LValueTypeData typeData,
                           Expr *subscriptIndexExpr = nullptr)
      : AccessorBasedComponent(GetterSetterKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               typeData, subscriptIndexExpr)
    {
    }
    
    GetterSetterComponent(const GetterSetterComponent &copied,
                          SILGenFunction &gen,
                          SILLocation loc)
      : AccessorBasedComponent(copied, gen, loc)
    {
    }
    
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) const override {
      // Pass in just the setter.
      auto args = prepareAccessorArgs(gen, loc, base, decl->getSetter());
      
      return gen.emitSetAccessor(loc, decl, substitutions,
                                 std::move(args.base), IsSuper,
                                 IsDirectAccessorUse,
                                 std::move(args.subscripts),
                                 std::move(value));
    }

    SILValue getMaterialized(SILGenFunction &gen,
                             SILLocation loc,
                             ManagedValue base) const override {
      // If the property is dynamic, or if it doesn't have a
      // materializeForSet, or if we're not in a writeback scope, we
      // should just use the normal logic.
      FuncDecl *materializeForSet;
      if (!gen.InWritebackScope ||
          decl->getAttrs().hasAttribute<DynamicAttr>() ||
          !(materializeForSet = decl->getMaterializeForSetFunc())) {
        return LogicalPathComponent::getMaterialized(gen, loc, base);
      }

      // Allocate a temporary.
      SILValue buffer =
        gen.emitTemporaryAllocation(loc, getTypeOfRValue().getObjectType());

      // The materializeForSet operation will consume
      // the base's +1, so copy the base for the setter.
      ManagedValue copiedBase = base;
      if (base && base.hasCleanup())
        copiedBase = base.copy(gen, loc);

      auto args = prepareAccessorArgs(gen, loc, copiedBase, materializeForSet);
      auto addressAndNeedsWriteback =
        gen.emitMaterializeForSetAccessor(loc, decl, substitutions,
                                          std::move(args.base), IsSuper,
                                          IsDirectAccessorUse,
                                          std::move(args.subscripts),
                                          buffer);

      SILValue address = addressAndNeedsWriteback.first;

      // TODO: maybe needsWriteback should be a thin function pointer
      // to which we pass the base?  That would let us use direct
      // access for stored properties with didSet.
      gen.getWritebackStack().emplace_back(loc, clone(gen, loc), base,
                                           Materialize{address,
                                                       CleanupHandle::invalid()},
                                           addressAndNeedsWriteback.second);

      return address;
    }

    void writeback(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base, Materialize temporary,
                   SILValue otherInfo) const {
      // If we don't have otherInfo, we don't have to conditionalize
      // the writeback.
      if (!otherInfo) {
        LogicalPathComponent::writeback(gen, loc, base, temporary, otherInfo);
        return;
      }

      // Otherwise, otherInfo is a flag indicating whether we need to
      // write back.
      SILValue needsWriteback = otherInfo;

      // Mark the writeback as auto-generated so that we don't get
      // warnings if we manage to devirtualize materializeForSet.
      loc.markAutoGenerated();

      SILBasicBlock *writebackBB = gen.createBasicBlock(gen.B.getInsertionBB());
      SILBasicBlock *contBB = gen.createBasicBlock();
      gen.B.createCondBranch(loc, needsWriteback, writebackBB, contBB);

      // The writeback block.
      gen.B.setInsertionPoint(writebackBB); {
        FullExpr scope(gen.Cleanups, CleanupLocation::getCleanupLocation(loc));

        // We need to clone the base here.  We can't just consume it
        // because we're in conditionally-executed code.
        if (base && base.hasCleanup())
          base = base.copy(gen, loc);

        ManagedValue mv = claimValueInMemory(gen, loc, temporary.address);
        set(gen, loc, RValue(gen, loc, getSubstFormalType(), mv), base);
      }

      // Continue.
      gen.B.emitBlock(contBB, loc);
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) const override {
      auto args = prepareAccessorArgs(gen, loc, base, decl->getGetter());
      
      return gen.emitGetAccessor(loc, decl, substitutions,
                                 std::move(args.base), IsSuper,
                                 IsDirectAccessorUse,
                                 std::move(args.subscripts), c);
    }
    
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone = new GetterSetterComponent(*this, gen, loc);
      return std::unique_ptr<LogicalPathComponent>(clone);
    }

    void print(raw_ostream &OS) const override {
      printBase(OS, "GetterSetterComponent");
    }

    /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
    /// the same dynamic PathComponent type as the receiver) to see if they are
    /// identical.  If so, there is a conflicting writeback happening, so emit a
    /// diagnostic.
    void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                   SILLocation loc1, SILLocation loc2,
                                   SILGenFunction &gen) override {
      auto &rhs = (GetterSetterComponent&)*RHS;

      // If the decls match, then this could conflict.
      if (decl != rhs.decl || IsSuper != rhs.IsSuper) return;

      // If this is a simple property access, then we must have a conflict.
      if (!subscriptIndexExpr) {
        assert(isa<VarDecl>(decl));
        gen.SGM.diagnose(loc1, diag::writeback_overlap_property,decl->getName())
           .highlight(loc1.getSourceRange());
        gen.SGM.diagnose(loc2, diag::writebackoverlap_note)
           .highlight(loc2.getSourceRange());
        return;
      }

      // Otherwise, it is a subscript, check the index values.
      // If we haven't emitted the lvalue for some reason, just ignore this.
      if (!origSubscripts || !rhs.origSubscripts) return;
      
      // If the indices are literally identical SILValue's, then there is
      // clearly a conflict.
      if (!origSubscripts.isObviouslyEqual(rhs.origSubscripts)) {
        // If the index value doesn't lower to literally the same SILValue's,
        // do some fuzzy matching to catch the common case.
        if (!areCertainlyEqualIndices(subscriptIndexExpr,
                                      rhs.subscriptIndexExpr))
          return;
      }

      // The locations for the subscripts are almost certainly SubscriptExprs.
      // If so, dig into them to produce better location info in the
      // diagnostics and be able to do more precise analysis.
      auto expr1 = loc1.getAsASTNode<SubscriptExpr>();
      auto expr2 = loc2.getAsASTNode<SubscriptExpr>();

      if (expr1 && expr2) {
        gen.SGM.diagnose(loc1, diag::writeback_overlap_subscript)
           .highlight(expr1->getBase()->getSourceRange());

        gen.SGM.diagnose(loc2, diag::writebackoverlap_note)
           .highlight(expr2->getBase()->getSourceRange());

      } else {
        gen.SGM.diagnose(loc1, diag::writeback_overlap_subscript)
           .highlight(loc1.getSourceRange());
        gen.SGM.diagnose(loc2, diag::writebackoverlap_note)
           .highlight(loc2.getSourceRange());
      }
    }
  };

  /// A physical component which involves calling addressors.
  class AddressorComponent
      : public AccessorBasedComponent<PhysicalPathComponent> {
    SILType SubstFieldType;
  public:
     AddressorComponent(AbstractStorageDecl *decl,
                        bool isSuper, bool isDirectAccessorUse,
                        ArrayRef<Substitution> substitutions,
                        LValueTypeData typeData, SILType substFieldType,
                        Expr *subscriptIndexExpr = nullptr)
      : AccessorBasedComponent(AddressorKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               typeData, subscriptIndexExpr),
        SubstFieldType(substFieldType)
    {
    }

    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) const override {
      auto args = prepareAccessorArgs(gen, loc, base,
                                      decl->getAddressorForAccess(accessKind));
      return gen.emitAddressorAccessor(loc, decl, accessKind, substitutions,
                                       std::move(args.base), IsSuper,
                                       IsDirectAccessorUse,
                                       std::move(args.subscripts),
                                       SubstFieldType);
    }

    void print(raw_ostream &OS) const override {
      printBase(OS, "AddressorComponent");
    }
  };
} // end anonymous namespace.

namespace {
  /// Remap an lvalue referencing a generic type to an lvalue of its substituted
  /// type in a concrete context.
  class OrigToSubstComponent : public LogicalPathComponent {
    AbstractionPattern origType;
    CanType substType;
    
  public:
    OrigToSubstComponent(SILGenFunction &gen,
                         AbstractionPattern origType, CanType substType)
      : LogicalPathComponent(getUnsubstitutedTypeData(gen, substType),
                             OrigToSubstKind),
        origType(origType), substType(substType)
    {}
    
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) const override {
      // Map the value to the original abstraction level.
      ManagedValue mv = std::move(value).getAsSingleValue(gen, loc);
      mv = gen.emitSubstToOrigValue(loc, mv, origType, substType);
      // Store to the base.
      mv.assignInto(gen, loc, base.getValue());
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) const override {
      // Load the original value.
      ManagedValue baseVal = gen.emitLoad(loc, base.getValue(),
                                          gen.getTypeLowering(base.getType()),
                                          SGFContext(),
                                          IsNotTake);
      // Map the base value to its substituted representation.
      return gen.emitOrigToSubstValue(loc, baseVal,
                                      origType, substType, c);
    }
    
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone
        = new OrigToSubstComponent(gen, origType, substType);
      return std::unique_ptr<LogicalPathComponent>(clone);
    }

    /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
    /// the same dynamic PathComponent type as the receiver) to see if they are
    /// identical.  If so, there is a conflicting writeback happening, so emit a
    /// diagnostic.
    void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                   SILLocation loc1, SILLocation loc2,
                                   SILGenFunction &gen) override {
      //      auto &rhs = (GetterSetterComponent&)*RHS;

    }

    void print(raw_ostream &OS) const override {
      OS << "OrigToSubstComponent(...)\n";
    }
  };
} // end anonymous namespace.

namespace {
  /// Remap a weak value to Optional<T>*, or unowned pointer to T*.
  class OwnershipComponent : public LogicalPathComponent {
  public:
    OwnershipComponent(LValueTypeData typeData)
      : LogicalPathComponent(typeData, OwnershipKind) {
    }


    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) const override {
      assert(base && "ownership component must not be root of lvalue path");
      auto &TL = gen.getTypeLowering(getTypeData().TypeOfRValue);

      // Load the original value.
      ManagedValue result = gen.emitLoad(loc, base.getValue(), TL,
                                         SGFContext(), IsNotTake);
      return result;
    }

    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) const override {
      assert(base && "ownership component must not be root of lvalue path");
      auto &TL = gen.getTypeLowering(base.getType());

      gen.emitSemanticStore(loc,
                            std::move(value).forwardAsSingleValue(gen, loc),
                            base.getValue(), TL, IsNotInitialization);
    }

    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone = new OwnershipComponent(getTypeData());
      return std::unique_ptr<LogicalPathComponent>(clone);
    }

    /// Compare 'this' lvalue and the 'rhs' lvalue (which is guaranteed to have
    /// the same dynamic PathComponent type as the receiver) to see if they are
    /// identical.  If so, there is a conflicting writeback happening, so emit a
    /// diagnostic.
    void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                   SILLocation loc1, SILLocation loc2,
                                   SILGenFunction &gen) override {
      //      auto &rhs = (GetterSetterComponent&)*RHS;

    }

    void print(raw_ostream &OS) const override {
      OS << "OwnershipComponent(...)\n";
    }
  };
} // end anonymous namespace.

LValue SILGenFunction::emitLValue(Expr *e, AccessKind accessKind) {
  LValue r = SILGenLValue(*this).visit(e, accessKind);
  // If the final component is physical with an abstraction change, introduce a
  // reabstraction component.
  if (r.isLastComponentPhysical()
      && getTypeLowering(r.getSubstFormalType()).getLoweredType()
            != r.getTypeOfRValue()) {
    r.add(new OrigToSubstComponent(*this, r.getOrigFormalType(),
                                   r.getSubstFormalType()));
  }
  return r;
}

LValue SILGenLValue::visitRec(Expr *e, AccessKind accessKind) {
  // Non-lvalue types (references, values, metatypes, etc) form the root of a
  // logical l-value.
  if (!e->getType()->is<LValueType>() && !e->getType()->is<InOutType>()) {
    // Calls through opaque protocols can be done with +0 rvalues.  This allows
    // us to avoid materializing copies of existentials.
    SGFContext Ctx;
    if (gen.SGM.Types.isIndirectPlusZeroSelfParameter(e->getType()))
      Ctx = SGFContext::AllowPlusZero;
    
    ManagedValue rv = gen.emitRValueAsSingleValue(e, Ctx);
    auto typeData = getValueTypeData(rv.getValue());
    LValue lv;
    lv.add(new ValueComponent(rv, typeData));
    return lv;
  }

  return visit(e, accessKind);
}

LValue SILGenLValue::visitExpr(Expr *e, AccessKind accessKind) {
  e->dump(llvm::errs());
  llvm_unreachable("unimplemented lvalue expr");
}

static ArrayRef<Substitution>
getNonMemberVarDeclSubstitutions(SILGenFunction &gen, VarDecl *var) {
  ArrayRef<Substitution> substitutions;
  if (auto genericParams
      = gen.SGM.Types.getEffectiveGenericParamsForContext(
                                                      var->getDeclContext()))
    substitutions = gen.buildForwardingSubstitutions(genericParams);
  return substitutions;
}

// For now, we don't need either an AccessKind or an
// AccessSemantics, because addressors are always directly
// dispatched.
static AddressorComponent *
makeNonMemberVarDeclAddressorComponent(SILGenFunction &gen, VarDecl *var,
                                       const LValueTypeData &typeData) {
  assert(var->hasAddressors());
  SILType storageType = gen.getLoweredType(var->getType()).getAddressType();
  return new AddressorComponent(var, /*isSuper=*/ false, /*direct*/ true,
                                getNonMemberVarDeclSubstitutions(gen, var),
                                typeData, storageType);
}

LValue
SILGenFunction::emitLValueForAddressedNonMemberVarDecl(SILLocation loc,
                                                       VarDecl *var,
                                                       CanType formalRValueType,
                                                       AccessKind accessKind,
                                                       AccessSemantics semantics) {
  auto typeData = getUnsubstitutedTypeData(*this, formalRValueType);
  LValue lv;
  lv.add(makeNonMemberVarDeclAddressorComponent(*this, var, typeData));
  return lv;
}

static LValue emitLValueForNonMemberVarDecl(SILGenFunction &gen,
                                            SILLocation loc, VarDecl *var,
                                            CanType formalRValueType,
                                            AccessKind accessKind,
                                            AccessSemantics semantics) {
  LValue lv;
  auto typeData = getUnsubstitutedTypeData(gen, formalRValueType);

  switch (var->getAccessStrategy(semantics, accessKind)) {

  case AccessStrategy::DispatchToAccessor:
    llvm_unreachable("can't polymorphically access non-member variable");

  // If it's a computed variable, push a reference to the getter and setter.
  case AccessStrategy::DirectToAccessor:
    lv.add(new GetterSetterComponent(var, /*isSuper=*/false, /*direct*/ true,
                                     getNonMemberVarDeclSubstitutions(gen, var),
                                     typeData));
    break;

  case AccessStrategy::Addressor:
    lv.add(makeNonMemberVarDeclAddressorComponent(gen, var, typeData));
    break;

  case AccessStrategy::Storage: {
    // If it's a physical value (e.g. a local variable in memory), push its
    // address.
    auto address = gen.emitLValueForDecl(loc, var, formalRValueType,
                                         accessKind, semantics);
    assert(address.isLValue() &&
           "physical lvalue decl ref must evaluate to an address");
    lv.add(new ValueComponent(address, typeData));

    if (address.getType().is<ReferenceStorageType>())
      lv.add(new OwnershipComponent(typeData));
    break;
  }
  }

  return std::move(lv);
}


LValue SILGenLValue::visitDiscardAssignmentExpr(DiscardAssignmentExpr *e,
                                                AccessKind accessKind) {
  auto formalRValueType = getSubstFormalRValueType(e);
  auto typeData = getUnsubstitutedTypeData(gen, formalRValueType);

  SILValue address = gen.emitTemporaryAllocation(e, typeData.TypeOfRValue);
  LValue lv;
  lv.add(new ValueComponent(ManagedValue::forUnmanaged(address), typeData));
  return std::move(lv);
}


LValue SILGenLValue::visitDeclRefExpr(DeclRefExpr *e, AccessKind accessKind) {
  // The only non-member decl that can be an lvalue is VarDecl.
  return emitLValueForNonMemberVarDecl(gen, e, cast<VarDecl>(e->getDecl()),
                                       getSubstFormalRValueType(e),
                                       accessKind,
                                       e->getAccessSemantics());
}

LValue SILGenLValue::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e,
                                                   AccessKind accessKind) {
  // If it is convenient to avoid loading the base, don't bother loading it.
  gen.emitRValue(e->getLHS(), SGFContext::AllowPlusZero);
  return visitRec(e->getRHS(), accessKind);
}

static AccessKind getBaseAccessKindForAccessor(FuncDecl *accessor) {
  if (accessor->isMutating()) {
    return AccessKind::ReadWrite;
  } else {
    return AccessKind::Read;
  }
}

/// Return the appropriate access kind for the base l-value of a
/// particular member, which is being accessed in a particular way.
static AccessKind getBaseAccessKind(AbstractStorageDecl *member,
                                    AccessKind accessKind,
                                    AccessStrategy strategy) {
  switch (strategy) {
  // Assume that the member only partially projects the enclosing value.
  case AccessStrategy::Storage:
    return (accessKind == AccessKind::Read
              ? AccessKind::Read : AccessKind::ReadWrite);

  case AccessStrategy::Addressor:
    return getBaseAccessKindForAccessor(
                           member->getAddressorForAccess(accessKind));

  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor:
    if (accessKind == AccessKind::Read) {
      return getBaseAccessKindForAccessor(member->getGetter());
    } else {
      return getBaseAccessKindForAccessor(member->getSetter());
    }
  }
  llvm_unreachable("bad access strategy");
}

LValue SILGenLValue::visitMemberRefExpr(MemberRefExpr *e,
                                        AccessKind accessKind) {
  // MemberRefExpr can refer to type and function members, but the only case
  // that can be an lvalue is a VarDecl.
  VarDecl *var = cast<VarDecl>(e->getMember().getDecl());
  AccessStrategy strategy =
    var->getAccessStrategy(e->getAccessSemantics(), accessKind);

  LValue lv = [&]{
    // If we're emitting an initializer, the base is a reference to 'self', and
    // we're doing direct property access, emit a +0 reference to self to avoid
    // retain/release traffic that breaks brittle custom r/r implementations in
    // ObjC.
    if (gen.EmittingClassInitializer
        && e->getAccessSemantics() == AccessSemantics::DirectToStorage) {
      if (auto baseDeclRef = dyn_cast<DeclRefExpr>(e->getBase())) {
        if (baseDeclRef->getDecl()->getName() == gen.getASTContext().Id_self) {
          ManagedValue self = gen.emitSelfForDirectPropertyInConstructor(
                             e->getBase(), cast<VarDecl>(baseDeclRef->getDecl()));
          auto typeData = getValueTypeData(self.getValue());
          LValue valueLV;
          valueLV.add(new ValueComponent(self, typeData));
          return valueLV;
        }
      }
    }

    return visitRec(e->getBase(), getBaseAccessKind(var, accessKind, strategy));
  }();

  LValueTypeData typeData = getMemberTypeData(gen, var->getType(), e);

  // Use the property accessors if the variable has accessors and this isn't a
  // direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    lv.add(new GetterSetterComponent(var, e->isSuper(),
                                 strategy == AccessStrategy::DirectToAccessor,
                                     e->getMember().getSubstitutions(),
                                     typeData));
    return std::move(lv);
  }

  assert(strategy == AccessStrategy::Addressor ||
         strategy == AccessStrategy::Storage);

  // Otherwise, the lvalue access is performed with a fragile element reference.
  // Find the substituted storage type.
  SILType varStorageType =
    gen.SGM.Types.getSubstitutedStorageType(var, e->getType());
    
  // For static variables, emit a reference to the global variable backing
  // them.
  // FIXME: This has to be dynamically looked up for classes, and
  // dynamically instantiated for generics.
  if (strategy == AccessStrategy::Storage && var->isStatic()) {
    auto baseMeta = e->getBase()->getType()->castTo<MetatypeType>()
      ->getInstanceType();
    (void)baseMeta;
    assert(!baseMeta->is<BoundGenericType>() &&
           "generic static stored properties not implemented");
    assert((baseMeta->getStructOrBoundGenericStruct() ||
            baseMeta->getEnumOrBoundGenericEnum()) &&
           "static stored properties for classes/protocols not implemented");
    
    return emitLValueForNonMemberVarDecl(gen, e, var,
                                         getSubstFormalRValueType(e),
                                         accessKind, e->getAccessSemantics());
  }

  // For member variables, this access is done w.r.t. a base computation that
  // was already emitted.  This member is accessed off of it.
  if (strategy == AccessStrategy::Addressor) {
    lv.add(new AddressorComponent(var, e->isSuper(), /*direct*/ true,
                                  e->getMember().getSubstitutions(),
                                  typeData, varStorageType));
  } else if (!e->getBase()->getType()->is<LValueType>() &&
             !e->getBase()->getType()->is<InOutType>()) {
    assert(e->getBase()->getType()->hasReferenceSemantics());
    lv.add(new RefElementComponent(var, varStorageType, typeData));
  } else {
    lv.add(new StructElementComponent(var, varStorageType, typeData));
  }
  
  // If the member has weak or unowned storage, convert it away.
  if (varStorageType.is<ReferenceStorageType>()) {
    lv.add(new OwnershipComponent(typeData));
  }
  
  return lv;
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e,
                                        AccessKind accessKind) {
  auto decl = cast<SubscriptDecl>(e->getDecl().getDecl());
  auto typeData = getMemberTypeData(gen, decl->getElementType(), e);

  AccessStrategy strategy =
    decl->getAccessStrategy(e->getAccessSemantics(), accessKind);
  
  LValue lv = visitRec(e->getBase(),
                       getBaseAccessKind(decl, accessKind, strategy));

  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    lv.add(new GetterSetterComponent(decl, e->isSuper(),
                                     strategy == AccessStrategy::DirectToAccessor,
                                     e->getDecl().getSubstitutions(),
                                     typeData, e->getIndex()));
  } else {
    assert(strategy == AccessStrategy::Addressor);
    auto storageType = 
      gen.SGM.Types.getSubstitutedStorageType(decl, e->getType());
    lv.add(new AddressorComponent(decl, e->isSuper(), /*direct*/ true,
                                  e->getDecl().getSubstitutions(),
                                  typeData, storageType, e->getIndex()));
  }

  return lv;
}

LValue SILGenLValue::visitTupleElementExpr(TupleElementExpr *e,
                                           AccessKind accessKind) {
  unsigned index = e->getFieldNumber();
  LValue lv = visitRec(e->getBase(),
                       accessKind == AccessKind::Read
                         ? AccessKind::Read : AccessKind::ReadWrite);

  auto baseTypeData = lv.getTypeData();
  LValueTypeData typeData = {
    baseTypeData.OrigFormalType.getTupleElementType(index),
    cast<TupleType>(baseTypeData.SubstFormalType).getElementType(index),
    baseTypeData.TypeOfRValue.getTupleElementType(index)
  };

  lv.add(new TupleElementComponent(index, typeData));
  return lv;
}

static LValueTypeData
getOptionalObjectTypeData(SILGenFunction &gen,
                          const LValueTypeData &baseTypeData) {
  OptionalTypeKind otk;
  CanType objectTy = baseTypeData.SubstFormalType.getAnyOptionalObjectType(otk);
  assert(objectTy);
  EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl(otk);
  
  return {
    AbstractionPattern(someDecl->getArgumentType()),
    objectTy,
    baseTypeData.TypeOfRValue.getEnumElementType(someDecl, gen.SGM.M),
  };
}

LValue SILGenLValue::visitForceValueExpr(ForceValueExpr *e,
                                         AccessKind accessKind) {
  LValue lv = visitRec(e->getSubExpr(), accessKind);
  LValueTypeData typeData = getOptionalObjectTypeData(gen, lv.getTypeData());
  lv.add(new ForceOptionalObjectComponent(typeData));
  return lv;
}

LValue SILGenLValue::visitBindOptionalExpr(BindOptionalExpr *e,
                                           AccessKind accessKind) {
  LValue lv = visitRec(e->getSubExpr(), accessKind);
  LValueTypeData typeData = getOptionalObjectTypeData(gen, lv.getTypeData());
  lv.add(new BindOptionalObjectComponent(typeData, e->getDepth()));
  return lv;
}

LValue SILGenLValue::visitInOutExpr(InOutExpr *e, AccessKind accessKind) {
  return visitRec(e->getSubExpr(), accessKind);
}

LValue SILGenFunction::emitDirectIVarLValue(SILLocation loc, ManagedValue base,
                                            VarDecl *ivar,
                                            AccessKind accessKind) {
  SILGenLValue sgl(*this);
  LValue lv;
  
  auto baseType = base.getType().getSwiftRValueType();

  // Refer to 'self' as the base of the lvalue.
  lv.add(new ValueComponent(base, getUnsubstitutedTypeData(*this, baseType)));

  auto origFormalType = getOrigFormalRValueType(ivar->getType());
  auto substFormalType = base.getType().getSwiftRValueType()
    ->getTypeOfMember(F.getModule().getSwiftModule(),
                      ivar, nullptr)
    ->getCanonicalType();
  LValueTypeData typeData = { origFormalType, substFormalType,
                              getLoweredType(origFormalType, substFormalType) };

  // Find the substituted storage type.
  SILType varStorageType =
    SGM.Types.getSubstitutedStorageType(ivar, LValueType::get(substFormalType));

  if (baseType->hasReferenceSemantics())
    lv.add(new RefElementComponent(ivar, varStorageType, typeData));
  else
    lv.add(new StructElementComponent(ivar, varStorageType, typeData));

  if (varStorageType.is<ReferenceStorageType>()) {
    auto formalRValueType =
      ivar->getType()->getRValueType()->getReferenceStorageReferent();
    auto typeData =
      getUnsubstitutedTypeData(*this, formalRValueType->getCanonicalType());
    lv.add(new OwnershipComponent(typeData));
  }

  return lv;
}

/// Load an r-value out of the given address.
///
/// \param rvalueTL - the type lowering for the type-of-rvalue
///   of the address
ManagedValue SILGenFunction::emitLoad(SILLocation loc, SILValue addr,
                                      const TypeLowering &rvalueTL,
                                      SGFContext C, IsTake_t isTake) {
  // Get the lowering for the address type.  We can avoid a re-lookup
  // in the very common case of this being equivalent to the r-value
  // type.
  auto &addrTL =
    (addr.getType() == rvalueTL.getLoweredType().getAddressType()
       ? rvalueTL : getTypeLowering(addr.getType()));

  if (rvalueTL.isAddressOnly()) {
    // If the client is cool with a +0 rvalue, the decl has an address-only
    // type, and there are no conversions, then we can return this as a +0
    // address RValue.
    if (C.isPlusZeroOk() && rvalueTL.getLoweredType() ==addrTL.getLoweredType())
      return ManagedValue::forUnmanaged(addr);
        
    // Copy the address-only value.
    SILValue copy = getBufferForExprResult(loc, rvalueTL.getLoweredType(), C);
    emitSemanticLoadInto(loc, addr, addrTL, copy, rvalueTL,
                         isTake, IsInitialization);
    return manageBufferForExprResult(copy, rvalueTL, C);
  }
  
  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = emitSemanticLoad(loc, addr, addrTL, rvalueTL, isTake);
  return emitManagedRValueWithCleanup(loadedV, rvalueTL);
}

static void emitUnloweredStoreOfCopy(SILBuilder &B, SILLocation loc,
                                     SILValue value, SILValue addr,
                                     IsInitialization_t isInit) {
  if (isInit)
    B.createStore(loc, value, addr);
  else
    B.createAssign(loc, value, addr);
}

#ifndef NDEBUG
static bool hasDifferentTypeOfRValue(const TypeLowering &srcTL) {
  return srcTL.getLoweredType().is<ReferenceStorageType>();
}
#endif

static Substitution getSimpleSubstitution(GenericParamList &generics,
                                          CanType typeArg) {
  assert(generics.getParams().size() == 1);
  auto typeParamDecl = generics.getParams().front();
  return Substitution{typeParamDecl->getArchetype(), typeArg, {}};
}

/// Create the correct substitution for calling the given function at
/// the given type.
static Substitution getSimpleSubstitution(FuncDecl *fn, CanType typeArg) {
  auto polyFnType =
    cast<PolymorphicFunctionType>(fn->getType()->getCanonicalType());
  return getSimpleSubstitution(polyFnType->getGenericParams(), typeArg);
}

static CanType getOptionalValueType(SILType optType,
                                    OptionalTypeKind &optionalKind) {
  auto generic = cast<BoundGenericType>(optType.getSwiftRValueType());
  optionalKind = generic->getDecl()->classifyAsOptionalType();
  assert(optionalKind);
  return generic.getGenericArgs()[0];
}

void SILGenFunction::emitInjectOptionalValueInto(SILLocation loc,
                                                 RValueSource &&value,
                                                 SILValue dest,
                                                 const TypeLowering &optTL) {
  SILType optType = optTL.getLoweredType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn =
    getASTContext().getInjectValueIntoOptionalDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  // Materialize the r-value into a temporary.
  FullExpr scope(Cleanups, CleanupLocation::getCleanupLocation(loc));
  auto valueAddr = std::move(value).materialize(*this,
                              AbstractionPattern(CanType(sub.getArchetype())));

  TemporaryInitialization emitInto(dest, CleanupHandle::invalid());
  auto result = emitApplyOfLibraryIntrinsic(loc, fn, sub, valueAddr,
                                            SGFContext(&emitInto));
  assert(result.isInContext() && "didn't emit directly into buffer?");
  (void)result;
}

void SILGenFunction::emitInjectOptionalNothingInto(SILLocation loc, 
                                                   SILValue dest,
                                                   const TypeLowering &optTL) {
  SILType optType = optTL.getLoweredType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn =
    getASTContext().getInjectNothingIntoOptionalDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  TemporaryInitialization emitInto(dest, CleanupHandle::invalid());
  auto result = emitApplyOfLibraryIntrinsic(loc, fn, sub, {},
                                            SGFContext(&emitInto));
  assert(result.isInContext() && "didn't emit directly into buffer?");
  (void)result;
}

void SILGenFunction::emitPreconditionOptionalHasValue(SILLocation loc,
                                                      SILValue addr) {
  SILType optType = addr.getType().getObjectType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn =
    getASTContext().getPreconditionOptionalHasValueDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  // The argument to _preconditionOptionalHasValue is passed by reference.
  emitApplyOfLibraryIntrinsic(loc, fn, sub,
                              ManagedValue::forUnmanaged(addr),
                              SGFContext());
}

SILValue SILGenFunction::emitDoesOptionalHaveValue(SILLocation loc,
                                                   SILValue addr) {
  SILType optType = addr.getType().getObjectType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn =
    getASTContext().getDoesOptionalHaveValueDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  // The argument to _doesOptionalHaveValue is passed by reference.
  return emitApplyOfLibraryIntrinsic(loc, fn, sub,
                                     ManagedValue::forUnmanaged(addr),
                                     SGFContext())
    .getUnmanagedValue();
}

ManagedValue SILGenFunction::emitGetOptionalValueFrom(SILLocation loc,
                                                      ManagedValue src,
                                                      const TypeLowering &optTL,
                                                      SGFContext C) {
  SILType optType = src.getType().getObjectType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn = getASTContext().getGetOptionalValueDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  return emitApplyOfLibraryIntrinsic(loc, fn, sub, src, C);
}

SILValue SILGenFunction::emitConversionToSemanticRValue(SILLocation loc,
                                                        SILValue src,
                                                  const TypeLowering &valueTL) {
  // Weak storage types are handled with their underlying type.
  assert(!src.getType().is<WeakStorageType>() &&
         "weak pointers are always the right optional types");

  // For @unowned(safe) types, we need to generate a strong retain and
  // strip the unowned box.
  if (auto unownedType = src.getType().getAs<UnownedStorageType>()) {
    B.createStrongRetainUnowned(loc, src);
    return B.createUnownedToRef(loc, src,
                SILType::getPrimitiveObjectType(unownedType.getReferentType()));
  }

  // For @unowned(unsafe) types, we need to strip the unmanaged box
  // and then do an (unsafe) retain.
  if (auto unmanagedType = src.getType().getAs<UnmanagedStorageType>()) {
    auto result = B.createUnmanagedToRef(loc, src,
              SILType::getPrimitiveObjectType(unmanagedType.getReferentType()));
    B.createStrongRetain(loc, result);
    return result;
  }

  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}


/// Given that the type-of-rvalue differs from the type-of-storage,
/// and given that the type-of-rvalue is loadable, produce a +1 scalar
/// of the type-of-rvalue.
static SILValue emitLoadOfSemanticRValue(SILGenFunction &gen,
                                         SILLocation loc,
                                         SILValue src,
                                         const TypeLowering &valueTL,
                                         IsTake_t isTake) {
  SILType storageType = src.getType();

  // For @weak types, we need to create an Optional<T>.
  // Optional<T> is currently loadable, but it probably won't be forever.
  if (storageType.is<WeakStorageType>())
    return gen.B.createLoadWeak(loc, src, isTake);

  // For @unowned(safe) types, we need to strip the unowned box.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    auto unownedValue = gen.B.createLoad(loc, src);
    gen.B.createStrongRetainUnowned(loc, unownedValue);
    if (isTake) gen.B.createUnownedRelease(loc, unownedValue);
    return gen.B.createUnownedToRef(loc, unownedValue,
              SILType::getPrimitiveObjectType(unownedType.getReferentType()));
  }

  // For @unowned(unsafe) types, we need to strip the unmanaged box.
  if (auto unmanagedType = src.getType().getAs<UnmanagedStorageType>()) {
    auto value = gen.B.createLoad(loc, src);
    auto result = gen.B.createUnmanagedToRef(loc, value,
            SILType::getPrimitiveObjectType(unmanagedType.getReferentType()));
    gen.B.createStrongRetain(loc, result);
    return result;
  }

  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Given that the type-of-rvalue differs from the type-of-storage,
/// store a +1 value (possibly not a scalar) of the type-of-rvalue
/// into the given address.
static void emitStoreOfSemanticRValue(SILGenFunction &gen,
                                      SILLocation loc,
                                      SILValue value,
                                      SILValue dest,
                                      const TypeLowering &valueTL,
                                      IsInitialization_t isInit) {
  auto storageType = dest.getType();

  // For @weak types, we need to break down an Optional<T> and then
  // emit the storeWeak ourselves.
  if (storageType.is<WeakStorageType>()) {
    gen.B.createStoreWeak(loc, value, dest, isInit);

    // store_weak doesn't take ownership of the input, so cancel it out.
    gen.B.emitReleaseValue(loc, value);
    return;
  }

  // For @unowned(safe) types, we need to enter the unowned box by
  // turning the strong retain into an unowned retain.
  if (storageType.is<UnownedStorageType>()) {
    auto unownedValue =
      gen.B.createRefToUnowned(loc, value, storageType.getObjectType());
    gen.B.createUnownedRetain(loc, unownedValue);
    emitUnloweredStoreOfCopy(gen.B, loc, unownedValue, dest, isInit);
    gen.B.emitStrongRelease(loc, value);
    return;
  }

  // For @unowned(unsafe) types, we need to enter the unmanaged box and
  // release the strong retain.
  if (storageType.is<UnmanagedStorageType>()) {
    auto unmanagedValue =
      gen.B.createRefToUnmanaged(loc, value, storageType.getObjectType());
    emitUnloweredStoreOfCopy(gen.B, loc, unmanagedValue, dest, isInit);
    gen.B.emitStrongRelease(loc, value);
    return;
  }

  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Load a value of the type-of-rvalue out of the given address as a
/// scalar.  The type-of-rvalue must be loadable.
SILValue SILGenFunction::emitSemanticLoad(SILLocation loc,
                                          SILValue src,
                                          const TypeLowering &srcTL,
                                          const TypeLowering &rvalueTL,
                                          IsTake_t isTake) {
  assert(srcTL.getLoweredType().getAddressType() == src.getType());
  assert(rvalueTL.isLoadable());

  // Easy case: the types match.
  if (srcTL.getLoweredType() == rvalueTL.getLoweredType()) {
    //assert(!hasDifferentTypeOfRValue(srcTL));
    return srcTL.emitLoadOfCopy(B, loc, src, isTake);
  }

  return emitLoadOfSemanticRValue(*this, loc, src, rvalueTL, isTake);
}

/// Load a value of the type-of-reference out of the given address
/// and into the destination address.
void SILGenFunction::emitSemanticLoadInto(SILLocation loc,
                                          SILValue src,
                                          const TypeLowering &srcTL,
                                          SILValue dest,
                                          const TypeLowering &destTL,
                                          IsTake_t isTake,
                                          IsInitialization_t isInit) {
  assert(srcTL.getLoweredType().getAddressType() == src.getType());
  assert(destTL.getLoweredType().getAddressType() == dest.getType());

  // Easy case: the types match.
  if (srcTL.getLoweredType() == destTL.getLoweredType()) {
    //assert(!hasDifferentTypeOfRValue(srcTL));
    B.createCopyAddr(loc, src, dest, isTake, isInit);
    return;
  }

  auto rvalue = emitLoadOfSemanticRValue(*this, loc, src, srcTL, isTake);
  emitUnloweredStoreOfCopy(B, loc, rvalue, dest, isInit);
}

/// Store an r-value into the given address as an initialization.
void SILGenFunction::emitSemanticStore(SILLocation loc,
                                       SILValue rvalue,
                                       SILValue dest,
                                       const TypeLowering &destTL,
                                       IsInitialization_t isInit) {
  assert(destTL.getLoweredType().getAddressType() == dest.getType());

  // Easy case: the types match.
  if (rvalue.getType() == destTL.getLoweredType()) {
    //assert(!hasDifferentTypeOfRValue(destTL));
    assert(destTL.isAddressOnly() == rvalue.getType().isAddress());
    if (rvalue.getType().isAddress()) {
      B.createCopyAddr(loc, rvalue, dest, IsTake, isInit);
    } else {
      emitUnloweredStoreOfCopy(B, loc, rvalue, dest, isInit);
    }
    return;
  }

  auto &rvalueTL = getTypeLowering(rvalue.getType());
  emitStoreOfSemanticRValue(*this, loc, rvalue, dest, rvalueTL, isInit);
}

/// Convert a semantic rvalue to a value of storage type.
SILValue SILGenFunction::emitConversionFromSemanticValue(SILLocation loc,
                                                         SILValue semanticValue,
                                                         SILType storageType) {
  auto &destTL = getTypeLowering(storageType);
  (void)destTL;
  // Easy case: the types match.
  if (semanticValue.getType() == storageType) {
    assert(!hasDifferentTypeOfRValue(destTL));
    return semanticValue;
  }
  
  // @weak types are never loadable, so we don't need to handle them here.
  
  // For @unowned types, place into an unowned box.
  if (storageType.is<UnownedStorageType>()) {
    SILValue unowned = B.createRefToUnowned(loc, semanticValue, storageType);
    B.createUnownedRetain(loc, unowned);
    B.emitStrongRelease(loc, semanticValue);
    return unowned;
  }

  // For @unmanaged types, place into an unmanaged box.
  if (storageType.is<UnmanagedStorageType>()) {
    SILValue unmanaged =
      B.createRefToUnmanaged(loc, semanticValue, storageType);
    B.emitStrongRelease(loc, semanticValue);
    return unmanaged;
  }
  
  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Produce a physical address that corresponds to the given l-value
/// component.
static ManagedValue drillIntoComponent(SILGenFunction &SGF,
                                       SILLocation loc,
                                       const PathComponent &component,
                                       ManagedValue base,
                                       AccessKind accessKind) {
  ManagedValue addr;
  if (component.isPhysical()) {
    addr = component.asPhysical().offset(SGF, loc, base, accessKind);
  } else {
    auto &lcomponent = component.asLogical();
    addr = ManagedValue::forLValue(lcomponent.getMaterialized(SGF, loc, base));
  }

  return addr;
}

/// Find the last component of the given lvalue and derive a base
/// location for it.
static const PathComponent &drillToLastComponent(SILGenFunction &SGF,
                                                 SILLocation loc,
                                                 const LValue &lv,
                                                 ManagedValue &addr,
                                                 AccessKind accessKind) {
  assert(lv.begin() != lv.end() &&
         "lvalue must have at least one component");

  auto component = lv.begin(), next = lv.begin(), end = lv.end();
  ++next;
  for (; next != end; component = next, ++next) {
    addr = drillIntoComponent(SGF, loc, **component, addr, accessKind);
  }

  return **component;
}

ManagedValue SILGenFunction::emitLoadOfLValue(SILLocation loc,
                                              const LValue &src,
                                              SGFContext C) {
  // No need to write back to a loaded lvalue.
  DisableWritebackScope scope(*this);

  ManagedValue addr;
  auto &component =
    drillToLastComponent(*this, loc, src, addr, AccessKind::Read);

  // If the last component is physical, just drill down and load from it.
  if (component.isPhysical()) {
    addr = component.asPhysical().offset(*this, loc, addr, AccessKind::Read);
    return emitLoad(loc, addr.getValue(),
                    getTypeLowering(src.getTypeOfRValue()), C, IsNotTake);
  }

  // If the last component is logical, just emit a get.
  return component.asLogical().get(*this, loc, addr, C);
}

ManagedValue SILGenFunction::emitAddressOfLValue(SILLocation loc,
                                                 const LValue &src,
                                                 AccessKind accessKind) {
  ManagedValue addr;
  auto &component = drillToLastComponent(*this, loc, src, addr, accessKind);
  addr = drillIntoComponent(*this, loc, component, addr, accessKind);
  assert(addr.getType().isAddress() &&
         "resolving lvalue did not give an address");
  return addr;
}

void SILGenFunction::emitAssignToLValue(SILLocation loc, RValue &&src,
                                        const LValue &dest) {
  WritebackScope scope(*this);
  
  // Resolve all components up to the last, keeping track of value-type logical
  // properties we need to write back to.
  ManagedValue destAddr;
  auto &component =
    drillToLastComponent(*this, loc, dest, destAddr, AccessKind::ReadWrite);
  
  // Write to the tail component.
  if (component.isPhysical()) {
    auto finalDestAddr = component.asPhysical().offset(*this, loc, destAddr,
                                                       AccessKind::Write);
    
    std::move(src).getAsSingleValue(*this, loc)
      .assignInto(*this, loc, finalDestAddr.getValue());
  } else {
    component.asLogical().set(*this, loc, std::move(src), destAddr);
  }

  // The writeback scope closing will propagate the value back up through the
  // writeback chain.
}

void SILGenFunction::emitCopyLValueInto(SILLocation loc, const LValue &src,
                                        Initialization *dest) {
  auto skipPeephole = [&]{
    auto loaded = emitLoadOfLValue(loc, src, SGFContext(dest));
    if (!loaded.isInContext())
      RValue(*this, loc, src.getSubstFormalType(), loaded)
        .forwardInto(*this, dest, loc);
  };
  
  // If the source is a physical lvalue, the destination is a single address,
  // and there's no semantic conversion necessary, do a copy_addr from the
  // lvalue into the destination.
  if (!src.isPhysical())
    return skipPeephole();
  auto destAddr = dest->getAddressOrNull();
  if (!destAddr)
    return skipPeephole();
  if (src.getTypeOfRValue().getSwiftRValueType()
        != destAddr.getType().getSwiftRValueType())
    return skipPeephole();
  
  auto srcAddr =
    emitAddressOfLValue(loc, src, AccessKind::Read).getUnmanagedValue();
  B.createCopyAddr(loc, srcAddr, destAddr, IsNotTake, IsInitialization);
  dest->finishInitialization(*this);
}

void SILGenFunction::emitAssignLValueToLValue(SILLocation loc,
                                              const LValue &src,
                                              const LValue &dest) {
  auto skipPeephole = [&]{
    ManagedValue loaded = emitLoadOfLValue(loc, src, SGFContext());
    emitAssignToLValue(loc, RValue(*this, loc, src.getSubstFormalType(),
                                   loaded), dest);
  };
  
  // Only perform the peephole if both operands are physical and there's no
  // semantic conversion necessary.
  if (!src.isPhysical())
    return skipPeephole();
  if (!dest.isPhysical())
    return skipPeephole();
  
  auto srcAddr =
    emitAddressOfLValue(loc, src, AccessKind::Read).getUnmanagedValue();
  auto destAddr =
    emitAddressOfLValue(loc, dest, AccessKind::Write).getUnmanagedValue();

  if (srcAddr.getType() == destAddr.getType()) {
    B.createCopyAddr(loc, srcAddr, destAddr, IsNotTake, IsNotInitialization);
  } else {
    // If there's a semantic conversion necessary, do a load then assign.
    auto loaded = emitLoad(loc, srcAddr, getTypeLowering(src.getTypeOfRValue()),
                           SGFContext(),
                           IsNotTake);
    loaded.assignInto(*this, loc, destAddr);
  }
}
