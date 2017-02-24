//===--- SILGenLValue.cpp - Constructs logical lvalues for SILGen ---------===//
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
// Emission of l-value expressions and basic operations on them.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "ArgumentSource.h"
#include "LValue.h"
#include "RValue.h"
#include "Scope.h"
#include "Initialization.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Mangle.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"
#include "ASTVisitor.h"
using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//

namespace {

struct LValueWritebackCleanup : Cleanup {
  FormalEvaluationContext::stable_iterator Depth;

  LValueWritebackCleanup() : Depth() {}

  void emit(SILGenFunction &gen, CleanupLocation loc) override {
    auto &evaluation = *gen.FormalEvalContext.find(Depth);
    assert(evaluation.getKind() == FormalAccess::Exclusive);
    auto &lvalue = static_cast<ExclusiveBorrowFormalAccess &>(evaluation);
    lvalue.performWriteback(gen, /*isFinal*/ false);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "LValueWritebackCleanup\n"
                 << "State: " << getState() << "Depth: " << Depth.getDepth()
                 << "\n";
#endif
  }
};

} // end anonymous namespace

/// Push a writeback onto the current LValueWriteback stack.
static void pushWriteback(SILGenFunction &gen,
                          SILLocation loc,
                          std::unique_ptr<LogicalPathComponent> &&comp,
                          ManagedValue base,
                          MaterializedLValue materialized) {
  assert(gen.InWritebackScope);

  // Push a cleanup to execute the writeback consistently.
  auto &context = gen.FormalEvalContext;
  LValueWritebackCleanup &cleanup =
      gen.Cleanups.pushCleanup<LValueWritebackCleanup>();
  CleanupHandle handle = gen.Cleanups.getTopCleanup();

  context.push<ExclusiveBorrowFormalAccess>(loc, std::move(comp), base,
                                            materialized, handle);
  cleanup.Depth = context.stable_begin();
}

//===----------------------------------------------------------------------===//

static CanType getSubstFormalRValueType(Expr *expr) {
  return expr->getType()->getRValueType()->getCanonicalType();
}

static LValueTypeData getLogicalStorageTypeData(SILGenModule &SGM,
                                                CanType substFormalType) {
  AbstractionPattern origFormalType(
      substFormalType.getReferenceStorageReferent());
  return {
    origFormalType,
    substFormalType,
    SGM.Types.getLoweredType(origFormalType, substFormalType).getObjectType()
  };
}

static LValueTypeData getPhysicalStorageTypeData(SILGenModule &SGM,
                                                 AbstractStorageDecl *storage,
                                                 CanType substFormalType) {
  auto origFormalType = SGM.Types.getAbstractionPattern(storage)
                                 .getReferenceStorageReferentType();
  return {
    origFormalType,
    substFormalType,
    SGM.Types.getLoweredType(origFormalType, substFormalType).getObjectType()
  };
}

/// SILGenLValue - An ASTVisitor for building logical lvalues.
class LLVM_LIBRARY_VISIBILITY SILGenLValue
  : public Lowering::ExprVisitor<SILGenLValue, LValue, AccessKind>
{
  /// A mapping from opaque value expressions to the open-existential
  /// expression that determines them.
  llvm::SmallDenseMap<OpaqueValueExpr *, OpenExistentialExpr *>
    openedExistentials;

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
  LValue visitOpaqueValueExpr(OpaqueValueExpr *e, AccessKind accessKind);

  // Nodes that make up components of lvalue paths
  
  LValue visitMemberRefExpr(MemberRefExpr *e, AccessKind accessKind);
  LValue visitSubscriptExpr(SubscriptExpr *e, AccessKind accessKind);
  LValue visitTupleElementExpr(TupleElementExpr *e, AccessKind accessKind);
  LValue visitForceValueExpr(ForceValueExpr *e, AccessKind accessKind);
  LValue visitBindOptionalExpr(BindOptionalExpr *e, AccessKind accessKind);
  LValue visitOpenExistentialExpr(OpenExistentialExpr *e,
                                  AccessKind accessKind);

  // Expressions that wrap lvalues
  
  LValue visitInOutExpr(InOutExpr *e, AccessKind accessKind);
  LValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e,
                                       AccessKind accessKind);
};

static ManagedValue
emitGetIntoTemporary(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                     std::unique_ptr<TemporaryInitialization> &&temporaryInit,
                     LogicalPathComponent &&component) {
  // Emit a 'get' into the temporary.
  RValue value =
    std::move(component).get(gen, loc, base, SGFContext(temporaryInit.get()));

  // Force the value into the temporary if necessary.
  if (!value.isInContext()) {
    std::move(value).forwardInto(gen, loc, temporaryInit.get());
  }

  return temporaryInit->getManagedAddress();
}

ManagedValue LogicalPathComponent::getMaterialized(SILGenFunction &gen,
                                                   SILLocation loc,
                                                   ManagedValue base,
                                                   AccessKind kind) && {
  // If this is just for a read, emit a load into a temporary memory
  // location.
  if (kind == AccessKind::Read) {
    // Create a temporary.
    std::unique_ptr<TemporaryInitialization> temporaryInit =
        gen.emitFormalAccessTemporary(loc,
                                      gen.getTypeLowering(getTypeOfRValue()));
    return emitGetIntoTemporary(gen, loc, base, std::move(temporaryInit),
                                std::move(*this));
  }

  assert(gen.InWritebackScope &&
         "materializing l-value for modification without writeback scope");

  // Clone anything else about the component that we might need in the
  // writeback.
  auto clonedComponent = clone(gen, loc);

  ManagedValue temporary;
  {
    // Create a temporary.
    std::unique_ptr<TemporaryInitialization> temporaryInit =
        gen.emitFormalAccessTemporary(loc,
                                      gen.getTypeLowering(getTypeOfRValue()));

    FormalEvaluationScope Scope(gen);

    // Otherwise, we need to emit a get and set.  Borrow the base for
    // the getter.
    ManagedValue getterBase =
        base ? base.formalAccessBorrow(gen, loc) : ManagedValue();

    // Emit a 'get' into a temporary and then pop the borrow of base.
    temporary = emitGetIntoTemporary(
        gen, loc, getterBase, std::move(temporaryInit), std::move(*this));
  }

  // Push a writeback for the temporary.
  pushWriteback(gen, loc, std::move(clonedComponent), base,
                MaterializedLValue(temporary));
  return temporary.unmanagedBorrow();
}

void LogicalPathComponent::writeback(SILGenFunction &gen, SILLocation loc,
                                     ManagedValue base,
                                     MaterializedLValue materialized,
                                     bool isFinal) {
  assert(!materialized.callback &&
         "unexpected materialized lvalue with callback!");

  // Load the value from the temporary unless the type is address-only
  // and this is the final use, in which case we can just consume the
  // value as-is.
  auto temporary = materialized.temporary;

  assert(temporary.getType().isAddress());
  auto &tempTL = gen.getTypeLowering(temporary.getType());
  if (!tempTL.isAddressOnly() || !isFinal) {
    if (isFinal) temporary.forward(gen);
    temporary = gen.emitLoad(loc, temporary.getValue(), tempTL,
                             SGFContext(), IsTake_t(isFinal));
  }
  RValue rvalue(gen, loc, getSubstFormalType(), temporary);

  // Don't consume cleanups on the base if this isn't final.
  if (!isFinal) { base = ManagedValue::forUnmanaged(base.getValue()); }

  // Clone the component if this isn't final.
  std::unique_ptr<LogicalPathComponent> clonedComponent =
    (isFinal ? nullptr : clone(gen, loc));
  LogicalPathComponent *component = (isFinal ? this : &*clonedComponent);
  std::move(*component).set(gen, loc, std::move(rvalue), base);
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

/// Return the LValueTypeData for a SIL value with the given AST formal type.
static LValueTypeData getValueTypeData(CanType formalType,
                                       SILValue value) {
  return {
    AbstractionPattern(formalType),
    formalType,
    value->getType().getObjectType()
  };
}
static LValueTypeData getValueTypeData(SILGenFunction &gen, Expr *e) {
  CanType formalType = getSubstFormalRValueType(e);
  SILType loweredType = gen.getLoweredType(formalType).getObjectType();

  return {
    AbstractionPattern(formalType),
    formalType,
    loweredType
  };
}

/// Given the address of an optional value, unsafely project out the
/// address of the value.
static ManagedValue getAddressOfOptionalValue(SILGenFunction &gen,
                                              SILLocation loc,
                                              ManagedValue optAddr,
                                        const LValueTypeData &valueTypeData) {
  // Project out the 'Some' payload.
  EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl();

  // If the base is +1, we want to forward the cleanup.
  bool hadCleanup = optAddr.hasCleanup();

  // UncheckedTakeEnumDataAddr is safe to apply to Optional, because it is
  // a single-payload enum. There will (currently) never be spare bits
  // embedded in the payload.
  SILValue valueAddr =
    gen.B.createUncheckedTakeEnumDataAddr(loc, optAddr.forward(gen), someDecl,
                                  valueTypeData.TypeOfRValue.getAddressType());

  // Return the value as +1 if the optional was +1.
  if (hadCleanup) {
    return gen.emitManagedBufferWithCleanup(valueAddr);
  } else {
    return ManagedValue::forLValue(valueAddr);
  }
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
                        AccessKind accessKind) && override {
      assert(base.getType().isObject() &&
             "base for ref element component must be an object");
      assert(base.getType().hasReferenceSemantics() &&
             "base for ref element component must be a reference type");
      // Borrow the ref element addr using formal access. If we need the ref
      // element addr, we will load it in this expression.
      base = base.formalAccessBorrow(gen, loc);
      auto Res = gen.B.createRefElementAddr(loc, base.getUnmanagedValue(),
                                            Field, SubstFieldType);
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
                        AccessKind accessKind) && override {
      assert(base && "invalid value for element base");
      // TODO: if the base is +1, break apart its cleanup.
      auto Res = gen.B.createTupleElementAddr(loc, base.getValue(),
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
                        AccessKind accessKind) && override {
      assert(base && "invalid value for element base");
      // TODO: if the base is +1, break apart its cleanup.
      auto Res = gen.B.createStructElementAddr(loc, base.getValue(),
                                               Field, SubstFieldType);
      return ManagedValue::forLValue(Res);
    }
    void print(raw_ostream &OS) const override {
      OS << "StructElementComponent(" << Field->getName() << ")\n";
    }
  };

  /// A physical path component which force-projects the address of
  /// the value of an optional l-value.
  class ForceOptionalObjectComponent : public PhysicalPathComponent {
  public:
    ForceOptionalObjectComponent(LValueTypeData typeData)
      : PhysicalPathComponent(typeData, OptionalObjectKind) {}
    
    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) && override {
      // Assert that the optional value is present and return the projected out
      // payload.
      return gen.emitPreconditionOptionalHasValue(loc, base);
    }

    void print(raw_ostream &OS) const override {
      OS << "ForceOptionalObjectComponent()\n";
    }
  };

  /// A physical path component which projects out an opened archetype
  /// from an existential.
  class OpenOpaqueExistentialComponent : public PhysicalPathComponent {
    static LValueTypeData getOpenedArchetypeTypeData(CanArchetypeType type) {
      return {
        AbstractionPattern::getOpaque(), type,
        SILType::getPrimitiveObjectType(type)
      };
    }
  public:
    OpenOpaqueExistentialComponent(CanArchetypeType openedArchetype)
      : PhysicalPathComponent(getOpenedArchetypeTypeData(openedArchetype),
                              OpenedExistentialKind) {}

    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) && override {
      assert(base.getType().isExistentialType() &&
             "base for open existential component must be an existential");
      auto addr = gen.B.createOpenExistentialAddr(
          loc, base.getLValueAddress(), getTypeOfRValue().getAddressType(),
          getOpenedExistentialAccessFor(accessKind));

      if (base.hasCleanup()) {
        assert(false && "I believe that we should never end up here. One, we "
                        "assert above that base is an l-value address and we "
                        "state l-values don't have associated cleanup. Two, we "
                        "enter deinit of the buffer but don't have "
                        "book-keeping for the value. Three, I believe that "
                        "would mean to have a l-value passed at +1 which I "
                        "don't believe we do.");
        // Leave a cleanup to deinit the existential container.
        gen.enterDeinitExistentialCleanup(base.getValue(), CanType(),
                                          ExistentialRepresentation::Opaque);
      }

      gen.setArchetypeOpeningSite(cast<ArchetypeType>(getSubstFormalType()),
                                  addr);
      return ManagedValue::forLValue(addr);
    }

    void print(raw_ostream &OS) const override {
      OS << "OpenOpaqueExistentialComponent(" << getSubstFormalType() << ")\n";
    }
  };

  /// A physical path component which returns a literal address.
  class ValueComponent : public PhysicalPathComponent {
    ManagedValue Value;
  public:
    ValueComponent(ManagedValue value, LValueTypeData typeData) :
      PhysicalPathComponent(typeData, ValueKind),
      Value(value) {
    }

    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) && override {
      assert(!base && "value component must be root of lvalue path");
      return Value;
    }

    void print(raw_ostream &OS) const override {
      OS << "ValueComponent()\n";
    }
  };
} // end anonymous namespace

static bool isReadNoneFunction(const Expr *e) {
  // If this is a curried call to an integer literal conversion operations, then
  // we can "safely" assume it is readnone (btw, yes this is totally gross).
  // This is better to be attribute driven, a la rdar://15587352.
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

    /// The subscript index expression.  Useless
    Expr *subscriptIndexExpr;
    RValue subscripts;

    /// AST type of the base expression, in case the accessor call
    /// requires re-abstraction.
    CanType baseFormalType;

    struct AccessorArgs {
      ArgumentSource base;
      RValue subscripts;
    };

    /// Returns a tuple of RValues holding the accessor value, base (retained if
    /// necessary), and subscript arguments, in that order.
    AccessorArgs
    prepareAccessorArgs(SILGenFunction &gen, SILLocation loc,
                        ManagedValue base, SILDeclRef accessor) &&
    {
      AccessorArgs result;
      if (base)
        result.base = gen.prepareAccessorBaseArg(loc, base, baseFormalType,
                                                 accessor);

      if (subscripts)
        result.subscripts = std::move(subscripts);
      
      return result;
    }

    AccessorBasedComponent(PathComponent::KindTy kind,
                           AbstractStorageDecl *decl,
                           bool isSuper, bool isDirectAccessorUse,
                           SubstitutionList substitutions,
                           CanType baseFormalType,
                           LValueTypeData typeData,
                           Expr *subscriptIndexExpr,
                           RValue *optSubscripts)
      : Base(typeData, kind), decl(decl),
        IsSuper(isSuper), IsDirectAccessorUse(isDirectAccessorUse),
        substitutions(substitutions.begin(), substitutions.end()),
        subscriptIndexExpr(subscriptIndexExpr),
        baseFormalType(baseFormalType)
    {
      if (optSubscripts)
        subscripts = std::move(*optSubscripts);
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
        subscripts(copied.subscripts.copy(gen, loc)) ,
        baseFormalType(copied.baseFormalType) {}

    virtual SILDeclRef getAccessor(SILGenFunction &gen,
                                   AccessKind kind) const  = 0;

    AccessKind getBaseAccessKind(SILGenFunction &gen,
                                 AccessKind kind) const override {
      SILDeclRef accessor = getAccessor(gen, kind);
      auto accessorSelf = gen.SGM.Types.getConstantSelfParameter(accessor);
      if (accessorSelf.getType() && accessorSelf.isIndirectMutating()) {
        return AccessKind::ReadWrite;
      } else {
        return AccessKind::Read;
      }
    }

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
                           SubstitutionList substitutions,
                           CanType baseFormalType,
                           LValueTypeData typeData,
                           Expr *subscriptIndexExpr = nullptr,
                           RValue *subscriptIndex = nullptr)
      : AccessorBasedComponent(GetterSetterKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               baseFormalType, typeData, subscriptIndexExpr,
                               subscriptIndex)
    {
    }
    
    GetterSetterComponent(const GetterSetterComponent &copied,
                          SILGenFunction &gen,
                          SILLocation loc)
      : AccessorBasedComponent(copied, gen, loc)
    {
    }

    SILDeclRef getAccessor(SILGenFunction &gen,
                           AccessKind accessKind) const override {
      if (accessKind == AccessKind::Read) {
        return gen.getGetterDeclRef(decl, IsDirectAccessorUse);
      } else {
        return gen.getSetterDeclRef(decl, IsDirectAccessorUse);
      }
    }

    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) && override {
      SILDeclRef setter = gen.getSetterDeclRef(decl, IsDirectAccessorUse);

      FormalEvaluationScope scope(gen);
      // Pass in just the setter.
      auto args =
        std::move(*this).prepareAccessorArgs(gen, loc, base, setter);

      return gen.emitSetAccessor(loc, setter, substitutions,
                                 std::move(args.base), IsSuper,
                                 IsDirectAccessorUse,
                                 std::move(args.subscripts),
                                 std::move(value));
    }

    bool shouldUseMaterializeForSet(SILGenFunction &gen,
                                    AccessKind accessKind) {
      // If this access is for a read, we can just call the getter.
      if (accessKind == AccessKind::Read)
        return false;

      // If the declaration is dynamic, there's no materializeForSet.
      if (decl->getAttrs().hasAttribute<DynamicAttr>())
        return false;

      // If the declaration was imported from C, we won't gain anything
      // from using materializeForSet, and furthermore, it might not
      // exist.
      if (decl->hasClangNode())
        return false;

      // If the declaration is not in type context, there's no
      // materializeForSet.
      if (!decl->getDeclContext()->isTypeContext())
        return false;

      // If the declaration is in a different resilience domain, we have
      // to use materializeForSet.
      //
      // FIXME: Use correct ResilienceExpansion if gen is @transparent
      if (!decl->hasFixedLayout(gen.SGM.M.getSwiftModule(),
                                ResilienceExpansion::Maximal))
        return true;

      // If the declaration is dynamically dispatched through a class,
      // we have to use materializeForSet.
      if (auto *classDecl = dyn_cast<ClassDecl>(decl->getDeclContext())) {
        if (decl->isFinal() || classDecl->isFinal())
          return false;

        return true;
      }

      // If the declaration is dynamically dispatched through a
      // non-ObjC protocol, we have to use materializeForSet.
      if (auto *protoDecl = dyn_cast<ProtocolDecl>(decl->getDeclContext()))
        if (!protoDecl->isObjC())
          return true;

      return false;
    }

    ManagedValue getMaterialized(SILGenFunction &gen,
                                 SILLocation loc,
                                 ManagedValue base,
                                 AccessKind accessKind) && override {
      if (!shouldUseMaterializeForSet(gen, accessKind)) {
        return std::move(*this).LogicalPathComponent::getMaterialized(gen,
                                                        loc, base, accessKind);
      }

      assert(decl->getMaterializeForSetFunc() &&
             "polymorphic storage without materializeForSet");
      assert(gen.InWritebackScope &&
             "materializing l-value for modification without writeback scope");

      // Allocate opaque storage for the callback to use.
      SILValue callbackStorage = gen.emitTemporaryAllocation(loc,
        SILType::getPrimitiveObjectType(
                                gen.getASTContext().TheUnsafeValueBufferType));

      // Allocate a temporary.
      SILValue buffer =
        gen.emitTemporaryAllocation(loc, getTypeOfRValue());

      // Clone the component without cloning the indices.  We don't actually
      // consume them in writeback().
      std::unique_ptr<LogicalPathComponent> clonedComponent(
          [&]() -> LogicalPathComponent* {
        // Steal the subscript values without copying them so that we
        // can peek at them in diagnoseWritebackConflict.
        //
        // This is *amazingly* unprincipled.
        RValue borrowedSubscripts;
        RValue *optSubscripts = nullptr;
        if (subscripts) {
          CanType type = subscripts.getType();
          SmallVector<ManagedValue, 4> values;
          std::move(subscripts).getAll(values);
          subscripts = RValue::withPreExplodedElements(values, type);
          borrowedSubscripts = RValue::withPreExplodedElements(values, type);
          optSubscripts = &borrowedSubscripts;
        }
        return new GetterSetterComponent(decl, IsSuper, IsDirectAccessorUse,
                                         substitutions, baseFormalType,
                                         getTypeData(), subscriptIndexExpr,
                                         optSubscripts);
      }());

      SILDeclRef materializeForSet =
        gen.getMaterializeForSetDeclRef(decl, IsDirectAccessorUse);

      MaterializedLValue materialized;
      {
        FormalEvaluationScope Scope(gen);

        // If the base is a +1 r-value, just borrow it for materializeForSet.
        // prepareAccessorArgs will copy it if necessary.
        ManagedValue borrowedBase =
            base ? base.formalAccessBorrow(gen, loc) : ManagedValue();

        auto args = std::move(*this).prepareAccessorArgs(gen, loc, borrowedBase,
                                                         materializeForSet);
        materialized = gen.emitMaterializeForSetAccessor(
            loc, materializeForSet, substitutions, std::move(args.base),
            IsSuper, IsDirectAccessorUse, std::move(args.subscripts), buffer,
            callbackStorage);

        // Mark a value-dependence on the base.  We do this regardless
        // of whether the base is trivial because even a trivial base
        // may be value-dependent on something non-trivial.
        if (base) {
          SILValue temporary = materialized.temporary.getValue();
          materialized.temporary = ManagedValue::forUnmanaged(
              gen.B.createMarkDependence(loc, temporary, base.getValue()));
        }
      }

      // TODO: maybe needsWriteback should be a thin function pointer
      // to which we pass the base?  That would let us use direct
      // access for stored properties with didSet.
      pushWriteback(gen, loc, std::move(clonedComponent), base, materialized);

      return ManagedValue::forLValue(materialized.temporary.getValue());
    }

    void writeback(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base, MaterializedLValue materialized,
                   bool isFinal) override {
      // If we don't have a callback, we don't have to conditionalize
      // the writeback.
      if (!materialized.callback) {
        LogicalPathComponent::writeback(gen, loc,
                                        base, materialized,
                                        isFinal);
        return;
      }

      // Otherwise, 'materialized' holds an optional callback and the
      // callback storage.

      // Mark the writeback as auto-generated so that we don't get
      // warnings if we manage to devirtualize materializeForSet.
      loc.markAutoGenerated();

      SILModule &M = gen.SGM.M;
      ASTContext &ctx = gen.getASTContext();

      SILBasicBlock *contBB = gen.createBasicBlock();
      SILBasicBlock *writebackBB = gen.createBasicBlock(gen.B.getInsertionBB());

      gen.B.createSwitchEnum(loc, materialized.callback, /*defaultDest*/ nullptr,
                             { { ctx.getOptionalSomeDecl(), writebackBB },
                               { ctx.getOptionalNoneDecl(), contBB } });

      // The writeback block.
      gen.B.setInsertionPoint(writebackBB); {
        FullExpr scope(gen.Cleanups, CleanupLocation::get(loc));

        auto emptyTupleTy =
          SILType::getPrimitiveObjectType(TupleType::getEmpty(ctx));
        auto rawPointerTy = SILType::getRawPointerType(ctx);

        // The callback is a BB argument from the switch_enum.
        SILValue callback = writebackBB->createPHIArgument(
            rawPointerTy, ValueOwnershipKind::Trivial);

        // Cast the callback to the correct polymorphic function type.
        auto origCallbackFnType = gen.SGM.Types.getMaterializeForSetCallbackType(
            decl, materialized.genericSig, materialized.origSelfType);
        auto origCallbackType = SILType::getPrimitiveObjectType(origCallbackFnType);
        callback = gen.B.createPointerToThinFunction(loc, callback, origCallbackType);

        auto substCallbackFnType = origCallbackFnType->substGenericArgs(
            M, substitutions);
        auto substCallbackType = SILType::getPrimitiveObjectType(substCallbackFnType);
        auto metatypeType =
            gen.getSILType(substCallbackFnType->getParameters().back());

        // We need to borrow the base here.  We can't just consume it
        // because we're in conditionally-executed code (and because
        // this might be a non-final use).  We also need to pass it
        // indirectly.
        SILValue baseAddress;
        SILValue baseMetatype;
        if (base) {
          if (base.getType().isAddress()) {
            baseAddress = base.getValue();
          } else {
            AbstractionPattern origSelfType(materialized.genericSig,
                                            materialized.origSelfType);
            base = gen.emitSubstToOrigValue(loc, base, origSelfType,
                                            baseFormalType);

            baseAddress = gen.emitTemporaryAllocation(loc, base.getType());
            gen.B.emitStoreValueOperation(loc, base.getValue(), baseAddress,
                                          StoreOwnershipQualifier::Init);
          }
          baseMetatype = gen.B.createMetatype(loc, metatypeType);

        // Otherwise, we have to pass something; use an empty tuple
        // and an undef metatype.
        } else {
          baseAddress = SILUndef::get(emptyTupleTy.getAddressType(), M);
          baseMetatype = SILUndef::get(metatypeType, M);
        }

        SILValue temporaryPointer =
          gen.B.createAddressToPointer(loc,
                                       materialized.temporary.getValue(),
                                       rawPointerTy);

        // Apply the callback.
        gen.B.createApply(loc, callback, substCallbackType,
                          emptyTupleTy, substitutions, {
                            temporaryPointer,
                            materialized.callbackStorage,
                            baseAddress,
                            baseMetatype
                          }, false);
      }

      // Continue.
      gen.B.emitBlock(contBB, loc);
    }
    
    RValue get(SILGenFunction &gen, SILLocation loc,
               ManagedValue base, SGFContext c) && override {
      SILDeclRef getter = gen.getGetterDeclRef(decl, IsDirectAccessorUse);

      FormalEvaluationScope scope(gen);

      auto args =
        std::move(*this).prepareAccessorArgs(gen, loc, base, getter);
      
      return gen.emitGetAccessor(loc, getter, substitutions,
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

      // If the decl is monomorphically a stored property, allow aliases.
      // It could be overridden by a computed property in a subclass, but
      // that's not likely enough to be worth the strictness here.
      if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
        switch (storage->getStorageKind()) {
        case AbstractStorageDecl::Stored:
        case AbstractStorageDecl::StoredWithTrivialAccessors:
        case AbstractStorageDecl::Addressed:
        case AbstractStorageDecl::AddressedWithTrivialAccessors:
          return;
        // TODO: Stored properties with didSet accessors that don't look at the
        // oldValue could also be addressed.
        case AbstractStorageDecl::StoredWithObservers:
        case AbstractStorageDecl::AddressedWithObservers:
          break;
          
        case AbstractStorageDecl::InheritedWithObservers:
        case AbstractStorageDecl::Computed:
        case AbstractStorageDecl::ComputedWithMutableAddress:
          break;
        }
      }
      
      // If the property is a generic requirement, allow aliases, because
      // it may be conformed to using a stored property.
      if (isa<ProtocolDecl>(decl->getDeclContext()))
        return;

      // If this is a simple property access, then we must have a conflict.
      if (!subscripts) {
        assert(isa<VarDecl>(decl));
        gen.SGM.diagnose(loc1, diag::writeback_overlap_property,decl->getName())
           .highlight(loc1.getSourceRange());
        gen.SGM.diagnose(loc2, diag::writebackoverlap_note)
           .highlight(loc2.getSourceRange());
        return;
      }

      // Otherwise, it is a subscript, check the index values.
      
      // If the indices are literally identical SILValue's, then there is
      // clearly a conflict.
      if (!subscripts.isObviouslyEqual(rhs.subscripts)) {
        // If the index value doesn't lower to literally the same SILValue's,
        // do some fuzzy matching to catch the common case.
        if (!subscriptIndexExpr ||
            !rhs.subscriptIndexExpr ||
            !areCertainlyEqualIndices(subscriptIndexExpr,
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

  class UnpinPseudoComponent : public LogicalPathComponent {
  public:
    UnpinPseudoComponent(const LValueTypeData &typeData)
      : LogicalPathComponent(typeData, WritebackPseudoKind) {}

  private:
    AccessKind getBaseAccessKind(SILGenFunction &SGF,
                                 AccessKind accessKind) const override {
      llvm_unreachable("called getBaseAccessKind on pseudo-component");
    }
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation l) const override {
      llvm_unreachable("called clone on pseudo-component");
    }

    RValue get(SILGenFunction &gen, SILLocation loc,
               ManagedValue base, SGFContext c) && override {
      llvm_unreachable("called get on a pseudo-component");
    }
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) && override {
      llvm_unreachable("called set on a pseudo-component");
    }
    ManagedValue getMaterialized(SILGenFunction &gen, SILLocation loc,
                                 ManagedValue base,
                                 AccessKind accessKind) && override {
      llvm_unreachable("called getMaterialized on a pseudo-component");
    }

    void diagnoseWritebackConflict(LogicalPathComponent *rhs,
                                   SILLocation loc1, SILLocation loc2,
                                   SILGenFunction &gen) override {
      // do nothing
    }

    void writeback(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base,
                   MaterializedLValue materialized,
                   bool isFinal) override {
      // If this is final, we can consume the owner (stored as
      // 'base').  If it isn't, we actually need to retain it, because
      // we've still got a release active.
      SILValue baseValue = (isFinal ? base.forward(gen) : base.getValue());
      if (!isFinal)
        baseValue = gen.B.createCopyValue(loc, baseValue);

      gen.B.createStrongUnpin(loc, baseValue, Atomicity::Atomic);
    }

    void print(raw_ostream &OS) const override {
      OS << "UnpinPseudoComponent";
    }
  };

  /// A physical component which involves calling addressors.
  class AddressorComponent
      : public AccessorBasedComponent<PhysicalPathComponent> {
    SILType SubstFieldType;
  public:
     AddressorComponent(AbstractStorageDecl *decl,
                        bool isSuper, bool isDirectAccessorUse,
                        SubstitutionList substitutions,
                        CanType baseFormalType, LValueTypeData typeData,
                        SILType substFieldType,
                        Expr *subscriptIndexExpr = nullptr,
                        RValue *subscriptIndex = nullptr)
      : AccessorBasedComponent(AddressorKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               baseFormalType, typeData, subscriptIndexExpr,
                               subscriptIndex),
        SubstFieldType(substFieldType)
    {
    }

    SILDeclRef getAccessor(SILGenFunction &gen,
                           AccessKind accessKind) const override {
      return gen.getAddressorDeclRef(decl, accessKind, IsDirectAccessorUse);
    }

    ManagedValue offset(SILGenFunction &gen, SILLocation loc, ManagedValue base,
                        AccessKind accessKind) && override {
      assert(gen.InWritebackScope &&
             "offsetting l-value for modification without writeback scope");

      SILDeclRef addressor = gen.getAddressorDeclRef(decl, accessKind, 
                                                     IsDirectAccessorUse);
      std::pair<ManagedValue, ManagedValue> result;
      {
        FormalEvaluationScope scope(gen);

        auto args =
            std::move(*this).prepareAccessorArgs(gen, loc, base, addressor);
        result = gen.emitAddressorAccessor(
            loc, addressor, substitutions, std::move(args.base), IsSuper,
            IsDirectAccessorUse, std::move(args.subscripts), SubstFieldType);
      }
      switch (cast<FuncDecl>(addressor.getDecl())->getAddressorKind()) {
      case AddressorKind::NotAddressor:
        llvm_unreachable("not an addressor!");

      // For unsafe addressors, we have no owner pointer to manage.
      case AddressorKind::Unsafe:
        assert(!result.second);
        return result.first;

      // For owning addressors, we can just let the owner get released
      // at an appropriate point.
      case AddressorKind::Owning:
      case AddressorKind::NativeOwning:
        return result.first;

      // For pinning addressors, we have to push a writeback.
      case AddressorKind::NativePinning: {
        std::unique_ptr<LogicalPathComponent>
          component(new UnpinPseudoComponent(getTypeData()));
        pushWriteback(gen, loc, std::move(component), result.second,
                      MaterializedLValue());
        return result.first;
      }
      }
      llvm_unreachable("bad addressor kind");
    }

    void print(raw_ostream &OS) const override {
      printBase(OS, "AddressorComponent");
    }
  };
} // end anonymous namespace

RValue
TranslationPathComponent::get(SILGenFunction &gen, SILLocation loc,
                              ManagedValue base, SGFContext c) && {
  // Load the original value.
  RValue baseVal(gen, loc, getSubstFormalType(),
           gen.emitLoad(loc, base.getValue(),
                        gen.getTypeLowering(base.getType()),
                        SGFContext(), IsNotTake));

  // Map the base value to its substituted representation.
  return std::move(*this).translate(gen, loc, std::move(baseVal), c);
}

void TranslationPathComponent::set(SILGenFunction &gen, SILLocation loc,
                                   RValue &&value, ManagedValue base) && {
  // Map the value to the original pattern.
  RValue newValue = std::move(*this).untranslate(gen, loc, std::move(value));

  // Store to the base.
  std::move(newValue).assignInto(gen, loc, base.getValue());
}

namespace {
  /// Remap an lvalue referencing a generic type to an lvalue of its
  /// substituted type in a concrete context.
  class OrigToSubstComponent : public TranslationPathComponent {
    AbstractionPattern OrigType;

  public:
    OrigToSubstComponent(AbstractionPattern origType,
                         CanType substFormalType,
                         SILType loweredSubstType)
      : TranslationPathComponent({ AbstractionPattern(substFormalType),
                                   substFormalType, loweredSubstType },
                                 OrigToSubstKind),
        OrigType(origType)
    {}

    RValue untranslate(SILGenFunction &gen, SILLocation loc,
                       RValue &&rv, SGFContext c) && override {
      return gen.emitSubstToOrigValue(loc, std::move(rv), OrigType,
                                      getSubstFormalType(), c);
    }

    RValue translate(SILGenFunction &gen, SILLocation loc,
                     RValue &&rv, SGFContext c) && override {
      return gen.emitOrigToSubstValue(loc, std::move(rv), OrigType,
                                      getSubstFormalType(), c);
    }

    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone
        = new OrigToSubstComponent(OrigType, getSubstFormalType(),
                                   getTypeOfRValue());
      return std::unique_ptr<LogicalPathComponent>(clone);
    }

    void print(raw_ostream &OS) const override {
      OS << "OrigToSubstComponent("
         << getOrigFormalType() << ", "
         << getSubstFormalType() << ", "
         << getTypeOfRValue() << ")\n";
    }
  };

  /// Remap an lvalue referencing a concrete type to an lvalue of a
  /// generically-reabstracted type.
  class SubstToOrigComponent : public TranslationPathComponent {
  public:
    SubstToOrigComponent(AbstractionPattern origType,
                         CanType substFormalType,
                         SILType loweredSubstType)
      : TranslationPathComponent({ origType, substFormalType, loweredSubstType },
                             SubstToOrigKind)
    {}

    RValue untranslate(SILGenFunction &gen, SILLocation loc,
                       RValue &&rv, SGFContext c) && override {
      return gen.emitOrigToSubstValue(loc, std::move(rv), getOrigFormalType(),
                                      getSubstFormalType(), c);
    }

    RValue translate(SILGenFunction &gen, SILLocation loc,
                     RValue &&rv, SGFContext c) && override {
      return gen.emitSubstToOrigValue(loc, std::move(rv), getOrigFormalType(),
                                      getSubstFormalType(), c);
    }
    
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone
        = new SubstToOrigComponent(getOrigFormalType(), getSubstFormalType(),
                                   getTypeOfRValue());
      return std::unique_ptr<LogicalPathComponent>(clone);
    }

    void print(raw_ostream &OS) const override {
      OS << "SubstToOrigComponent("
         << getOrigFormalType() << ", "
         << getSubstFormalType() << ", "
         << getTypeOfRValue() << ")\n";
    }
  };

  /// Remap a weak value to Optional<T>*, or unowned pointer to T*.
  class OwnershipComponent : public LogicalPathComponent {
  public:
    OwnershipComponent(LValueTypeData typeData)
      : LogicalPathComponent(typeData, OwnershipKind) {
    }

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
               ManagedValue base, SGFContext c) && override {
      assert(base && "ownership component must not be root of lvalue path");
      auto &TL = gen.getTypeLowering(getTypeOfRValue());

      // Load the original value.
      ManagedValue result = gen.emitLoad(loc, base.getValue(), TL,
                                         SGFContext(), IsNotTake);
      return RValue(gen, loc, getSubstFormalType(), result);
    }

    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) && override {
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

    void print(raw_ostream &OS) const override {
      OS << "OwnershipComponent(...)\n";
    }
  };
} // end anonymous namespace

LValue LValue::forValue(ManagedValue value,
                        CanType substFormalType) {
  assert(value.getType().isObject());
  LValueTypeData typeData = getValueTypeData(substFormalType,
                                             value.getValue());

  LValue lv;
  lv.add<ValueComponent>(value, typeData);
  return lv;
}

LValue LValue::forAddress(ManagedValue address,
                          AbstractionPattern origFormalType,
                          CanType substFormalType) {
  assert(address.isLValue());
  LValueTypeData typeData = {
    origFormalType, substFormalType, address.getType().getObjectType()
  };

  LValue lv;
  lv.add<ValueComponent>(address, typeData);
  return lv;
}

void LValue::addMemberComponent(SILGenFunction &gen, SILLocation loc,
                                AbstractStorageDecl *storage,
                                SubstitutionList subs,
                                bool isSuper,
                                AccessKind accessKind,
                                AccessSemantics accessSemantics,
                                AccessStrategy accessStrategy,
                                CanType formalRValueType,
                                RValue &&indices) {
  if (auto var = dyn_cast<VarDecl>(storage)) {
    assert(!indices);
    addMemberVarComponent(gen, loc, var, subs, isSuper,
                          accessKind, accessSemantics, accessStrategy,
                          formalRValueType);
  } else {
    auto subscript = cast<SubscriptDecl>(storage);
    addMemberSubscriptComponent(gen, loc, subscript, subs, isSuper,
                                accessKind, accessSemantics, accessStrategy,
                                formalRValueType, std::move(indices));
  }
}

void LValue::addOrigToSubstComponent(SILType loweredSubstType) {
  loweredSubstType = loweredSubstType.getObjectType();
  assert(getTypeOfRValue() != loweredSubstType &&
         "reabstraction component is unnecessary!");

  // Peephole away complementary reabstractions.
  assert(!Path.empty() && "adding translation component to empty l-value");
  if (Path.back()->getKind() == PathComponent::SubstToOrigKind) {
    // But only if the lowered type matches exactly.
    if (Path[Path.size()-2]->getTypeOfRValue() == loweredSubstType) {
      Path.pop_back();
      return;
    }
    // TODO: combine reabstractions; this doesn't matter all that much
    // for most things, but it can be dramatically better for function
    // reabstraction.
  }
  add<OrigToSubstComponent>(getOrigFormalType(), getSubstFormalType(),
                            loweredSubstType);
}

void LValue::addSubstToOrigComponent(AbstractionPattern origType,
                                     SILType loweredSubstType) {
  loweredSubstType = loweredSubstType.getObjectType();
  assert(getTypeOfRValue() != loweredSubstType &&
         "reabstraction component is unnecessary!");

  // Peephole away complementary reabstractions.
  assert(!Path.empty() && "adding translation component to empty l-value");
  if (Path.back()->getKind() == PathComponent::OrigToSubstKind) {
    // But only if the lowered type matches exactly.
    if (Path[Path.size()-2]->getTypeOfRValue() == loweredSubstType) {
      Path.pop_back();
      return;
    }
    // TODO: combine reabstractions; this doesn't matter all that much
    // for most things, but it can be dramatically better for function
    // reabstraction.
  }

  add<SubstToOrigComponent>(origType, getSubstFormalType(), loweredSubstType);
}

void LValue::dump() const {
  print(llvm::errs());
}

void LValue::print(raw_ostream &OS) const {
  for (const auto &component : *this) {
    component->print(OS);
  }
}

LValue SILGenFunction::emitLValue(Expr *e, AccessKind accessKind) {
  // Some lvalue nodes (namely BindOptionalExprs) require immediate evaluation
  // of their subexpression, so we must have a writeback scope open while
  // building an lvalue.
  assert(InWritebackScope && "must be in a writeback scope");

  LValue r = SILGenLValue(*this).visit(e, accessKind);
  // If the final component has an abstraction change, introduce a
  // reabstraction component.
  auto substFormalType = r.getSubstFormalType();
  auto loweredSubstType = getLoweredType(substFormalType);
  if (r.getTypeOfRValue() != loweredSubstType.getObjectType()) {
    // Logical components always re-abstract back to the substituted
    // type.
    assert(r.isLastComponentPhysical());
    r.addOrigToSubstComponent(loweredSubstType);
  }
  return r;
}

LValue SILGenLValue::visitRec(Expr *e, AccessKind accessKind) {
  // Non-lvalue types (references, values, metatypes, etc) form the root of a
  // logical l-value.
  if (!e->getType()->is<LValueType>() && !e->getType()->is<InOutType>()) {
    // Decide if we can evaluate this expression at +0 for the rest of the
    // lvalue.
    SGFContext Ctx;
    ManagedValue rv;

    // Calls through opaque protocols can be done with +0 rvalues.  This allows
    // us to avoid materializing copies of existentials.
    if (gen.SGM.Types.isIndirectPlusZeroSelfParameter(e->getType()))
      Ctx = SGFContext::AllowGuaranteedPlusZero;
    else if (auto *DRE = dyn_cast<DeclRefExpr>(e)) {
      // Any reference to "self" can be done at +0 so long as it is a direct
      // access, since we know it is guaranteed.
      // TODO: it would be great to factor this even lower into SILGen to the
      // point where we can see that the parameter is +0 guaranteed.  Note that
      // this handles the case in initializers where there is actually a stack
      // allocation for it as well.
      if (isa<ParamDecl>(DRE->getDecl()) &&
          DRE->getDecl()->getName() == gen.getASTContext().Id_self &&
          DRE->getDecl()->isImplicit()) {
        Ctx = SGFContext::AllowGuaranteedPlusZero;
        if (gen.SelfInitDelegationState != SILGenFunction::NormalSelf) {
          // This needs to be inlined since there is a Formal EvaluatioN Scope
          // in emitRValueForDecl that causing any borrow for this LValue to be
          // popped too soon.
          auto *vd = cast<ParamDecl>(DRE->getDecl());
          ManagedValue selfLValue = gen.emitLValueForDecl(
              DRE, vd, DRE->getType()->getCanonicalType(), AccessKind::Read,
              DRE->getAccessSemantics());
          rv = gen.emitRValueForSelfInDelegationInit(
                      e, DRE->getType()->getCanonicalType(),
                      selfLValue.getLValueAddress(), Ctx)
                   .getScalarValue();
        }
      } else if (auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
        // All let values are guaranteed to be held alive across their lifetime,
        // and won't change once initialized.  Any loaded value is good for the
        // duration of this expression evaluation.
        if (VD->isLet())
          Ctx = SGFContext::AllowGuaranteedPlusZero;
      }
    }

    if (!rv)
      rv = gen.emitRValueAsSingleValue(e, Ctx);
    CanType formalType = getSubstFormalRValueType(e);
    auto typeData = getValueTypeData(formalType, rv.getValue());
    LValue lv;
    lv.add<ValueComponent>(rv, typeData);
    return lv;
  }

  return visit(e, accessKind);
}

LValue SILGenLValue::visitExpr(Expr *e, AccessKind accessKind) {
  e->dump(llvm::errs());
  llvm_unreachable("unimplemented lvalue expr");
}

SubstitutionList
SILGenModule::getNonMemberVarDeclSubstitutions(VarDecl *var) {
  SubstitutionList substitutions;
  auto *dc = var->getDeclContext();
  if (auto *genericEnv = dc->getGenericEnvironmentOfContext())
    substitutions = genericEnv->getForwardingSubstitutions();
  return substitutions;
}

// For now, we don't need either an AccessKind or an
// AccessSemantics, because addressors are always directly
// dispatched.
static void
addNonMemberVarDeclAddressorComponent(SILGenModule &SGM, VarDecl *var,
                                      CanType formalRValueType,
                                      LValue &lvalue) {
  assert(!lvalue.isValid());
  auto typeData = getPhysicalStorageTypeData(SGM, var, formalRValueType);
  SILType storageType = SGM.Types.getLoweredType(var->getType()).getAddressType();
  lvalue.add<AddressorComponent>(var, /*isSuper=*/ false, /*direct*/ true,
                                 SGM.getNonMemberVarDeclSubstitutions(var),
                                 CanType(), typeData, storageType);
}

LValue
SILGenFunction::emitLValueForAddressedNonMemberVarDecl(SILLocation loc,
                                                       VarDecl *var,
                                                       CanType formalRValueType,
                                                       AccessKind accessKind,
                                                       AccessSemantics semantics) {
  LValue lv;
  addNonMemberVarDeclAddressorComponent(SGM, var, formalRValueType, lv);
  return lv;
}

static LValue emitLValueForNonMemberVarDecl(SILGenFunction &gen,
                                            SILLocation loc, VarDecl *var,
                                            CanType formalRValueType,
                                            AccessKind accessKind,
                                            AccessSemantics semantics) {
  LValue lv;

  switch (var->getAccessStrategy(semantics, accessKind)) {

  case AccessStrategy::DispatchToAccessor:
    llvm_unreachable("can't polymorphically access non-member variable");

  // If it's a computed variable, push a reference to the getter and setter.
  case AccessStrategy::DirectToAccessor: {
    auto typeData = getLogicalStorageTypeData(gen.SGM, formalRValueType);
    lv.add<GetterSetterComponent>(var, /*isSuper=*/false, /*direct*/ true,
                                  gen.SGM.getNonMemberVarDeclSubstitutions(var),
                                  CanType(), typeData);
    break;
  }

  case AccessStrategy::Addressor: {
    addNonMemberVarDeclAddressorComponent(gen.SGM, var, formalRValueType, lv);
    break;
  }

  case AccessStrategy::Storage: {
    // If it's a physical value (e.g. a local variable in memory), push its
    // address.
    auto address = gen.emitLValueForDecl(loc, var, formalRValueType,
                                         accessKind, semantics);
    assert(address.isLValue() &&
           "physical lvalue decl ref must evaluate to an address");
    auto typeData = getPhysicalStorageTypeData(gen.SGM, var, formalRValueType);
    lv.add<ValueComponent>(address, typeData);

    if (address.getType().is<ReferenceStorageType>())
      lv.add<OwnershipComponent>(typeData);
    break;
  }
  
  case AccessStrategy::BehaviorStorage:
    // TODO: Behaviors aren't supported for non-instance properties yet.
    llvm_unreachable("not implemented");
  }

  return lv;
}


LValue SILGenLValue::visitDiscardAssignmentExpr(DiscardAssignmentExpr *e,
                                                AccessKind accessKind) {
  LValueTypeData typeData = getValueTypeData(gen, e);

  SILValue address = gen.emitTemporaryAllocation(e, typeData.TypeOfRValue);
  address = gen.B.createMarkUninitialized(e, address,
                                          MarkUninitializedInst::Var);
  LValue lv;
  lv.add<ValueComponent>(gen.emitManagedBufferWithCleanup(address), typeData);
  return lv;
}


LValue SILGenLValue::visitDeclRefExpr(DeclRefExpr *e, AccessKind accessKind) {
  // The only non-member decl that can be an lvalue is VarDecl.
  return emitLValueForNonMemberVarDecl(gen, e, cast<VarDecl>(e->getDecl()),
                                       getSubstFormalRValueType(e),
                                       accessKind,
                                       e->getAccessSemantics());
}

LValue SILGenLValue::visitOpaqueValueExpr(OpaqueValueExpr *e,
                                          AccessKind accessKind) {
  // Handle an opaque lvalue that refers to an opened existential.
  auto known = openedExistentials.find(e);
  if (known != openedExistentials.end()) {
    // Dig the open-existential expression out of the list.
    OpenExistentialExpr *opened = known->second;
    openedExistentials.erase(known);

    // Do formal evaluation of the underlying existential lvalue.
    LValue existentialLV = visitRec(opened->getExistentialValue(), accessKind);

    ManagedValue existentialAddr
      = gen.emitAddressOfLValue(e, std::move(existentialLV), accessKind);

    // Open up the existential.
    LValue lv;
    lv.add<ValueComponent>(existentialAddr, existentialLV.getTypeData());
    lv.add<OpenOpaqueExistentialComponent>(
      cast<ArchetypeType>(opened->getOpenedArchetype()->getCanonicalType()));
    return lv;
  }

  assert(gen.OpaqueValues.count(e) && "Didn't bind OpaqueValueExpr");

  auto &entry = gen.OpaqueValues.find(e)->second;
  assert(!entry.HasBeenConsumed && "opaque value already consumed");
  entry.HasBeenConsumed = true;

  RegularLocation loc(e);
  LValue lv;
  lv.add<ValueComponent>(entry.Value.borrow(gen, loc),
                         getValueTypeData(gen, e));
  return lv;
}

LValue SILGenLValue::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e,
                                                   AccessKind accessKind) {
  gen.emitIgnoredExpr(e->getLHS());
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
    
  case AccessStrategy::BehaviorStorage:
    // We should only access the behavior storage for initialization purposes.
    assert(accessKind == AccessKind::Write);
    return AccessKind::Write;
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

  LValue lv = visitRec(e->getBase(),
                       getBaseAccessKind(var, accessKind, strategy));
  assert(lv.isValid());

  CanType substFormalRValueType = getSubstFormalRValueType(e);
  lv.addMemberVarComponent(gen, e, var, e->getMember().getSubstitutions(),
                           e->isSuper(), accessKind, e->getAccessSemantics(),
                           strategy, substFormalRValueType);
  return lv;
}

void LValue::addMemberVarComponent(SILGenFunction &gen, SILLocation loc,
                                   VarDecl *var,
                                   SubstitutionList subs,
                                   bool isSuper,
                                   AccessKind accessKind,
                                   AccessSemantics accessSemantics,
                                   AccessStrategy strategy,
                                   CanType formalRValueType) {
  CanType baseFormalType = getSubstFormalType();

  // Use the property accessors if the variable has accessors and this isn't a
  // direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    auto typeData = getLogicalStorageTypeData(gen.SGM, formalRValueType);
    add<GetterSetterComponent>(var, isSuper,
                               strategy == AccessStrategy::DirectToAccessor,
                               subs, baseFormalType, typeData);
    return;
  }

  assert(strategy == AccessStrategy::Addressor ||
         strategy == AccessStrategy::Storage ||
         strategy == AccessStrategy::BehaviorStorage);

  // Otherwise, the lvalue access is performed with a fragile element reference.
  // Find the substituted storage type.
  SILType varStorageType =
    gen.SGM.Types.getSubstitutedStorageType(var, formalRValueType);
    
  // For static variables, emit a reference to the global variable backing
  // them.
  // FIXME: This has to be dynamically looked up for classes, and
  // dynamically instantiated for generics.
  if (strategy == AccessStrategy::Storage && var->isStatic()) {
    auto baseMeta = baseFormalType->castTo<MetatypeType>()->getInstanceType();
    (void)baseMeta;
    assert(!baseMeta->is<BoundGenericType>() &&
           "generic static stored properties not implemented");

    // FIXME: this implicitly drops the earlier components, but maybe
    // we ought to evaluate them for side-effects even during the
    // formal access?
    *this = emitLValueForNonMemberVarDecl(gen, loc, var,
                                          formalRValueType,
                                          accessKind, accessSemantics);
    return;
  }

  auto typeData = getPhysicalStorageTypeData(gen.SGM, var, formalRValueType);

  // For behavior initializations, we should have set up a marking proxy that
  // replaces the access path.
  if (strategy == AccessStrategy::BehaviorStorage) {
    auto addr = gen.VarLocs.find(var);
    assert(addr != gen.VarLocs.end() && addr->second.value);
    Path.clear();
    add<ValueComponent>(ManagedValue::forUnmanaged(addr->second.value),
                        typeData);
  // For member variables, this access is done w.r.t. a base computation that
  // was already emitted.  This member is accessed off of it.
  } else if (strategy == AccessStrategy::Addressor) {
    add<AddressorComponent>(var, isSuper, /*direct*/ true, subs,
                            baseFormalType, typeData, varStorageType);
  } else if (baseFormalType->mayHaveSuperclass()) {
    add<RefElementComponent>(var, varStorageType, typeData);
  } else {
    assert(baseFormalType->getStructOrBoundGenericStruct());
    add<StructElementComponent>(var, varStorageType, typeData);
  }
  
  // If the member has weak or unowned storage, convert it away.
  if (varStorageType.is<ReferenceStorageType>()) {
    add<OwnershipComponent>(typeData);
  }
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e,
                                        AccessKind accessKind) {
  auto decl = cast<SubscriptDecl>(e->getDecl().getDecl());

  auto accessSemantics = e->getAccessSemantics();
  auto strategy = decl->getAccessStrategy(accessSemantics, accessKind);
  
  LValue lv = visitRec(e->getBase(),
                       getBaseAccessKind(decl, accessKind, strategy));
  assert(lv.isValid());

  Expr *indexExpr = e->getIndex();
  // FIXME: This admits varargs tuples, which should only be handled as part of
  // argument emission.
  RValue index = gen.emitRValue(indexExpr);

  CanType formalRValueType = getSubstFormalRValueType(e);
  lv.addMemberSubscriptComponent(gen, e, decl, e->getDecl().getSubstitutions(),
                                 e->isSuper(), accessKind, accessSemantics,
                                 strategy, formalRValueType, std::move(index),
                                 indexExpr);
  return lv;
}

void LValue::addMemberSubscriptComponent(SILGenFunction &gen, SILLocation loc,
                                         SubscriptDecl *decl,
                                         SubstitutionList subs,
                                         bool isSuper,
                                         AccessKind accessKind,
                                         AccessSemantics accessSemantics,
                                         AccessStrategy strategy,
                                         CanType formalRValueType,
                                         RValue &&indices,
                                         Expr *indexExprForDiagnostics) {
  CanType baseFormalType = getSubstFormalType();

  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    auto typeData = getLogicalStorageTypeData(gen.SGM, formalRValueType);
    add<GetterSetterComponent>(decl, isSuper,
                               strategy == AccessStrategy::DirectToAccessor,
                               subs, baseFormalType, typeData,
                               indexExprForDiagnostics, &indices);
  } else {
    assert(strategy == AccessStrategy::Addressor);
    auto typeData = getPhysicalStorageTypeData(gen.SGM, decl, formalRValueType);
    auto storageType = 
      gen.SGM.Types.getSubstitutedStorageType(decl, formalRValueType);
    add<AddressorComponent>(decl, isSuper, /*direct*/ true,
                            subs, baseFormalType, typeData, storageType,
                            indexExprForDiagnostics, &indices);
  }
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

  lv.add<TupleElementComponent>(index, typeData);
  return lv;
}

LValue SILGenLValue::visitOpenExistentialExpr(OpenExistentialExpr *e,
                                              AccessKind accessKind) {
  // If the opaque value is not an lvalue, open the existential immediately.
  if (!e->getOpaqueValue()->getType()->is<LValueType>()) {
    return gen.emitOpenExistentialExpr<LValue>(e,
                                               [&](Expr *subExpr) -> LValue {
                                                 return visitRec(subExpr,
                                                                 accessKind);
                                               });
  }

  // Record the fact that we're opening this existential. The actual
  // opening operation will occur when we see the OpaqueValueExpr.
  bool inserted = openedExistentials.insert({e->getOpaqueValue(), e}).second;
  (void)inserted;
  assert(inserted && "already have this opened existential?");

  // Visit the subexpression.
  LValue lv = visitRec(e->getSubExpr(), accessKind);

  // Sanity check that we did see the OpaqueValueExpr.
  assert(openedExistentials.count(e->getOpaqueValue()) == 0 &&
         "opened existential not removed?");
  return lv;
}

static LValueTypeData
getOptionalObjectTypeData(SILGenFunction &gen,
                          const LValueTypeData &baseTypeData) {
  EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl();
  
  return {
    baseTypeData.OrigFormalType.getAnyOptionalObjectType(),
    baseTypeData.SubstFormalType.getAnyOptionalObjectType(),
    baseTypeData.TypeOfRValue.getEnumElementType(someDecl, gen.SGM.M),
  };
}

LValue SILGenLValue::visitForceValueExpr(ForceValueExpr *e,
                                         AccessKind accessKind) {
  LValue lv = visitRec(e->getSubExpr(), accessKind);
  LValueTypeData typeData = getOptionalObjectTypeData(gen, lv.getTypeData());
  lv.add<ForceOptionalObjectComponent>(typeData);
  return lv;
}

LValue SILGenLValue::visitBindOptionalExpr(BindOptionalExpr *e,
                                           AccessKind accessKind) {
  // Do formal evaluation of the base l-value.
  LValue optLV = visitRec(e->getSubExpr(), accessKind);

  LValueTypeData optTypeData = optLV.getTypeData();
  LValueTypeData valueTypeData = getOptionalObjectTypeData(gen, optTypeData);

  // The chaining operator immediately begins a formal access to the
  // base l-value.  In concrete terms, this means we can immediately
  // evaluate the base down to an address.
  ManagedValue optAddr =
    gen.emitAddressOfLValue(e, std::move(optLV), accessKind);

  // Bind the value, branching to the destination address if there's no
  // value there.
  gen.emitBindOptional(e, optAddr, e->getDepth());

  // Project out the payload on the success branch.  We can just use a
  // naked ValueComponent here; this is effectively a separate l-value.
  ManagedValue valueAddr =
    getAddressOfOptionalValue(gen, e, optAddr, valueTypeData);
  LValue valueLV;
  valueLV.add<ValueComponent>(valueAddr, valueTypeData);
  return valueLV;
}

LValue SILGenLValue::visitInOutExpr(InOutExpr *e, AccessKind accessKind) {
  return visitRec(e->getSubExpr(), accessKind);
}

/// Emit an lvalue that refers to the given property.  This is
/// designed to work with ManagedValue 'base's that are either +0 or +1.
LValue SILGenFunction::emitPropertyLValue(SILLocation loc, ManagedValue base,
                                          CanType baseFormalType,
                                          VarDecl *ivar, AccessKind accessKind,
                                          AccessSemantics semantics) {
  SILGenLValue sgl(*this);
  LValue lv;

  SubstitutionList subs =
      base.getType().gatherAllSubstitutions(SGM.M);

  LValueTypeData baseTypeData = getValueTypeData(baseFormalType,
                                                 base.getValue());

  // Refer to 'self' as the base of the lvalue.
  lv.add<ValueComponent>(base, baseTypeData);

  auto substFormalType = base.getType().getSwiftRValueType()
    ->getTypeOfMember(F.getModule().getSwiftModule(),
                      ivar, nullptr)
    ->getCanonicalType();

  AccessStrategy strategy =
    ivar->getAccessStrategy(semantics, accessKind);

  // Use the property accessors if the variable has accessors and this
  // isn't a direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    auto typeData = getLogicalStorageTypeData(SGM, substFormalType);
    lv.add<GetterSetterComponent>(ivar, /*super*/ false,
                                  strategy == AccessStrategy::DirectToAccessor,
                                  subs, baseFormalType, typeData);
    return lv;
  }

  assert(strategy == AccessStrategy::Addressor ||
         strategy == AccessStrategy::Storage);

  // Find the substituted storage type.
  SILType varStorageType =
    SGM.Types.getSubstitutedStorageType(ivar, substFormalType);

  auto typeData = getPhysicalStorageTypeData(SGM, ivar, substFormalType);

  if (strategy == AccessStrategy::Addressor) {
    lv.add<AddressorComponent>(ivar, /*super*/ false, /*direct*/ true,
                               subs, baseFormalType, typeData, varStorageType);
  } else if (baseFormalType->hasReferenceSemantics()) {
    lv.add<RefElementComponent>(ivar, varStorageType, typeData);
  } else {
    lv.add<StructElementComponent>(ivar, varStorageType, typeData);
  }

  if (varStorageType.is<ReferenceStorageType>()) {
    auto formalRValueType =
      ivar->getDeclContext()->mapTypeIntoContext(ivar->getInterfaceType())
          ->getReferenceStorageReferent()
          ->getCanonicalType();
    auto typeData =
      getPhysicalStorageTypeData(SGM, ivar, formalRValueType);
    lv.add<OwnershipComponent>(typeData);
  }

  return lv;
}

/// Load an r-value out of the given address.
///
/// \param rvalueTL - the type lowering for the type-of-rvalue
///   of the address
/// \param isGuaranteedValid - true if the value in this address
///   is guaranteed to be valid for the duration of the current
///   evaluation (see SGFContext::AllowGuaranteedPlusZero)
ManagedValue SILGenFunction::emitLoad(SILLocation loc, SILValue addr,
                                      const TypeLowering &rvalueTL,
                                      SGFContext C, IsTake_t isTake,
                                      bool isGuaranteedValid) {
  // Get the lowering for the address type.  We can avoid a re-lookup
  // in the very common case of this being equivalent to the r-value
  // type.
  auto &addrTL =
    (addr->getType() == rvalueTL.getLoweredType().getAddressType()
       ? rvalueTL : getTypeLowering(addr->getType()));

  // Never do a +0 load together with a take.
  bool isPlusZeroOk = (isTake == IsNotTake &&
                       (isGuaranteedValid ? C.isGuaranteedPlusZeroOk()
                                          : C.isImmediatePlusZeroOk()));

  if (rvalueTL.isAddressOnly() && silConv.useLoweredAddresses()) {
    // If the client is cool with a +0 rvalue, the decl has an address-only
    // type, and there are no conversions, then we can return this as a +0
    // address RValue.
    if (isPlusZeroOk && rvalueTL.getLoweredType() == addrTL.getLoweredType())
      return ManagedValue::forUnmanaged(addr);
        
    // Copy the address-only value.
    return B.bufferForExpr(
        loc, rvalueTL.getLoweredType(), rvalueTL, C,
        [&](SILValue newAddr) {
          emitSemanticLoadInto(loc, addr, addrTL, newAddr, rvalueTL,
                               isTake, IsInitialization);
        });
  }

  // Ok, this is something loadable.  If this is a non-take access at plus zero,
  // we can perform a +0 load of the address instead of materializing a +1
  // value.
  if (isPlusZeroOk && addrTL.getLoweredType() == rvalueTL.getLoweredType()) {
    return B.createLoadBorrow(loc, ManagedValue::forUnmanaged(addr));
  }

  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = emitSemanticLoad(loc, addr, addrTL, rvalueTL, isTake);
  return emitManagedRValueWithCleanup(loadedV);
}

/// Load an r-value out of the given address.
///
/// \param rvalueTL - the type lowering for the type-of-rvalue
///   of the address
/// \param isGuaranteedValid - true if the value in this address
///   is guaranteed to be valid for the duration of the current
///   evaluation (see SGFContext::AllowGuaranteedPlusZero)
ManagedValue SILGenFunction::emitFormalAccessLoad(SILLocation loc,
                                                  SILValue addr,
                                                  const TypeLowering &rvalueTL,
                                                  SGFContext C, IsTake_t isTake,
                                                  bool isGuaranteedValid) {
  // Get the lowering for the address type.  We can avoid a re-lookup
  // in the very common case of this being equivalent to the r-value
  // type.
  auto &addrTL = (addr->getType() == rvalueTL.getLoweredType().getAddressType()
                      ? rvalueTL
                      : getTypeLowering(addr->getType()));

  // Never do a +0 load together with a take.
  bool isPlusZeroOk =
      (isTake == IsNotTake && (isGuaranteedValid ? C.isGuaranteedPlusZeroOk()
                                                 : C.isImmediatePlusZeroOk()));

  if (rvalueTL.isAddressOnly()) {
    // If the client is cool with a +0 rvalue, the decl has an address-only
    // type, and there are no conversions, then we can return this as a +0
    // address RValue.
    if (isPlusZeroOk && rvalueTL.getLoweredType() == addrTL.getLoweredType())
      return ManagedValue::forUnmanaged(addr);

    // Copy the address-only value.
    return B.formalAccessBufferForExpr(
        loc, rvalueTL.getLoweredType(), rvalueTL, C,
        [&](SILValue addressForCopy) {
          emitSemanticLoadInto(loc, addr, addrTL, addressForCopy, rvalueTL,
                               isTake, IsInitialization);
        });
  }

  // Ok, this is something loadable.  If this is a non-take access at plus zero,
  // we can perform a +0 load of the address instead of materializing a +1
  // value.
  if (isPlusZeroOk && addrTL.getLoweredType() == rvalueTL.getLoweredType()) {
    return B.createFormalAccessLoadBorrow(loc,
                                          ManagedValue::forUnmanaged(addr));
  }

  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = emitSemanticLoad(loc, addr, addrTL, rvalueTL, isTake);
  return emitFormalAccessManagedRValueWithCleanup(loc, loadedV);
}

static void emitUnloweredStoreOfCopy(SILGenBuilder &B, SILLocation loc,
                                     SILValue value, SILValue addr,
                                     IsInitialization_t isInit) {
  if (isInit) {
    B.emitStoreValueOperation(loc, value, addr, StoreOwnershipQualifier::Init);
  } else {
    B.createAssign(loc, value, addr);
  }
}

SILValue SILGenFunction::emitConversionToSemanticRValue(SILLocation loc,
                                                        SILValue src,
                                                  const TypeLowering &valueTL) {
  // Weak storage types are handled with their underlying type.
  assert(!src->getType().is<WeakStorageType>() &&
         "weak pointers are always the right optional types");

  // For @unowned(safe) types, we need to generate a strong retain and
  // strip the unowned box.
  if (auto unownedType = src->getType().getAs<UnownedStorageType>()) {
    assert(unownedType->isLoadable(ResilienceExpansion::Maximal));
    (void) unownedType;

    B.createStrongRetainUnowned(loc, src, Atomicity::Atomic);
    return B.createUnownedToRef(loc, src,
                SILType::getPrimitiveObjectType(unownedType.getReferentType()));
  }

  // For @unowned(unsafe) types, we need to strip the unmanaged box
  // and then do an (unsafe) retain.
  if (auto unmanagedType = src->getType().getAs<UnmanagedStorageType>()) {
    auto result = B.createUnmanagedToRef(loc, src,
              SILType::getPrimitiveObjectType(unmanagedType.getReferentType()));
    // SEMANTIC ARC TODO: Does this need a cleanup?
    return B.createCopyValue(loc, result);
  }

  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

ManagedValue SILGenFunction::emitConversionToSemanticRValue(
    SILLocation loc, ManagedValue src, const TypeLowering &valueTL) {
  // Weak storage types are handled with their underlying type.
  assert(!src.getType().is<WeakStorageType>() &&
         "weak pointers are always the right optional types");

  // For @unowned(safe) types, we need to generate a strong retain and
  // strip the unowned box.
  if (src.getType().is<UnownedStorageType>()) {
    return B.createCopyUnownedValue(loc, src);
  }

  // For @unowned(unsafe) types, we need to strip the unmanaged box
  // and then do an (unsafe) retain.
  if (src.getType().is<UnmanagedStorageType>()) {
    return B.createUnsafeCopyUnownedValue(loc, src);
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
  SILType storageType = src->getType();

  // For @weak types, we need to create an Optional<T>.
  // Optional<T> is currently loadable, but it probably won't be forever.
  if (storageType.is<WeakStorageType>())
    return gen.B.createLoadWeak(loc, src, isTake);

  // For @unowned(safe) types, we need to strip the unowned box.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    if (!unownedType->isLoadable(ResilienceExpansion::Maximal)) {
      return gen.B.createLoadUnowned(loc, src, isTake);
    }

    auto unownedValue =
        gen.B.emitLoadValueOperation(loc, src, LoadOwnershipQualifier::Take);
    gen.B.createStrongRetainUnowned(loc, unownedValue, Atomicity::Atomic);
    if (isTake)
      gen.B.createUnownedRelease(loc, unownedValue, Atomicity::Atomic);
    return gen.B.createUnownedToRef(
        loc, unownedValue,
        SILType::getPrimitiveObjectType(unownedType.getReferentType()));
  }

  // For @unowned(unsafe) types, we need to strip the unmanaged box.
  if (auto unmanagedType = src->getType().getAs<UnmanagedStorageType>()) {
    auto value = gen.B.createLoad(loc, src, LoadOwnershipQualifier::Trivial);
    auto result = gen.B.createUnmanagedToRef(loc, value,
            SILType::getPrimitiveObjectType(unmanagedType.getReferentType()));
    // SEMANTIC ARC TODO: Does this need a cleanup?
    return gen.B.createCopyValue(loc, result);
  }

  // NSString * must be bridged to String.
  if (storageType.getSwiftRValueType() == gen.SGM.Types.getNSStringType()) {
    auto nsstr = gen.B.createLoad(loc, src, LoadOwnershipQualifier::Copy);
    auto str = gen.emitBridgedToNativeValue(loc,
                                ManagedValue::forUnmanaged(nsstr),
                                SILFunctionTypeRepresentation::CFunctionPointer,
                                gen.SGM.Types.getStringType());
    return str.forward(gen);
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
  auto storageType = dest->getType();

  // For @weak types, we need to break down an Optional<T> and then
  // emit the storeWeak ourselves.
  if (storageType.is<WeakStorageType>()) {
    gen.B.createStoreWeak(loc, value, dest, isInit);

    // store_weak doesn't take ownership of the input, so cancel it out.
    gen.B.emitDestroyValueOperation(loc, value);
    return;
  }

  // For @unowned(safe) types, we need to enter the unowned box by
  // turning the strong retain into an unowned retain.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    // FIXME: resilience
    if (!unownedType->isLoadable(ResilienceExpansion::Maximal)) {
      gen.B.createStoreUnowned(loc, value, dest, isInit);

      // store_unowned doesn't take ownership of the input, so cancel it out.
      gen.B.emitDestroyValueOperation(loc, value);
      return;
    }

    auto unownedValue =
      gen.B.createRefToUnowned(loc, value, storageType.getObjectType());
    gen.B.createUnownedRetain(loc, unownedValue, Atomicity::Atomic);
    emitUnloweredStoreOfCopy(gen.B, loc, unownedValue, dest, isInit);
    gen.B.emitDestroyValueOperation(loc, value);
    return;
  }

  // For @unowned(unsafe) types, we need to enter the unmanaged box and
  // release the strong retain.
  if (storageType.is<UnmanagedStorageType>()) {
    auto unmanagedValue =
      gen.B.createRefToUnmanaged(loc, value, storageType.getObjectType());
    emitUnloweredStoreOfCopy(gen.B, loc, unmanagedValue, dest, isInit);
    gen.B.emitDestroyValueOperation(loc, value);
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
  assert(srcTL.getLoweredType().getAddressType() == src->getType());
  assert(rvalueTL.isLoadable() || !silConv.useLoweredAddresses());

  // Easy case: the types match.
  if (srcTL.getLoweredType() == rvalueTL.getLoweredType()) {
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
  assert(srcTL.getLoweredType().getAddressType() == src->getType());
  assert(destTL.getLoweredType().getAddressType() == dest->getType());

  // Easy case: the types match.
  if (srcTL.getLoweredType() == destTL.getLoweredType()) {
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
  assert(destTL.getLoweredType().getAddressType() == dest->getType());

  // Easy case: the types match.
  if (rvalue->getType() == destTL.getLoweredType()) {
    assert(!silConv.useLoweredAddresses()
           || (destTL.isAddressOnly() == rvalue->getType().isAddress()));
    if (rvalue->getType().isAddress()) {
      B.createCopyAddr(loc, rvalue, dest, IsTake, isInit);
    } else {
      emitUnloweredStoreOfCopy(B, loc, rvalue, dest, isInit);
    }
    return;
  }

  auto &rvalueTL = getTypeLowering(rvalue->getType());
  emitStoreOfSemanticRValue(*this, loc, rvalue, dest, rvalueTL, isInit);
}

/// Convert a semantic rvalue to a value of storage type.
SILValue SILGenFunction::emitConversionFromSemanticValue(SILLocation loc,
                                                         SILValue semanticValue,
                                                         SILType storageType) {
  auto &destTL = getTypeLowering(storageType);
  (void)destTL;
  // Easy case: the types match.
  if (semanticValue->getType() == storageType) {
    return semanticValue;
  }
  
  // @weak types are never loadable, so we don't need to handle them here.
  
  // For @unowned types, place into an unowned box.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    assert(unownedType->isLoadable(ResilienceExpansion::Maximal));
    (void) unownedType;

    SILValue unowned = B.createRefToUnowned(loc, semanticValue, storageType);
    B.createUnownedRetain(loc, unowned, Atomicity::Atomic);
    B.emitDestroyValueOperation(loc, semanticValue);
    return unowned;
  }

  // For @unmanaged types, place into an unmanaged box.
  if (storageType.is<UnmanagedStorageType>()) {
    SILValue unmanaged =
      B.createRefToUnmanaged(loc, semanticValue, storageType);
    B.emitDestroyValueOperation(loc, semanticValue);
    return unmanaged;
  }
  
  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Produce a physical address that corresponds to the given l-value
/// component.
static ManagedValue drillIntoComponent(SILGenFunction &SGF,
                                       SILLocation loc,
                                       PathComponent &&component,
                                       ManagedValue base,
                                       AccessKind accessKind) {
  ManagedValue addr;
  if (component.isPhysical()) {
    addr = std::move(component.asPhysical()).offset(SGF, loc, base, accessKind);
  } else {
    auto &lcomponent = component.asLogical();
    addr = std::move(lcomponent).getMaterialized(SGF, loc, base, accessKind);
  }

  return addr;
}

/// Find the last component of the given lvalue and derive a base
/// location for it.
static PathComponent &&drillToLastComponent(SILGenFunction &SGF,
                                            SILLocation loc,
                                            LValue &&lv,
                                            ManagedValue &addr,
                                            AccessKind accessKind) {
  assert(lv.begin() != lv.end() &&
         "lvalue must have at least one component");

  // Remember all the access kinds we needed along the path.
  SmallVector<AccessKind, 8> pathAccessKinds;
  for (auto i = lv.end(), e = lv.begin() + 1; i != e; --i) {
    pathAccessKinds.push_back(accessKind);
    accessKind = (*(i-1))->getBaseAccessKind(SGF, accessKind);
  }

  for (auto i = lv.begin(), e = lv.end() - 1; i != e; ++i) {
    addr = drillIntoComponent(SGF, loc, std::move(**i), addr, accessKind);
    accessKind = pathAccessKinds.pop_back_val();
  }

  return std::move(**(lv.end() - 1));
}

RValue SILGenFunction::emitLoadOfLValue(SILLocation loc, LValue &&src,
                                        SGFContext C, bool isGuaranteedValid) {
  // Any writebacks should be scoped to after the load.
  FormalEvaluationScope scope(*this);

  ManagedValue addr;
  PathComponent &&component =
    drillToLastComponent(*this, loc, std::move(src), addr, AccessKind::Read);

  // If the last component is physical, just drill down and load from it.
  if (component.isPhysical()) {
    addr = std::move(component.asPhysical())
             .offset(*this, loc, addr, AccessKind::Read);
    return RValue(*this, loc, src.getSubstFormalType(),
                  emitLoad(loc, addr.getValue(),
                           getTypeLowering(src.getTypeOfRValue()), C, IsNotTake,
                           isGuaranteedValid));
  }

  // If the last component is logical, just emit a get.
  return std::move(component.asLogical()).get(*this, loc, addr, C);
}

ManagedValue SILGenFunction::emitAddressOfLValue(SILLocation loc,
                                                 LValue &&src,
                                                 AccessKind accessKind) {
  ManagedValue addr;
  PathComponent &&component =
    drillToLastComponent(*this, loc, std::move(src), addr, accessKind);
  addr = drillIntoComponent(*this, loc, std::move(component), addr, accessKind);
  assert(addr.getType().isAddress() &&
         "resolving lvalue did not give an address");
  return ManagedValue::forLValue(addr.getValue());
}

void SILGenFunction::emitAssignToLValue(SILLocation loc, RValue &&src,
                                        LValue &&dest) {
  FormalEvaluationScope scope(*this);

  // Peephole: instead of materializing and then assigning into a
  // translation component, untransform the value first.
  while (dest.isLastComponentTranslation()) {
    src = std::move(dest.getLastTranslationComponent())
                 .untranslate(*this, loc, std::move(src));
    dest.dropLastTranslationComponent();
  }
  
  // Resolve all components up to the last, keeping track of value-type logical
  // properties we need to write back to.
  ManagedValue destAddr;
  PathComponent &&component =
    drillToLastComponent(*this, loc, std::move(dest), destAddr,
                         AccessKind::ReadWrite);
  
  // Write to the tail component.
  if (component.isPhysical()) {
    auto finalDestAddr =
      std::move(component.asPhysical()).offset(*this, loc, destAddr,
                                               AccessKind::Write);
    
    std::move(src).assignInto(*this, loc, finalDestAddr.getValue());
  } else {
    std::move(component.asLogical()).set(*this, loc, std::move(src), destAddr);
  }

  // The writeback scope closing will propagate the value back up through the
  // writeback chain.
}

void SILGenFunction::emitCopyLValueInto(SILLocation loc, LValue &&src,
                                        Initialization *dest) {
  auto skipPeephole = [&]{
    auto loaded = emitLoadOfLValue(loc, std::move(src), SGFContext(dest));
    if (!loaded.isInContext())
      std::move(loaded).forwardInto(*this, loc, dest);
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
        != destAddr->getType().getSwiftRValueType())
    return skipPeephole();
  
  auto srcAddr = emitAddressOfLValue(loc, std::move(src), AccessKind::Read)
                   .getUnmanagedValue();
  B.createCopyAddr(loc, srcAddr, destAddr, IsNotTake, IsInitialization);
  dest->finishInitialization(*this);
}

void SILGenFunction::emitAssignLValueToLValue(SILLocation loc,
                                              LValue &&src,
                                              LValue &&dest) {
  // Only perform the peephole if both operands are physical and there's no
  // semantic conversion necessary.
  if (!src.isPhysical() || !dest.isPhysical()) {
    RValue loaded = emitLoadOfLValue(loc, std::move(src), SGFContext());
    emitAssignToLValue(loc, std::move(loaded), std::move(dest));
    return;
  }

  auto srcAddr = emitAddressOfLValue(loc, std::move(src), AccessKind::Read)
                   .getUnmanagedValue();
  auto destAddr = emitAddressOfLValue(loc, std::move(dest), AccessKind::Write)
                    .getUnmanagedValue();

  if (srcAddr->getType() == destAddr->getType()) {
    B.createCopyAddr(loc, srcAddr, destAddr, IsNotTake, IsNotInitialization);
  } else {
    // If there's a semantic conversion necessary, do a load then assign.
    auto loaded = emitLoad(loc, srcAddr, getTypeLowering(src.getTypeOfRValue()),
                           SGFContext(),
                           IsNotTake);
    loaded.assignInto(*this, loc, destAddr);
  }
}
