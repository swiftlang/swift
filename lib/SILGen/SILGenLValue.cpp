//===--- SILGenLValue.cpp - Constructs logical lvalues for SILGen ---------===//
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
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/DiagnosticsCommon.h"
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

/// A pending writeback.
namespace swift {
namespace Lowering {

struct LLVM_LIBRARY_VISIBILITY LValueWriteback {
  SILLocation loc;
  std::unique_ptr<LogicalPathComponent> component;
  ManagedValue base;
  ManagedValue temp;
  SmallVector<SILValue, 2> ExtraInfo;
  CleanupHandle cleanup;

  ~LValueWriteback() {}
  LValueWriteback(LValueWriteback&&) = default;
  LValueWriteback &operator=(LValueWriteback&&) = default;

  LValueWriteback() = default;
  LValueWriteback(SILLocation loc,
                  std::unique_ptr<LogicalPathComponent> &&comp,
                  ManagedValue base, ManagedValue temp,
                  ArrayRef<SILValue> extraInfo,
                  CleanupHandle cleanup)
    : loc(loc), component(std::move(comp)), base(base), temp(temp),
      cleanup(cleanup) {
    ExtraInfo.append(extraInfo.begin(), extraInfo.end());
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

  void performWriteback(SILGenFunction &gen, bool isFinal) {
    Scope S(gen.Cleanups, CleanupLocation::get(loc));
    component->writeback(gen, loc, base, temp, ExtraInfo, isFinal);
  }
};
}
}

namespace {
  class LValueWritebackCleanup : public Cleanup {
    unsigned Depth;
  public:
    LValueWritebackCleanup(unsigned depth) : Depth(depth) {}
    void emit(SILGenFunction &gen, CleanupLocation loc) override {
      gen.getWritebackStack()[Depth].performWriteback(gen, /*isFinal*/ false);
    }
  };
}

std::vector<LValueWriteback> &SILGenFunction::getWritebackStack() {
  if (!WritebackStack)
    WritebackStack = new std::vector<LValueWriteback>();

  return *WritebackStack;
}

void SILGenFunction::freeWritebackStack() {
  assert((!WritebackStack || WritebackStack->empty()) &&
         "entries remaining on writeback stack at end of function!");
  delete WritebackStack;
}

/// Push a writeback onto the current LValueWriteback stack.
static void pushWriteback(SILGenFunction &gen,
                          SILLocation loc,
                          std::unique_ptr<LogicalPathComponent> &&comp,
                          ManagedValue base, ManagedValue temp,
                          ArrayRef<SILValue> extraInfo) {
  assert(gen.InWritebackScope);

  // Push a cleanup to execute the writeback consistently.
  auto &stack = gen.getWritebackStack();
  gen.Cleanups.pushCleanup<LValueWritebackCleanup>(stack.size());
  auto cleanup = gen.Cleanups.getTopCleanup();

  stack.emplace_back(loc, std::move(comp), base, temp, extraInfo, cleanup);
}

//===----------------------------------------------------------------------===//

static CanType getSubstFormalRValueType(Expr *expr) {
  return expr->getType()->getRValueType()->getCanonicalType();
}

static LValueTypeData getStorageTypeData(SILGenModule &SGM,
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

static ManagedValue emitGetIntoTemporary(SILGenFunction &gen,
                                         SILLocation loc,
                                         ManagedValue base,
                                         LogicalPathComponent &&component) {
  // Create a temporary.
  auto temporaryInit =
    gen.emitTemporary(loc, gen.getTypeLowering(component.getTypeOfRValue()));

  // Emit a 'get' into the temporary.
  ManagedValue value =
    std::move(component).get(gen, loc, base, SGFContext(temporaryInit.get()));

  if (!value.isInContext()) {
    if (value.getType().getSwiftRValueType()
          != temporaryInit->getAddress().getType().getSwiftRValueType()) {
      value = gen.emitSubstToOrigValue(loc, value,
                                       component.getOrigFormalType(),
                                       component.getSubstFormalType());
    }

    value.forwardInto(gen, loc, temporaryInit->getAddress());
    temporaryInit->finishInitialization(gen);
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
    return emitGetIntoTemporary(gen, loc, base, std::move(*this));
  }

  assert(gen.InWritebackScope &&
         "materializing l-value for modification without writeback scope");

  // Otherwise, we need to emit a get and set.  Borrow the base for
  // the getter.
  ManagedValue getterBase = (base ? base.borrow() : ManagedValue());

  // Clone anything else about the component that we might need in the
  // writeback.
  auto clonedComponent = clone(gen, loc);

  // Emit a 'get' into a temporary.
  ManagedValue temporary = emitGetIntoTemporary(gen, loc, getterBase,
                                                std::move(*this));

  // Push a writeback for the temporary.
  pushWriteback(gen, loc, std::move(clonedComponent), base, temporary, {});
  return temporary.borrow();
}

void LogicalPathComponent::writeback(SILGenFunction &gen, SILLocation loc,
                                     ManagedValue base, ManagedValue temporary,
                                     ArrayRef<SILValue> otherInfo, bool isFinal) {
  assert(otherInfo.empty() && "unexpected otherInfo parameter!");

  // Load the value from the temporary unless the type is address-only
  // and this is the final use, in which case we can just consume the
  // value as-is.
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

void WritebackScope::popImpl() {
  // Pop the InWritebackScope bit.
  gen->InWritebackScope = wasInWritebackScope;

  // Check to see if there is anything going on here.
  
  auto &stack = gen->getWritebackStack();
  size_t depthAtPop = stack.size();
  if (depthAtPop == savedDepth) return;

  size_t prevIndex = depthAtPop;
  while (prevIndex-- > savedDepth) {
    auto index = prevIndex;

    // Deactivate the cleanup.
    gen->Cleanups.setCleanupState(stack[index].cleanup, CleanupState::Dead);

    // Attempt to diagnose problems where obvious aliasing introduces illegal
    // code.  We do a simple N^2 comparison here to detect this because it is
    // extremely unlikely more than a few writebacks are active at once.
    for (auto j = index; j > savedDepth; --j)
      stack[index].diagnoseConflict(stack[j-1], *gen);

    // Claim the address of each and then perform the writeback from the
    // temporary allocation to the source we copied from.
    //
    // This evaluates arbitrary code, so it's best to be paranoid
    // about iterators on the stack.
    stack[index].performWriteback(*gen, /*isFinal*/ true);
  }

  assert(depthAtPop == stack.size() &&
         "more writebacks left on stack during writeback scope pop?");
  stack.erase(stack.begin() + savedDepth, stack.end());
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

/// Return the LValueTypeData for a SIL value with the given AST formal type.
static LValueTypeData getValueTypeData(CanType formalType,
                                       SILValue value) {
  return {
    AbstractionPattern(formalType),
    formalType,
    value.getType().getObjectType()
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
  OptionalTypeKind otk;
  auto valueTy
    = optAddr.getType().getSwiftRValueType().getAnyOptionalObjectType(otk);
  assert(valueTy && "base was not optional?"); (void) valueTy;

  EnumElementDecl *someDecl = gen.getASTContext().getOptionalSomeDecl(otk);

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
      // Assert that the optional value is present.
      gen.emitPreconditionOptionalHasValue(loc, base.getValue());

      // Project out the payload.
      return getAddressOfOptionalValue(gen, loc, base, getTypeData());
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
      auto addr = gen.B.createOpenExistentialAddr(loc, base.getLValueAddress(),
                                           getTypeOfRValue().getAddressType());

      if (base.hasCleanup()) {
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
} // end anonymous namespace.

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
                           ArrayRef<Substitution> substitutions,
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
      auto accessorType = gen.SGM.Types.getConstantFunctionType(accessor);
      if (accessorType->getSelfParameter().isIndirectMutating()) {
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
                           ArrayRef<Substitution> substitutions,
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

      // Pass in just the setter.
      auto args =
        std::move(*this).prepareAccessorArgs(gen, loc, base, setter);
      
      return gen.emitSetAccessor(loc, setter, substitutions,
                                 std::move(args.base), IsSuper,
                                 IsDirectAccessorUse,
                                 std::move(args.subscripts),
                                 std::move(value));
    }

    ManagedValue getMaterialized(SILGenFunction &gen,
                                 SILLocation loc,
                                 ManagedValue base,
                                 AccessKind accessKind) && override {
      // If this is just for a read, or the property is dynamic, or if
      // it doesn't have a materializeForSet, or if this is a direct
      // use of something defined in a protocol extension (see
      // maybeEmitMaterializeForSetThunk), just materialize to a
      // temporary directly.
      if (accessKind == AccessKind::Read ||
          decl->getAttrs().hasAttribute<DynamicAttr>() ||
          !decl->getMaterializeForSetFunc() ||
          decl->getDeclContext()->isProtocolExtensionContext()) {
        return std::move(*this).LogicalPathComponent::getMaterialized(gen,
                                                        loc, base, accessKind);
      }

      assert(gen.InWritebackScope &&
             "materializing l-value for modification without writeback scope");

      // Allocate opaque storage for the callback to use.
      SILValue callbackStorage = gen.emitTemporaryAllocation(loc,
        SILType::getPrimitiveObjectType(
                                gen.getASTContext().TheUnsafeValueBufferType));

      // Allocate a temporary.
      SILValue buffer =
        gen.emitTemporaryAllocation(loc, getTypeOfRValue());

      // If the base is a +1 r-value, just borrow it for materializeForSet.
      // prepareAccessorArgs will copy it if necessary.
      ManagedValue borrowedBase = (base ? base.borrow() : ManagedValue());

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
          subscripts = RValue(values, type);
          borrowedSubscripts = RValue(values, type);
          optSubscripts = &borrowedSubscripts;
        }
        return new GetterSetterComponent(decl, IsSuper, IsDirectAccessorUse,
                                         substitutions, baseFormalType,
                                         getTypeData(), subscriptIndexExpr,
                                         optSubscripts);
      }());

      SILDeclRef materializeForSet =
        gen.getMaterializeForSetDeclRef(decl, IsDirectAccessorUse);

      auto args = std::move(*this).prepareAccessorArgs(gen, loc, borrowedBase,
                                                       materializeForSet);
      auto addressAndCallback =
        gen.emitMaterializeForSetAccessor(loc, materializeForSet, substitutions,
                                          std::move(args.base), IsSuper,
                                          IsDirectAccessorUse,
                                          std::move(args.subscripts),
                                          buffer, callbackStorage);

      SILValue address = addressAndCallback.first;

      // Mark a value-dependence on the base.  We do this regardless
      // of whether the base is trivial because even a trivial base
      // may be value-dependent on something non-trivial.
      if (base) {
        address = gen.B.createMarkDependence(loc, address, base.getValue());
      }

      SILValue extraInfo[] = {
        addressAndCallback.second,
        callbackStorage,
      };

      // TODO: maybe needsWriteback should be a thin function pointer
      // to which we pass the base?  That would let us use direct
      // access for stored properties with didSet.
      pushWriteback(gen, loc, std::move(clonedComponent), base,
                    ManagedValue::forUnmanaged(address), extraInfo);

      return ManagedValue::forLValue(address);
    }

    void writeback(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base, ManagedValue temporary,
                   ArrayRef<SILValue> extraInfo, bool isFinal) override {
      // If we don't have otherInfo, we don't have to conditionalize
      // the writeback.
      if (extraInfo.empty()) {
        LogicalPathComponent::writeback(gen, loc, base, temporary, extraInfo,
                                        isFinal);
        return;
      }

      // Otherwise, extraInfo holds an optional callback and the
      // callback storage.
      assert(extraInfo.size() == 2);
      SILValue optionalCallback = extraInfo[0];
      SILValue callbackStorage = extraInfo[1];

      // Mark the writeback as auto-generated so that we don't get
      // warnings if we manage to devirtualize materializeForSet.
      loc.markAutoGenerated();

      ASTContext &ctx = gen.getASTContext();

      SILBasicBlock *contBB = gen.createBasicBlock();
      SILBasicBlock *writebackBB = gen.createBasicBlock(gen.B.getInsertionBB());
      gen.B.createSwitchEnum(loc, optionalCallback, /*defaultDest*/ nullptr,
                             { { ctx.getOptionalSomeDecl(), writebackBB },
                               { ctx.getOptionalNoneDecl(), contBB } });

      // The writeback block.
      gen.B.setInsertionPoint(writebackBB); {
        FullExpr scope(gen.Cleanups, CleanupLocation::get(loc));

        auto emptyTupleTy =
          SILType::getPrimitiveObjectType(TupleType::getEmpty(ctx));

        SILType callbackSILType = gen.getLoweredType(
                  optionalCallback.getType().getSwiftRValueType()
                                            .getAnyOptionalObjectType());

        // The callback is a BB argument from the switch_enum.
        SILValue callback =
          writebackBB->createBBArg(callbackSILType);

        auto callbackType = callbackSILType.castTo<SILFunctionType>();
        SILType metatypeType = callbackType->getParameters().back().getSILType();

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
            baseAddress = gen.emitTemporaryAllocation(loc, base.getType());
            gen.B.createStore(loc, base.getValue(), baseAddress);
          }
          baseMetatype = gen.B.createMetatype(loc, metatypeType);

        // Otherwise, we have to pass something; use an empty tuple
        // and an undef metatype.
        } else {
          baseAddress = SILUndef::get(emptyTupleTy.getAddressType(), gen.SGM.M);
          baseMetatype = SILUndef::get(metatypeType, gen.SGM.M);
        }

        SILValue temporaryPointer =
          gen.B.createAddressToPointer(loc, temporary.getValue(),
                                       SILType::getRawPointerType(ctx));

        gen.B.createApply(loc, callback, {
                            temporaryPointer,
                            callbackStorage,
                            baseAddress,
                            baseMetatype
                          }, false);
      }

      // Continue.
      gen.B.emitBlock(contBB, loc);
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) && override {
      SILDeclRef getter = gen.getGetterDeclRef(decl, IsDirectAccessorUse);

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

    ManagedValue get(SILGenFunction &gen, SILLocation loc,
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
                   ManagedValue base, ManagedValue temporary,
                   ArrayRef<SILValue> otherInfo, bool isFinal) override {
      // If this is final, we can consume the owner (stored as
      // 'base').  If it isn't, we actually need to retain it, because
      // we've still got a release active.
      SILValue baseValue = (isFinal ? base.forward(gen) : base.getValue());
      if (!isFinal) gen.B.createRetainValue(loc, baseValue);

      gen.B.createStrongUnpin(loc, baseValue);
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
                        ArrayRef<Substitution> substitutions,
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
      auto args =
        std::move(*this).prepareAccessorArgs(gen, loc, base, addressor);
      auto result = gen.emitAddressorAccessor(loc, addressor, substitutions,
                                              std::move(args.base), IsSuper,
                                              IsDirectAccessorUse,
                                              std::move(args.subscripts),
                                              SubstFieldType);
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
                      ManagedValue(), {});
        return result.first;
      }
      }
      llvm_unreachable("bad addressor kind");
    }

    void print(raw_ostream &OS) const override {
      printBase(OS, "AddressorComponent");
    }
  };
} // end anonymous namespace.

ManagedValue
TranslationPathComponent::get(SILGenFunction &gen, SILLocation loc,
                              ManagedValue base, SGFContext c) && {
  // Load the original value.
  ManagedValue baseVal = gen.emitLoad(loc, base.getValue(),
                                      gen.getTypeLowering(base.getType()),
                                      SGFContext(),
                                      IsNotTake);

  // Map the base value to its substituted representation.
  return std::move(*this).translate(gen, loc, baseVal, c);
}

void TranslationPathComponent::set(SILGenFunction &gen, SILLocation loc,
                                   RValue &&value, ManagedValue base) && {
  // Map the value to the original pattern.
  ManagedValue mv = std::move(value).getAsSingleValue(gen, loc);
  mv = std::move(*this).untranslate(gen, loc, mv);

  // Store to the base.
  mv.assignInto(gen, loc, base.getValue());
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

    ManagedValue untranslate(SILGenFunction &gen, SILLocation loc,
                             ManagedValue mv, SGFContext c) && override {
      return gen.emitSubstToOrigValue(loc, mv, OrigType,
                                      getSubstFormalType(), c);
    }

    ManagedValue translate(SILGenFunction &gen, SILLocation loc,
                           ManagedValue mv, SGFContext c) && override {
      return gen.emitOrigToSubstValue(loc, mv, OrigType,
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
         << OrigType << ", "
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

    ManagedValue untranslate(SILGenFunction &gen, SILLocation loc,
                             ManagedValue mv, SGFContext c) && override {
      return gen.emitOrigToSubstValue(loc, mv, getOrigFormalType(),
                                      getSubstFormalType(), c);
    }

    ManagedValue translate(SILGenFunction &gen, SILLocation loc,
                           ManagedValue mv, SGFContext c) && override {
      return gen.emitSubstToOrigValue(loc, mv, getOrigFormalType(),
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

    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) && override {
      assert(base && "ownership component must not be root of lvalue path");
      auto &TL = gen.getTypeLowering(getTypeOfRValue());

      // Load the original value.
      ManagedValue result = gen.emitLoad(loc, base.getValue(), TL,
                                         SGFContext(), IsNotTake);
      return result;
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
} // end anonymous namespace.

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

LValue LValue::forClassReference(ManagedValue ref) {
  assert(ref.isPlusZeroRValueOrTrivial());
  assert(ref.getType().isObject());
  assert(ref.getType().getSwiftRValueType()->mayHaveSuperclass());

  CanType classType = ref.getType().getSwiftRValueType();
  LValueTypeData typeData = {
    AbstractionPattern(classType), classType, ref.getType()
  };

  LValue lv;
  lv.add<ValueComponent>(ref, typeData);
  return lv;
}

void LValue::addMemberComponent(SILGenFunction &gen, SILLocation loc,
                                AbstractStorageDecl *storage,
                                ArrayRef<Substitution> subs,
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
  LValue r = SILGenLValue(*this).visit(e, accessKind);
  // If the final component is physical with an abstraction change, introduce a
  // reabstraction component.
  if (r.isLastComponentPhysical()) {
    auto substFormalType = r.getSubstFormalType();
    auto loweredSubstType = getLoweredType(substFormalType);
    if (r.getTypeOfRValue() != loweredSubstType.getObjectType()) {
      r.addOrigToSubstComponent(loweredSubstType);
    }
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
          DRE->getDecl()->getName().str() == "self" &&
          DRE->getDecl()->isImplicit()) {
        Ctx = SGFContext::AllowGuaranteedPlusZero;
      } else if (auto *VD = dyn_cast<VarDecl>(DRE->getDecl())) {
        // All let values are guaranteed to be held alive across their lifetime,
        // and won't change once initialized.  Any loaded value is good for the
        // duration of this expression evaluation.
        if (VD->isLet())
          Ctx = SGFContext::AllowGuaranteedPlusZero;
      }
    }
    
    ManagedValue rv = gen.emitRValueAsSingleValue(e, Ctx);
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

static ArrayRef<Substitution>
getNonMemberVarDeclSubstitutions(SILGenFunction &gen, VarDecl *var) {
  ArrayRef<Substitution> substitutions;
  if (auto genericParams
      = var->getDeclContext()->getGenericParamsOfContext())
    substitutions =
        genericParams->getForwardingSubstitutions(gen.getASTContext());
  return substitutions;
}

// For now, we don't need either an AccessKind or an
// AccessSemantics, because addressors are always directly
// dispatched.
static void
addNonMemberVarDeclAddressorComponent(SILGenFunction &gen, VarDecl *var,
                                      const LValueTypeData &typeData,
                                      LValue &lvalue) {
  assert(!lvalue.isValid());
  SILType storageType = gen.getLoweredType(var->getType()).getAddressType();
  lvalue.add<AddressorComponent>(var, /*isSuper=*/ false, /*direct*/ true,
                                 getNonMemberVarDeclSubstitutions(gen, var),
                                 CanType(), typeData, storageType);
}

LValue
SILGenFunction::emitLValueForAddressedNonMemberVarDecl(SILLocation loc,
                                                       VarDecl *var,
                                                       CanType formalRValueType,
                                                       AccessKind accessKind,
                                                       AccessSemantics semantics) {
  auto typeData = getStorageTypeData(SGM, var, formalRValueType);
  LValue lv;
  addNonMemberVarDeclAddressorComponent(*this, var, typeData, lv);
  return lv;
}

static LValue emitLValueForNonMemberVarDecl(SILGenFunction &gen,
                                            SILLocation loc, VarDecl *var,
                                            CanType formalRValueType,
                                            AccessKind accessKind,
                                            AccessSemantics semantics) {
  LValue lv;
  auto typeData = getStorageTypeData(gen.SGM, var, formalRValueType);

  switch (var->getAccessStrategy(semantics, accessKind)) {

  case AccessStrategy::DispatchToAccessor:
    llvm_unreachable("can't polymorphically access non-member variable");

  // If it's a computed variable, push a reference to the getter and setter.
  case AccessStrategy::DirectToAccessor:
    lv.add<GetterSetterComponent>(var, /*isSuper=*/false, /*direct*/ true,
                                  getNonMemberVarDeclSubstitutions(gen, var),
                                  CanType(), typeData);
    break;

  case AccessStrategy::Addressor:
    addNonMemberVarDeclAddressorComponent(gen, var, typeData, lv);
    break;

  case AccessStrategy::Storage: {
    // If it's a physical value (e.g. a local variable in memory), push its
    // address.
    auto address = gen.emitLValueForDecl(loc, var, formalRValueType,
                                         accessKind, semantics);
    assert(address.isLValue() &&
           "physical lvalue decl ref must evaluate to an address");
    lv.add<ValueComponent>(address, typeData);

    if (address.getType().is<ReferenceStorageType>())
      lv.add<OwnershipComponent>(typeData);
    break;
  }
  }

  return lv;
}


LValue SILGenLValue::visitDiscardAssignmentExpr(DiscardAssignmentExpr *e,
                                                AccessKind accessKind) {
  LValueTypeData typeData = getValueTypeData(gen, e);

  SILValue address = gen.emitTemporaryAllocation(e, typeData.TypeOfRValue);
  address = gen.B.createMarkUninitialized(e, address,
                                          MarkUninitializedInst::Var);
  gen.enterDestroyCleanup(address);
  LValue lv;
  lv.add<ValueComponent>(ManagedValue::forUnmanaged(address), typeData);
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

  auto &entry = gen.OpaqueValues[e];
  assert((!entry.isConsumable || !entry.hasBeenConsumed) &&
         "consumable opaque value already consumed");
  entry.hasBeenConsumed = true;

  LValue lv;
  lv.add<ValueComponent>(ManagedValue::forUnmanaged(entry.value),
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
                                   ArrayRef<Substitution> subs,
                                   bool isSuper,
                                   AccessKind accessKind,
                                   AccessSemantics accessSemantics,
                                   AccessStrategy strategy,
                                   CanType formalRValueType) {
  LValueTypeData typeData = getStorageTypeData(gen.SGM, var, formalRValueType);
  CanType baseFormalType = getSubstFormalType();

  // Use the property accessors if the variable has accessors and this isn't a
  // direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    add<GetterSetterComponent>(var, isSuper,
                               strategy == AccessStrategy::DirectToAccessor,
                               subs, baseFormalType, typeData);
    return;
  }

  assert(strategy == AccessStrategy::Addressor ||
         strategy == AccessStrategy::Storage);

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

  // For member variables, this access is done w.r.t. a base computation that
  // was already emitted.  This member is accessed off of it.
  if (strategy == AccessStrategy::Addressor) {
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
                                         ArrayRef<Substitution> subs,
                                         bool isSuper,
                                         AccessKind accessKind,
                                         AccessSemantics accessSemantics,
                                         AccessStrategy strategy,
                                         CanType formalRValueType,
                                         RValue &&indices,
                                         Expr *indexExprForDiagnostics) {
  auto typeData = getStorageTypeData(gen.SGM, decl, formalRValueType);
  CanType baseFormalType = getSubstFormalType();

  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    add<GetterSetterComponent>(decl, isSuper,
                               strategy == AccessStrategy::DirectToAccessor,
                               subs, baseFormalType, typeData,
                               indexExprForDiagnostics, &indices);
  } else {
    assert(strategy == AccessStrategy::Addressor);
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

  ArrayRef<Substitution> subs;
  if (auto genericType = base.getType().getAs<BoundGenericType>()) {
    subs = genericType->getSubstitutions(SGM.SwiftModule, nullptr);
  }

  LValueTypeData baseTypeData = getValueTypeData(baseFormalType,
                                                 base.getValue());

  // Refer to 'self' as the base of the lvalue.
  lv.add<ValueComponent>(base, baseTypeData);

  auto substFormalType = base.getType().getSwiftRValueType()
    ->getTypeOfMember(F.getModule().getSwiftModule(),
                      ivar, nullptr)
    ->getCanonicalType();
  LValueTypeData typeData = getStorageTypeData(SGM, ivar, substFormalType);

  AccessStrategy strategy =
    ivar->getAccessStrategy(semantics, accessKind);

  // Use the property accessors if the variable has accessors and this
  // isn't a direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
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
      ivar->getType()->getRValueType()->getReferenceStorageReferent();
    auto typeData =
      getStorageTypeData(SGM, ivar, formalRValueType->getCanonicalType());
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
    (addr.getType() == rvalueTL.getLoweredType().getAddressType()
       ? rvalueTL : getTypeLowering(addr.getType()));

  // Never do a +0 load together with a take.
  bool isPlusZeroOk = (isTake == IsNotTake &&
                       (isGuaranteedValid ? C.isGuaranteedPlusZeroOk()
                                          : C.isImmediatePlusZeroOk()));

  if (rvalueTL.isAddressOnly()) {
    // If the client is cool with a +0 rvalue, the decl has an address-only
    // type, and there are no conversions, then we can return this as a +0
    // address RValue.
    if (isPlusZeroOk && rvalueTL.getLoweredType() == addrTL.getLoweredType())
      return ManagedValue::forUnmanaged(addr);
        
    // Copy the address-only value.
    SILValue copy = getBufferForExprResult(loc, rvalueTL.getLoweredType(), C);
    emitSemanticLoadInto(loc, addr, addrTL, copy, rvalueTL,
                         isTake, IsInitialization);
    return manageBufferForExprResult(copy, rvalueTL, C);
  }

  // Ok, this is something loadable.  If this is a non-take access at plus zero,
  // we can perform a +0 load of the address instead of materializing a +1
  // value.
  if (isPlusZeroOk && addrTL.getLoweredType() == rvalueTL.getLoweredType()) {
    return ManagedValue::forUnmanaged(B.createLoad(loc, addr));
  }

  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = emitSemanticLoad(loc, addr, addrTL, rvalueTL, isTake);
  return emitManagedRValueWithCleanup(loadedV, rvalueTL);
}

static void emitUnloweredStoreOfCopy(SILGenBuilder &B, SILLocation loc,
                                     SILValue value, SILValue addr,
                                     IsInitialization_t isInit) {
  if (isInit)
    B.createStore(loc, value, addr);
  else
    B.createAssign(loc, value, addr);
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
    assert(unownedType->isLoadable(ResilienceExpansion::Maximal));
    (void) unownedType;

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
    if (!unownedType->isLoadable(ResilienceExpansion::Maximal)) {
      return gen.B.createLoadUnowned(loc, src, isTake);
    }

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

  // NSString * must be bridged to String.
  if (storageType.getSwiftRValueType() == gen.SGM.Types.getNSStringType()) {
    auto nsstr = gen.B.createLoad(loc, src);
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
  auto storageType = dest.getType();

  // For @weak types, we need to break down an Optional<T> and then
  // emit the storeWeak ourselves.
  if (storageType.is<WeakStorageType>()) {
    gen.B.createStoreWeak(loc, value, dest, isInit);

    // store_weak doesn't take ownership of the input, so cancel it out.
    gen.B.emitReleaseValueAndFold(loc, value);
    return;
  }

  // For @unowned(safe) types, we need to enter the unowned box by
  // turning the strong retain into an unowned retain.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    // FIXME: resilience
    if (!unownedType->isLoadable(ResilienceExpansion::Maximal)) {
      gen.B.createStoreUnowned(loc, value, dest, isInit);

      // store_unowned doesn't take ownership of the input, so cancel it out.
      gen.B.emitStrongReleaseAndFold(loc, value);
      return;
    }

    auto unownedValue =
      gen.B.createRefToUnowned(loc, value, storageType.getObjectType());
    gen.B.createUnownedRetain(loc, unownedValue);
    emitUnloweredStoreOfCopy(gen.B, loc, unownedValue, dest, isInit);
    gen.B.emitStrongReleaseAndFold(loc, value);
    return;
  }

  // For @unowned(unsafe) types, we need to enter the unmanaged box and
  // release the strong retain.
  if (storageType.is<UnmanagedStorageType>()) {
    auto unmanagedValue =
      gen.B.createRefToUnmanaged(loc, value, storageType.getObjectType());
    emitUnloweredStoreOfCopy(gen.B, loc, unmanagedValue, dest, isInit);
    gen.B.emitStrongReleaseAndFold(loc, value);
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
    return semanticValue;
  }
  
  // @weak types are never loadable, so we don't need to handle them here.
  
  // For @unowned types, place into an unowned box.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    assert(unownedType->isLoadable(ResilienceExpansion::Maximal));
    (void) unownedType;

    SILValue unowned = B.createRefToUnowned(loc, semanticValue, storageType);
    B.createUnownedRetain(loc, unowned);
    B.emitStrongReleaseAndFold(loc, semanticValue);
    return unowned;
  }

  // For @unmanaged types, place into an unmanaged box.
  if (storageType.is<UnmanagedStorageType>()) {
    SILValue unmanaged =
      B.createRefToUnmanaged(loc, semanticValue, storageType);
    B.emitStrongReleaseAndFold(loc, semanticValue);
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

ManagedValue SILGenFunction::emitLoadOfLValue(SILLocation loc, LValue &&src,
                                              SGFContext C,
                                              bool isGuaranteedValid) {
  // Any writebacks should be scoped to after the load.
  WritebackScope scope(*this);

  ManagedValue addr;
  PathComponent &&component =
    drillToLastComponent(*this, loc, std::move(src), addr, AccessKind::Read);

  // If the last component is physical, just drill down and load from it.
  if (component.isPhysical()) {
    addr = std::move(component.asPhysical())
             .offset(*this, loc, addr, AccessKind::Read);
    return emitLoad(loc, addr.getValue(),
                    getTypeLowering(src.getTypeOfRValue()), C, IsNotTake,
                    isGuaranteedValid);
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
  WritebackScope scope(*this);

  // Peephole: instead of materializing and then assigning into a
  // translation component, untransform the value first.
  if (dest.isLastComponentTranslation()) {
    // Implode to a single value.
    auto srcFormalType = src.getType();
    ManagedValue srcValue = std::move(src).getAsSingleValue(*this, loc);

    // Repeatedly reverse translation components.
    do {
      srcValue = std::move(dest.getLastTranslationComponent())
                   .untranslate(*this, loc, srcValue);
      dest.dropLastTranslationComponent();
    } while (dest.isLastComponentTranslation());

    // Explode to an r-value.
    src = RValue(*this, loc, srcFormalType, srcValue);
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
    
    std::move(src).getAsSingleValue(*this, loc)
      .assignInto(*this, loc, finalDestAddr.getValue());
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
  
  auto srcAddr = emitAddressOfLValue(loc, std::move(src), AccessKind::Read)
                   .getUnmanagedValue();
  B.createCopyAddr(loc, srcAddr, destAddr, IsNotTake, IsInitialization);
  dest->finishInitialization(*this);
}

void SILGenFunction::emitAssignLValueToLValue(SILLocation loc,
                                              LValue &&src,
                                              LValue &&dest) {
  auto skipPeephole = [&]{
    ManagedValue loaded = emitLoadOfLValue(loc, std::move(src), SGFContext());
    emitAssignToLValue(loc, RValue(*this, loc, src.getSubstFormalType(),
                                   loaded), std::move(dest));
  };
  
  // Only perform the peephole if both operands are physical and there's no
  // semantic conversion necessary.
  if (!src.isPhysical())
    return skipPeephole();
  if (!dest.isPhysical())
    return skipPeephole();
  
  auto srcAddr = emitAddressOfLValue(loc, std::move(src), AccessKind::Read)
                   .getUnmanagedValue();
  auto destAddr = emitAddressOfLValue(loc, std::move(dest), AccessKind::Write)
                    .getUnmanagedValue();

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

//===----------------------------------------------------------------------===//
// materializeForSet emission
//===----------------------------------------------------------------------===//

namespace {

/// A helper class for emitting materializeForSet.
struct MaterializeForSetEmitter {
  SILGenModule &SGM;
  ProtocolConformance *Conformance;
  FuncDecl *Requirement;
  FuncDecl *Witness;
  AbstractStorageDecl *RequirementStorage;
  AbstractStorageDecl *WitnessStorage;
  ArrayRef<Substitution> WitnessSubs;
  CanType SubstSelfType;
  CanType SubstStorageType;
  AccessSemantics TheAccessSemantics;
  bool IsSuper;
  GenericParamList *OuterGenericParams = nullptr; // initialized in emit()

  AbstractionPattern RequirementStoragePattern;
  SILType RequirementStorageType;
  SILType WitnessStorageType;

  MaterializeForSetEmitter(SILGenModule &SGM,
                           ProtocolConformance *conformance,
                           FuncDecl *requirement,
                           FuncDecl *witness,
                           ArrayRef<Substitution> witnessSubs,
                           SILType selfType)
    : SGM(SGM),
      Conformance(conformance), Requirement(requirement), Witness(witness),
      RequirementStorage(requirement->getAccessorStorageDecl()),
      WitnessStorage(witness->getAccessorStorageDecl()),
      WitnessSubs(witnessSubs),
      RequirementStoragePattern(AbstractionPattern::getInvalid())
  {
    // Assume that we don't need to reabstract 'self'.  Right now,
    // that's always true; if we ever reabstract Optional (or other
    // nominal types) and allow "partial specialization" extensions,
    // this will break, and we'll have to do inout-translation in
    // the callback buffer.
    SubstSelfType = selfType.getSwiftRValueType();

    // Determine the formal type of the storage.
    CanType witnessIfaceType =
      WitnessStorage->getInterfaceType()->getCanonicalType();
    if (isa<SubscriptDecl>(WitnessStorage))
      witnessIfaceType = cast<FunctionType>(witnessIfaceType).getResult();
    SubstStorageType = getSubstWitnessInterfaceType(witnessIfaceType);

    // Determine the desired abstraction pattern of the storage type
    // in the requirement and the witness.
    RequirementStoragePattern =
      SGM.Types.getAbstractionPattern(RequirementStorage);
    RequirementStorageType =
      SGM.Types.getLoweredType(RequirementStoragePattern, SubstStorageType)
               .getObjectType();

    auto witnessStoragePattern =
      SGM.Types.getAbstractionPattern(WitnessStorage);
    WitnessStorageType =
      SGM.Types.getLoweredType(witnessStoragePattern, SubstStorageType)
               .getObjectType();

    // In a protocol witness thunk, we always want to use ordinary
    // access semantics.  But when we're changing for standard
    // implementations, we'll need to modify this to use direct
    // semantics.
    TheAccessSemantics = AccessSemantics::Ordinary;
    IsSuper = false;
  }

  bool shouldOpenCode() const {
    // We need to open-code if there's an abstraction difference in the
    // result address.
    if (RequirementStorageType != WitnessStorageType)
      return true;

    // We also need to open-code if the witness is defined in a
    // protocol context because IRGen won't know how to reconstruct
    // the type parameters.  (In principle, this can be done in the
    // callback storage if we need to.)
    if (Witness->getDeclContext()->isProtocolOrProtocolExtensionContext())
      return true;

    return false;
  }

  void emit(SILGenFunction &gen, ManagedValue self, SILValue resultBuffer,
            SILValue callbackBuffer, ArrayRef<ManagedValue> indices);

  SILValue emitUsingStorage(SILGenFunction &gen, SILLocation loc,
                            ManagedValue self, RValue &&indices);

  SILValue emitUsingAddressor(SILGenFunction &gen, SILLocation loc,
                              ManagedValue self, RValue &&indices,
                              SILValue callbackBuffer, SILFunction *&callback);
  SILFunction *createAddressorCallback(SILType ownerType,
                                       AddressorKind addressorKind);

  SILValue emitUsingGetterSetter(SILGenFunction &gen, SILLocation loc,
                                 ManagedValue self, RValue &&indices,
                                 SILValue resultBuffer,
                                 SILValue callbackBuffer,
                                 SILFunction *&callback); 
  SILFunction *createSetterCallback(const TypeLowering *indicesTL,
                                    CanType indicesFormalType);

  using GeneratorFn = llvm::function_ref<void(SILGenFunction &gen,
                                              SILLocation loc,
                                              SILValue valueBuffer,
                                              SILValue callbackBuffer,
                                              SILValue self)>;

  SILFunction *createCallback(GeneratorFn generator);

  RValue collectIndicesFromParameters(SILGenFunction &gen, SILLocation loc,
                                      ArrayRef<ManagedValue> sourceIndices);

  LValue buildSelfLValue(SILGenFunction &gen, SILLocation loc,
                         ManagedValue self) {
    // All of the complexity here is tied up with class types.  If the
    // substituted type isn't a reference type, then we can't have a
    // class-bounded protocol or inheritance, and the simple case just
    // works.
    AbstractionPattern selfPattern(SubstSelfType);
    if (!SubstSelfType->mayHaveSuperclass()) {
      return LValue::forAddress(self, selfPattern, SubstSelfType);
    }

    CanType witnessSelfType =
      Witness->computeInterfaceSelfType(false)->getCanonicalType();
    witnessSelfType = getSubstWitnessInterfaceType(witnessSelfType);
    if (auto selfTuple = dyn_cast<TupleType>(witnessSelfType)) {
      assert(selfTuple->getNumElements() == 1);
      witnessSelfType = selfTuple.getElementType(0);
    }
    witnessSelfType = witnessSelfType.getLValueOrInOutObjectType();

    // Eagerly loading here could cause an unnecessary
    // load+materialize in some cases, but it's not really important.
    SILValue selfValue = self.getValue();
    if (selfValue.getType().isAddress()) {
      selfValue = gen.B.createLoad(loc, selfValue);
    }

    // Do a derived-to-base conversion if necessary.
    if (witnessSelfType != SubstSelfType) {
      auto selfSILType = SILType::getPrimitiveObjectType(witnessSelfType);
      selfValue = gen.B.createUpcast(loc, selfValue, selfSILType);
    }

    // Recreate as a borrowed value.
    self = ManagedValue::forUnmanaged(selfValue);
    return LValue::forClassReference(self);
  }

  LValue buildLValue(SILGenFunction &gen, SILLocation loc,
                     ManagedValue self, RValue &&indices,
                     AccessKind accessKind) {
    // Begin with the 'self' value.
    LValue lv = buildSelfLValue(gen, loc, self);

    auto strategy =
      WitnessStorage->getAccessStrategy(TheAccessSemantics, accessKind);

    // Drill down to the member storage.
    lv.addMemberComponent(gen, loc, WitnessStorage, WitnessSubs, IsSuper,
                          accessKind, TheAccessSemantics, strategy,
                          SubstStorageType, std::move(indices));
    assert(lv.getTypeOfRValue().getObjectType() == WitnessStorageType);

    // Reabstract back to the requirement pattern.
    if (RequirementStorageType != WitnessStorageType) {
      SILType substTy = SGM.getLoweredType(SubstStorageType).getObjectType();

      // Translate to the formal type...
      if (WitnessStorageType != substTy)
        lv.addOrigToSubstComponent(substTy);

      // ...then back to the requirement type.
      if (substTy != RequirementStorageType)
        lv.addSubstToOrigComponent(RequirementStoragePattern,
                                   RequirementStorageType);
    }

    return lv;
  }

  /// Given part of the witness's interface type, produce its
  /// substitution according to the witness substitutions.
  CanType getSubstWitnessInterfaceType(CanType type) {
    return SubstSelfType->getTypeOfMember(SGM.SwiftModule, type,
                                          WitnessStorage->getDeclContext())
                        ->getCanonicalType();
  }

};

} // end anonymous namespace

void MaterializeForSetEmitter::emit(SILGenFunction &gen, ManagedValue self,
                                    SILValue resultBuffer,
                                    SILValue callbackBuffer,
                                    ArrayRef<ManagedValue> indices) {
  SILLocation loc = Witness;
  loc.markAutoGenerated();

  OuterGenericParams = gen.F.getContextGenericParams();

  // If there's an abstraction difference, we always need to use the
  // get/set pattern.
  AccessStrategy strategy;
  if (RequirementStorageType != WitnessStorageType) {
    strategy = AccessStrategy::DispatchToAccessor;
  } else {
    strategy = WitnessStorage->getAccessStrategy(TheAccessSemantics,
                                                 AccessKind::ReadWrite);
  }

  // Handle the indices.
  RValue indicesRV;
  if (isa<SubscriptDecl>(WitnessStorage)) {
    indicesRV = collectIndicesFromParameters(gen, loc, indices);
  } else {
    assert(indices.empty() && "indices for a non-subscript?");
  }

  // As above, assume that we don't need to reabstract 'self'.

  // Choose the right implementation.
  SILValue address;
  SILFunction *callbackFn = nullptr;
  switch (strategy) {
  case AccessStrategy::Storage:
    address = emitUsingStorage(gen, loc, self, std::move(indicesRV));
    break;

  case AccessStrategy::Addressor:
    address = emitUsingAddressor(gen, loc, self, std::move(indicesRV),
                                 callbackBuffer, callbackFn);
    break;

  case AccessStrategy::DirectToAccessor:
  case AccessStrategy::DispatchToAccessor:
    address = emitUsingGetterSetter(gen, loc, self, std::move(indicesRV),
                                    resultBuffer, callbackBuffer, callbackFn);
    break;
  }

  // Return the address as a Builtin.RawPointer.
  SILType rawPointerTy = SILType::getRawPointerType(gen.getASTContext());
  address = gen.B.createAddressToPointer(loc, address, rawPointerTy);

  SILType resultTupleTy = gen.F.mapTypeIntoContext(
                 gen.F.getLoweredFunctionType()->getResult().getSILType());
  SILType optCallbackTy = resultTupleTy.getTupleElementType(1);

  // Form the callback.
  SILValue callback;
  if (callbackFn) {
    // Make a reference to the function.
    callback = gen.B.createFunctionRef(loc, callbackFn);

    // If it's polymorphic, cast to RawPointer and then back to the
    // right monomorphic type.  The safety of this cast relies on some
    // assumptions about what exactly IRGen can reconstruct from the
    // callback's thick type argument.
    if (callbackFn->getLoweredFunctionType()->isPolymorphic()) {
      callback = gen.B.createThinFunctionToPointer(loc, callback, rawPointerTy);

      OptionalTypeKind optKind;
      auto callbackTy = optCallbackTy.getAnyOptionalObjectType(SGM.M, optKind);
      callback = gen.B.createPointerToThinFunction(loc, callback, callbackTy);
    }

    callback = gen.B.createOptionalSome(loc, callback, optCallbackTy);
  } else {
    callback = gen.B.createOptionalNone(loc, optCallbackTy);
  }

  // Form the result and return.
  auto result = gen.B.createTuple(loc, resultTupleTy, { address, callback });
  gen.Cleanups.emitCleanupsForReturn(CleanupLocation::get(loc));
  gen.B.createReturn(loc, result);
}

/// Recursively walk into the given formal index type, expanding tuples,
/// in order to 
static void translateIndices(SILGenFunction &gen, SILLocation loc,
                             AbstractionPattern pattern, CanType formalType,
                             ArrayRef<ManagedValue> &sourceIndices,
                             RValue &result) {
  // Expand if the pattern was a tuple.
  if (pattern.isTuple()) {
    auto formalTupleType = cast<TupleType>(formalType);
    for (auto i : indices(formalTupleType.getElementTypes())) {
      translateIndices(gen, loc, pattern.getTupleElementType(i),
                       formalTupleType.getElementType(i),
                       sourceIndices, result);
    }
    return;
  }

  assert(!sourceIndices.empty() && "ran out of elements in index!");
  ManagedValue value = sourceIndices.front();
  sourceIndices = sourceIndices.slice(1);

  // We're going to build an RValue here, so make sure we translate
  // indirect arguments to be scalar if we have a loadable type.
  if (value.getType().isAddress()) {
    auto &valueTL = gen.getTypeLowering(value.getType());
    if (!valueTL.isAddressOnly()) {
      value = gen.emitLoad(loc, value.forward(gen), valueTL,
                           SGFContext(), IsTake);
    }
  }

  // Reabstract the subscripts from the requirement pattern to the
  // formal type.
  value = gen.emitOrigToSubstValue(loc, value, pattern, formalType);

  // Invoking the accessor will expect a value of the formal type, so
  // don't reabstract to that here.

  // Add that to the result, further expanding if necessary.
  result.addElement(gen, value, formalType, loc);
}

RValue MaterializeForSetEmitter::
collectIndicesFromParameters(SILGenFunction &gen, SILLocation loc,
                             ArrayRef<ManagedValue> sourceIndices) {
  auto witnessSubscript = cast<SubscriptDecl>(WitnessStorage);
  CanType witnessIndicesType =
    witnessSubscript->getIndicesInterfaceType()->getCanonicalType();
  CanType substIndicesType =
    getSubstWitnessInterfaceType(witnessIndicesType);

  auto reqSubscript = cast<SubscriptDecl>(RequirementStorage);
  auto pattern = SGM.Types.getIndicesAbstractionPattern(reqSubscript);

  RValue result(substIndicesType);

  // Translate and reabstract the index values by recursively walking
  // the abstracted index type.
  SmallVector<ManagedValue, 4> translatedIndices;
  translateIndices(gen, loc, pattern, substIndicesType,
                   sourceIndices, result);
  assert(sourceIndices.empty() && "index value not claimed!");

  return result;
}

static AnyFunctionType *getMaterializeForSetCallbackType(ASTContext &ctx,
                                                         Type selfType,
                                            GenericParamList *genericParams) {
  //        inout storage: Builtin.ValueBuffer,
  //        inout self: Self,
  //        @thick selfType: Self.Type) -> ()
  TupleTypeElt params[] = {
    ctx.TheRawPointerType,
    InOutType::get(ctx.TheUnsafeValueBufferType),
    InOutType::get(selfType),
    MetatypeType::get(selfType, MetatypeRepresentation::Thick)
  };
  Type input = TupleType::get(params, ctx);
  Type result = TupleType::getEmpty(ctx);
  FunctionType::ExtInfo extInfo = FunctionType::ExtInfo()
                     .withRepresentation(FunctionType::Representation::Thin);

  if (genericParams) {
    return PolymorphicFunctionType::get(input, result, genericParams, extInfo);
  } else {
    return FunctionType::get(input, result, extInfo);
  }
}

static Type getSelfTypeForCallbackDeclaration(FuncDecl *witness) {
  // We're intentionally using non-interface types here: we want
  // something specified in terms of the witness's archetypes, because
  // we're going to build the closure as if it were contextually
  // within the witness.
  auto type = witness->getType()->castTo<AnyFunctionType>()->getInput();
  if (auto tuple = type->getAs<TupleType>()) {
    assert(tuple->getNumElements() == 1);
    type = tuple->getElementType(0);
  }
  return type->getLValueOrInOutObjectType();
}

SILFunction *MaterializeForSetEmitter::createCallback(GeneratorFn generator) {
  auto &ctx = SGM.getASTContext();
 
  // Mangle this as if it were a conformance thunk for a closure
  // within the witness.
  std::string name;
  {
    ClosureExpr closure(/*patterns*/ nullptr,
                        /*throws*/ SourceLoc(),
                        /*arrow*/ SourceLoc(),
                        /*in*/ SourceLoc(),
                        /*result*/ TypeLoc(),
                        /*discriminator*/ 0,
                        /*context*/ Witness);
    closure.setType(getMaterializeForSetCallbackType(ctx,
                                 getSelfTypeForCallbackDeclaration(Witness),
                                                     nullptr));
    closure.getCaptureInfo().setGenericParamCaptures(true);

    Mangle::Mangler mangler;
    mangler.append("_TTW");
    mangler.mangleProtocolConformance(Conformance);
    mangler.mangleClosureEntity(&closure, /*uncurryLevel=*/1);
    name = mangler.finalize();
  }

  // Create the SILFunctionType for the callback.
  CanType selfIfaceType = Conformance->getInterfaceType()->getCanonicalType();
  SILParameterInfo params[] = {
    { ctx.TheRawPointerType, ParameterConvention::Direct_Unowned },
    { ctx.TheUnsafeValueBufferType, ParameterConvention::Indirect_Inout },
    { selfIfaceType, ParameterConvention::Indirect_Inout },
    { CanMetatypeType::get(selfIfaceType, MetatypeRepresentation::Thick),
      ParameterConvention::Direct_Unowned },
  };
  SILResultInfo result = {
    TupleType::getEmpty(ctx), ResultConvention::Unowned
  };
  auto extInfo = 
    SILFunctionType::ExtInfo()
      .withRepresentation(SILFunctionTypeRepresentation::Thin);

  GenericSignature *signature = Conformance->getGenericSignature();
  if (signature) signature = signature->getCanonicalSignature();

  auto callbackType = SILFunctionType::get(signature, extInfo,
                                /*callee*/ ParameterConvention::Direct_Unowned,
                                           params, result, None, ctx);

  auto linkage = SGM.Types.getLinkageForProtocolConformance(
                       Conformance->getRootNormalConformance(), ForDefinition);
  auto callback =
    SGM.M.getOrCreateFunction(Witness, name, linkage, callbackType,
                              IsBare, IsTransparent, IsNotFragile);

  callback->setContextGenericParams(Conformance->getGenericParams());
  callback->setDebugScope(new (SGM.M) SILDebugScope(Witness, *callback));

  PrettyStackTraceSILFunction X("silgen materializeForSet callback", callback);
  {
    SILGenFunction gen(SGM, *callback);

    auto makeParam = [&](unsigned index) -> SILArgument* {
      SILType type = gen.F.mapTypeIntoContext(params[index].getSILType());
      return new (SGM.M) SILArgument(gen.F.begin(), type);
    };

    // Add arguments for all the parameters.
    auto valueBuffer = makeParam(0);
    auto storageBuffer = makeParam(1);
    auto self = makeParam(2);
    (void) makeParam(3);

    SILLocation loc = Witness;
    loc.markAutoGenerated();

    // Call the generator function we were provided.
    {
      LexicalScope scope(gen.Cleanups, gen, CleanupLocation::get(loc));
      generator(gen, loc, valueBuffer, storageBuffer, self);
    }

    // Return void.
    auto result = gen.emitEmptyTuple(loc);
    gen.B.createReturn(loc, result);
  }

  callback->verify();
  return callback;
}

/// Emit a materializeForSet operation that projects storage, assuming
/// that no cleanups or callbacks are required.
SILValue MaterializeForSetEmitter::emitUsingStorage(SILGenFunction &gen,
                                                    SILLocation loc,
                                                    ManagedValue self,
                                                    RValue &&indices) {
  LValue lvalue = buildLValue(gen, loc, self, std::move(indices),
                              AccessKind::ReadWrite);
  ManagedValue address =
    gen.emitAddressOfLValue(loc, std::move(lvalue), AccessKind::ReadWrite);
  return address.getUnmanagedValue();
}

/// Emit a materializeForSet operation that calls a mutable addressor.
///
/// If it's not an unsafe addressor, this uses a callback function to
/// write the l-value back.
SILValue MaterializeForSetEmitter::emitUsingAddressor(SILGenFunction &gen,
                                                      SILLocation loc,
                                                      ManagedValue self,
                                                      RValue &&indices,
                                                      SILValue callbackBuffer,
                                                      SILFunction *&callback) {
  bool isDirect = (TheAccessSemantics != AccessSemantics::Ordinary);

  // Call the mutable addressor.
  auto addressor = gen.getAddressorDeclRef(WitnessStorage,
                                           AccessKind::ReadWrite,
                                           isDirect);
  RValue selfRV(gen, loc, SubstSelfType, self);
  auto result = gen.emitAddressorAccessor(loc, addressor, WitnessSubs,
                                          { loc, std::move(selfRV) },
                                          IsSuper, isDirect,
                                          std::move(indices),
                                          WitnessStorageType);
  SILValue address = result.first.getUnmanagedValue();

  AddressorKind addressorKind =
    WitnessStorage->getMutableAddressor()->getAddressorKind();
  ManagedValue owner = result.second;
  if (!owner) {
    assert(addressorKind == AddressorKind::Unsafe);
  } else {
    SILValue allocatedCallbackBuffer =
      gen.B.createAllocValueBuffer(loc, owner.getType(), callbackBuffer);
    gen.B.createStore(loc, owner.forward(gen), allocatedCallbackBuffer);

    callback = createAddressorCallback(owner.getType(), addressorKind);
  }

  return address;
}

/// Emit a materializeForSet callback to clean up after an addressor
/// with an owner result.
SILFunction *
MaterializeForSetEmitter::createAddressorCallback(SILType ownerType,
                                                  AddressorKind addressorKind) {
  return createCallback([&](SILGenFunction &gen, SILLocation loc,
                            SILValue resultBuffer, SILValue callbackStorage,
                            SILValue self) {
    auto ownerAddress =
      gen.B.createProjectValueBuffer(loc, ownerType, callbackStorage);
    auto owner = gen.B.createLoad(loc, ownerAddress);

    switch (addressorKind) {
    case AddressorKind::NotAddressor:
    case AddressorKind::Unsafe:
      llvm_unreachable("owner with unexpected addressor kind");

    case AddressorKind::Owning:
    case AddressorKind::NativeOwning:
      gen.B.createStrongRelease(loc, owner);
      return;

    case AddressorKind::NativePinning:
      gen.B.createStrongUnpin(loc, owner);
      return;
    }
    llvm_unreachable("bad addressor kind");
  });
}

/// Emit a materializeForSet operation that simply loads the l-value
/// into the result buffer.  This operation creates a callback to write
/// the l-value back.
SILValue
MaterializeForSetEmitter::emitUsingGetterSetter(SILGenFunction &gen,
                                                SILLocation loc,
                                                ManagedValue self,
                                                RValue &&indices,
                                                SILValue resultBuffer,
                                                SILValue callbackBuffer,
                                                SILFunction *&callback) {
  // Copy the indices into the callback storage.
  const TypeLowering *indicesTL = nullptr;
  CleanupHandle indicesCleanup = CleanupHandle::invalid();
  CanType indicesFormalType;
  if (isa<SubscriptDecl>(WitnessStorage)) {
    indicesFormalType = indices.getType();
    indicesTL = &gen.getTypeLowering(indicesFormalType);
    SILValue allocatedCallbackBuffer =
      gen.B.createAllocValueBuffer(loc, indicesTL->getLoweredType(),
                                   callbackBuffer);

    // Emit into the buffer.
    auto init = gen.useBufferAsTemporary(loc, allocatedCallbackBuffer,
                                         *indicesTL);
    indicesCleanup = init->getInitializedCleanup();

    indices.copyInto(gen, init.get(), loc);
  }

  // Set up the result buffer.
  resultBuffer =
    gen.B.createPointerToAddress(loc, resultBuffer,
                                 RequirementStorageType.getAddressType());
  TemporaryInitialization init(resultBuffer, CleanupHandle::invalid());

  // Evaluate the getter into the result buffer.
  LValue lv = buildLValue(gen, loc, self, std::move(indices), AccessKind::Read);
  ManagedValue result = gen.emitLoadOfLValue(loc, std::move(lv),
                                             SGFContext(&init));
  if (!result.isInContext()) {
    result.forwardInto(gen, loc, resultBuffer);
  }

  // Forward the cleanup on the saved indices.
  if (indicesCleanup.isValid()) {
    gen.Cleanups.setCleanupState(indicesCleanup, CleanupState::Dead);
  }

  callback = createSetterCallback(indicesTL, indicesFormalType);
  return resultBuffer;
}

namespace {
  class DeallocateValueBuffer : public Cleanup {
    SILValue Buffer;
    SILType ValueType;
  public:
    DeallocateValueBuffer(SILType valueType, SILValue buffer)
      : Buffer(buffer), ValueType(valueType) {}
    void emit(SILGenFunction &gen, CleanupLocation loc) override {
      gen.B.createDeallocValueBuffer(loc, ValueType, Buffer);
    }
  }; 
}

/// Emit a materializeForSet callback that stores the value from the
/// result buffer back into the l-value.
SILFunction *
MaterializeForSetEmitter::createSetterCallback(const TypeLowering *indicesTL,
                                               CanType indicesFormalType) {
  return createCallback([&](SILGenFunction &gen, SILLocation loc,
                            SILValue value, SILValue callbackBuffer,
                            SILValue self) {
    // If this is a subscript, we need to handle the indices in the
    // callback storage.
    RValue indices;
    if (indicesTL) {
      assert(isa<SubscriptDecl>(WitnessStorage));
      SILType indicesTy = indicesTL->getLoweredType();

      // Enter a cleanup to deallocate the callback storage.
      gen.Cleanups.pushCleanup<DeallocateValueBuffer>(indicesTy,
                                                      callbackBuffer);

      // Project the value out, loading if necessary, and take
      // ownership of it.
      SILValue indicesV =
        gen.B.createProjectValueBuffer(loc, indicesTy, callbackBuffer);
      if (indicesTL->isLoadable())
        indicesV = gen.B.createLoad(loc, indicesV);
      ManagedValue mIndices =
        gen.emitManagedRValueWithCleanup(indicesV, *indicesTL);

      // Explode as an r-value.
      indices = RValue(gen, loc, indicesFormalType, mIndices);
    }

    // The callback gets the address of 'self' at +0.
    ManagedValue mSelf = ManagedValue::forLValue(self);

    // That's enough to build the l-value.
    LValue lvalue = buildLValue(gen, loc, mSelf, std::move(indices),
                                AccessKind::Write);

    // The callback gets the value at +1.
    auto &valueTL = gen.getTypeLowering(lvalue.getTypeOfRValue());
    value = gen.B.createPointerToAddress(loc, value,
                                   valueTL.getLoweredType().getAddressType());
    if (valueTL.isLoadable())
      value = gen.B.createLoad(loc, value);
    ManagedValue mValue = gen.emitManagedRValueWithCleanup(value, valueTL);
    RValue rvalue(gen, loc, lvalue.getSubstFormalType(), mValue);

    // Finally, call the setter.
    gen.emitAssignToLValue(loc, std::move(rvalue), std::move(lvalue));
  });
}

/// Emit an open-coded protocol-witness thunk for materializeForSet if
/// delegating to the standard implementation isn't good enough.
///
/// materializeForSet sometimes needs to be open-coded because of the
/// thin callback function, which is dependent but cannot be reabstracted.
///
/// - In a protocol extension, the callback doesn't know how to capture
///   or reconstruct the generic conformance information.
///
/// - The abstraction pattern of the variable from the witness may
///   differ from the abstraction pattern of the protocol, likely forcing
///   a completely different access pattern (e.g. to write back a
///   reabstracted value instead of modifying it in-place).
///
/// \return true if special code was emitted
bool SILGenFunction::
maybeEmitMaterializeForSetThunk(ProtocolConformance *conformance,
                                FuncDecl *requirement, FuncDecl *witness,
                                ArrayRef<Substitution> witnessSubs,
                                ArrayRef<ManagedValue> origParams) {
  // The formal type of materializeForSet is:
  //
  // (self: Self) -> (temporary: Builtin.RawPointer,
  //                  inout storage: Builtin.ValueBuffer,
  //                  indices...)
  //              -> (address: Builtin.RawPointer,
  //                  callback: (@thin (address: Builtin.RawPointer,
  //                                    inout storage: Builtin.ValueBuffer,
  //                                    inout self: Self,
  //                                    @thick selfType: Self.Type) -> ())?)

  // Break apart the parameters.  self comes last, the result buffer
  // comes first, the callback storage buffer comes second, and the
  // rest are indices.
  ManagedValue self = origParams.back();
  SILValue resultBuffer = origParams[0].getUnmanagedValue();
  SILValue callbackBuffer = origParams[1].getUnmanagedValue();
  ArrayRef<ManagedValue> indices = origParams.slice(2).drop_back();

  MaterializeForSetEmitter emitter(SGM, conformance, requirement, witness,
                                   witnessSubs, self.getType());

  if (!emitter.shouldOpenCode())
    return false;

  emitter.emit(*this, self, resultBuffer, callbackBuffer, indices);
  return true;
}
