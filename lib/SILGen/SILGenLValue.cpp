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
  Materialize temp;
  SmallVector<SILValue, 2> ExtraInfo;

  ~LValueWriteback() {}
  LValueWriteback(LValueWriteback&&) = default;
  LValueWriteback &operator=(LValueWriteback&&) = default;

  LValueWriteback() = default;
  LValueWriteback(SILLocation loc,
                  std::unique_ptr<LogicalPathComponent> &&comp,
                  ManagedValue base, Materialize temp,
                  ArrayRef<SILValue> extraInfo)
    : loc(loc), component(std::move(comp)), base(base), temp(temp) {
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

  void performWriteback(SILGenFunction &gen) && {
    std::move(*component).writeback(gen, loc, base, temp, ExtraInfo);
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
  assert((!WritebackStack || WritebackStack->empty()) &&
         "entries remaining on writeback stack at end of function!");
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

static LValueTypeData getStorageTypeData(SILGenFunction &gen,
                                        AbstractStorageDecl *storage,
                                        CanType substFormalType) {
  auto origFormalType = gen.SGM.Types.getAbstractionPattern(storage)
                                     .getReferenceStorageReferentType();
  return {
    origFormalType,
    substFormalType,
    gen.getLoweredType(origFormalType, substFormalType).getObjectType()
  };
}

static LValueTypeData getStorageTypeData(SILGenFunction &gen,
                                        AbstractStorageDecl *storage,
                                        Expr *lvalueExpr) {
  return getStorageTypeData(gen, storage, getSubstFormalRValueType(lvalueExpr));
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
  LValue visitOpaqueValueExpr(OpaqueValueExpr *e, AccessKind accessKind);

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
  gen.getWritebackStack().emplace_back(loc, std::move(clonedComponent), base,
                                       Materialize{temporary.getValue(),
                                                   temporary.getCleanup()},
                                       ArrayRef<SILValue>());
  return temporary.borrow();
}

void LogicalPathComponent::writeback(SILGenFunction &gen, SILLocation loc,
                                     ManagedValue base, Materialize temporary,
                                     ArrayRef<SILValue> otherInfo) && {
  assert(otherInfo.empty() && "unexpected otherInfo parameter!");
  ManagedValue mv = temporary.claim(gen, loc);
  std::move(*this).set(gen, loc, RValue(gen, loc, getSubstFormalType(), mv),
                       base);
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
    std::move(stack[index]).performWriteback(*gen);
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

/// Return the LValueTypeData for a value whose type is its own
/// lowering.
static LValueTypeData getValueTypeData(SILType type, SILModule &M) {
  return {
    AbstractionPattern(type.getSwiftRValueType()),
    type.getSwiftRValueType(),
    type.getObjectType()
  };
}
static LValueTypeData getValueTypeData(SILValue value, SILModule &M) {
  return getValueTypeData(value.getType(), M);
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

    /// The subscript index expression.  Useless
    Expr *subscriptIndexExpr;
    RValue subscripts;

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
        result.base = gen.prepareAccessorBaseArg(loc, base, accessor);

      if (subscripts)
        result.subscripts = std::move(subscripts);
      
      return result;
    }

    AccessorBasedComponent(PathComponent::KindTy kind,
                           AbstractStorageDecl *decl,
                           bool isSuper, bool isDirectAccessorUse,
                           ArrayRef<Substitution> substitutions,
                           LValueTypeData typeData,
                           Expr *subscriptIndexExpr,
                           RValue *optSubscripts)
      : Base(typeData, kind), decl(decl),
        IsSuper(isSuper), IsDirectAccessorUse(isDirectAccessorUse),
        substitutions(substitutions.begin(), substitutions.end()),
        subscriptIndexExpr(subscriptIndexExpr)
    {
      assert((optSubscripts != nullptr) == (subscriptIndexExpr != nullptr));
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
        subscripts(copied.subscripts.copy(gen, loc)) {}

    virtual SILDeclRef getAccessor(SILGenFunction &gen,
                                   AccessKind kind) const  = 0;

    AccessKind getBaseAccessKind(SILGenFunction &gen,
                                 AccessKind kind) const override {
      SILDeclRef accessor = getAccessor(gen, kind);
      auto accessorType = gen.SGM.Types.getConstantFunctionType(accessor);
      if (accessorType->getSelfParameter().isIndirectInOut()) {
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
                           LValueTypeData typeData,
                           Expr *subscriptIndexExpr = nullptr,
                           RValue *subscriptIndex = nullptr)
      : AccessorBasedComponent(GetterSetterKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               typeData, subscriptIndexExpr, subscriptIndex)
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
      // it doesn't have a materializeForSet, just materialize to a
      // temporary.
      if (accessKind == AccessKind::Read ||
          decl->getAttrs().hasAttribute<DynamicAttr>() ||
          !decl->getMaterializeForSetFunc()) {
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
                                         substitutions, getTypeData(),
                                         subscriptIndexExpr, optSubscripts);
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
      gen.getWritebackStack().emplace_back(loc, std::move(clonedComponent),
                                           base,
                                           Materialize{address,
                                                       CleanupHandle::invalid()},
                                           extraInfo);

      return ManagedValue::forLValue(address);
    }

    void writeback(SILGenFunction &gen, SILLocation loc,
                   ManagedValue base, Materialize temporary,
                   ArrayRef<SILValue> extraInfo) && override {
      // If we don't have otherInfo, we don't have to conditionalize
      // the writeback.
      if (extraInfo.empty()) {
        std::move(*this).LogicalPathComponent::writeback(gen, loc, base,
                                                         temporary, extraInfo);
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
        // because we're in conditionally-executed code.  We also need
        // to pass it indirectly.
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
          gen.B.createAddressToPointer(loc, temporary.address,
                                       SILType::getRawPointerType(ctx));

        gen.B.createApply(loc, callback, {
                            temporaryPointer,
                            callbackStorage,
                            baseAddress,
                            baseMetatype
                          });
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
                   ManagedValue base, Materialize temporary,
                   ArrayRef<SILValue> otherInfo) && override {
      gen.B.createStrongUnpin(loc, base.forward(gen));
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
                        LValueTypeData typeData, SILType substFieldType,
                        Expr *subscriptIndexExpr = nullptr,
                        RValue *subscriptIndex = nullptr)
      : AccessorBasedComponent(AddressorKind, decl, isSuper,
                               isDirectAccessorUse, substitutions,
                               typeData, subscriptIndexExpr, subscriptIndex),
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
        gen.getWritebackStack().emplace_back(loc, std::move(component),
                                             result.second, Materialize(),
                                             ArrayRef<SILValue>());
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

namespace {
  /// An abstract class for components which translate values in some way.
  class TranslationComponent : public LogicalPathComponent {
  public:
    TranslationComponent(const LValueTypeData &typeData, KindTy kind)
      : LogicalPathComponent(typeData, kind) {}

    void diagnoseWritebackConflict(LogicalPathComponent *RHS,
                                   SILLocation loc1, SILLocation loc2,
                                   SILGenFunction &gen) override {
      // no useful writeback diagnostics at this point
    }

    AccessKind getBaseAccessKind(SILGenFunction &gen,
                                 AccessKind kind) const override {
      // Always use the same access kind for the base.
      return kind;
    }
  };

  /// Remap an lvalue referencing a generic type to an lvalue of its
  /// substituted type in a concrete context.
  class OrigToSubstComponent : public TranslationComponent {
    AbstractionPattern OrigType;
    
  public:
    OrigToSubstComponent(AbstractionPattern origType,
                         CanType substFormalType,
                         SILType loweredSubstType)
      : TranslationComponent({ AbstractionPattern(substFormalType),
                               substFormalType, loweredSubstType },
                             OrigToSubstKind),
        OrigType(origType)
    {}
    
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) && override {
      // Map the value to the original abstraction level.
      ManagedValue mv = std::move(value).getAsSingleValue(gen, loc);
      mv = gen.emitSubstToOrigValue(loc, mv, OrigType, getSubstFormalType());
      // Store to the base.
      mv.assignInto(gen, loc, base.getValue());
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) && override {
      // Load the original value.
      ManagedValue baseVal = gen.emitLoad(loc, base.getValue(),
                                          gen.getTypeLowering(base.getType()),
                                          SGFContext(),
                                          IsNotTake);
      // Map the base value to its substituted representation.
      return gen.emitOrigToSubstValue(loc, baseVal,
                                      OrigType, getSubstFormalType(), c);
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
  class SubstToOrigComponent : public TranslationComponent {
  public:
    SubstToOrigComponent(AbstractionPattern origType,
                         CanType substFormalType,
                         SILType loweredSubstType)
      : TranslationComponent({ origType, substFormalType, loweredSubstType },
                             SubstToOrigKind)
    {}
    
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&value, ManagedValue base) && override {
      // Map the value to the substituted abstraction level.
      ManagedValue mv = std::move(value).getAsSingleValue(gen, loc);
      mv = gen.emitOrigToSubstValue(loc, mv, getOrigFormalType(),
                                    getSubstFormalType());
      // Store to the base.
      mv.assignInto(gen, loc, base.getValue());
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     ManagedValue base, SGFContext c) && override {
      // Load the original value.
      ManagedValue baseVal = gen.emitLoad(loc, base.getValue(),
                                          gen.getTypeLowering(base.getType()),
                                          SGFContext(),
                                          IsNotTake);
      // Map the base value to the original representation.
      return gen.emitSubstToOrigValue(loc, baseVal,
                                      getOrigFormalType(),
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
  class OwnershipComponent : public TranslationComponent {
  public:
    OwnershipComponent(LValueTypeData typeData)
      : TranslationComponent(typeData, OwnershipKind) {
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

void LValue::addOrigToSubstComponent(SILType loweredSubstType) {
  loweredSubstType = loweredSubstType.getObjectType();
  assert(getTypeOfRValue() != loweredSubstType &&
         "reabstraction component is unnecessary!");

  // Peephole away complementary reabstractons.
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

  // Peephole away complementary reabstractons.
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
    auto typeData = getValueTypeData(rv.getValue(), gen.SGM.M);
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
      = gen.SGM.Types.getEffectiveGenericParamsForContext(
                                                      var->getDeclContext()))
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
  SILType storageType = gen.getLoweredType(var->getType()).getAddressType();
  lvalue.add<AddressorComponent>(var, /*isSuper=*/ false, /*direct*/ true,
                                 getNonMemberVarDeclSubstitutions(gen, var),
                                 typeData, storageType);
}

LValue
SILGenFunction::emitLValueForAddressedNonMemberVarDecl(SILLocation loc,
                                                       VarDecl *var,
                                                       CanType formalRValueType,
                                                       AccessKind accessKind,
                                                       AccessSemantics semantics) {
  auto typeData = getStorageTypeData(*this, var, formalRValueType);
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
  auto typeData = getStorageTypeData(gen, var, formalRValueType);

  switch (var->getAccessStrategy(semantics, accessKind)) {

  case AccessStrategy::DispatchToAccessor:
    llvm_unreachable("can't polymorphically access non-member variable");

  // If it's a computed variable, push a reference to the getter and setter.
  case AccessStrategy::DirectToAccessor:
    lv.add<GetterSetterComponent>(var, /*isSuper=*/false, /*direct*/ true,
                                  getNonMemberVarDeclSubstitutions(gen, var),
                                  typeData);
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

  return std::move(lv);
}


LValue SILGenLValue::visitDiscardAssignmentExpr(DiscardAssignmentExpr *e,
                                                AccessKind accessKind) {
  SILType tempType = gen.getLoweredType(getSubstFormalRValueType(e));

  // FIXME: this is a leak?
  SILValue address = gen.emitTemporaryAllocation(e, tempType.getObjectType());
  LValue lv;
  lv.add<ValueComponent>(ManagedValue::forUnmanaged(address),
                         getValueTypeData(tempType, gen.SGM.M));
  return std::move(lv);
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
  assert(gen.OpaqueValues.count(e) && "Didn't bind OpaqueValueExpr");

  auto &entry = gen.OpaqueValues[e];
  assert((!entry.isUniquelyReferenced || !entry.hasBeenConsumed) &&
         "uniquely-referenced opaque value already consumed");
  entry.hasBeenConsumed = true;

  SILType type = gen.getLoweredType(getSubstFormalRValueType(e));
  LValue lv;
  lv.add<ValueComponent>(ManagedValue::forUnmanaged(entry.value),
                         getValueTypeData(type, gen.SGM.M));
  return std::move(lv);
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
  LValueTypeData typeData = getStorageTypeData(gen, var, e);

  // Use the property accessors if the variable has accessors and this isn't a
  // direct access to underlying storage.
  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    lv.add<GetterSetterComponent>(var, e->isSuper(),
                                  strategy == AccessStrategy::DirectToAccessor,
                                  e->getMember().getSubstitutions(),
                                  typeData);
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

    return emitLValueForNonMemberVarDecl(gen, e, var,
                                         getSubstFormalRValueType(e),
                                         accessKind, e->getAccessSemantics());
  }

  // For member variables, this access is done w.r.t. a base computation that
  // was already emitted.  This member is accessed off of it.
  if (strategy == AccessStrategy::Addressor) {
    lv.add<AddressorComponent>(var, e->isSuper(), /*direct*/ true,
                               e->getMember().getSubstitutions(),
                               typeData, varStorageType);
  } else if (e->getBase()->getType()->mayHaveSuperclass()) {
    lv.add<RefElementComponent>(var, varStorageType, typeData);
  } else {
    assert(e->getBase()->getType()->getLValueOrInOutObjectType()
                                  ->getStructOrBoundGenericStruct());
    lv.add<StructElementComponent>(var, varStorageType, typeData);
  }
  
  // If the member has weak or unowned storage, convert it away.
  if (varStorageType.is<ReferenceStorageType>()) {
    lv.add<OwnershipComponent>(typeData);
  }
  
  return lv;
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e,
                                        AccessKind accessKind) {
  auto decl = cast<SubscriptDecl>(e->getDecl().getDecl());
  auto typeData = getStorageTypeData(gen, decl, e);

  AccessStrategy strategy =
    decl->getAccessStrategy(e->getAccessSemantics(), accessKind);
  
  LValue lv = visitRec(e->getBase(),
                       getBaseAccessKind(decl, accessKind, strategy));

  Expr *indexExpr = e->getIndex();
  RValue index = gen.emitRValue(indexExpr);

  if (strategy == AccessStrategy::DirectToAccessor ||
      strategy == AccessStrategy::DispatchToAccessor) {
    lv.add<GetterSetterComponent>(decl, e->isSuper(),
                                  strategy == AccessStrategy::DirectToAccessor,
                                  e->getDecl().getSubstitutions(),
                                  typeData, indexExpr, &index);
  } else {
    assert(strategy == AccessStrategy::Addressor);
    auto storageType = 
      gen.SGM.Types.getSubstitutedStorageType(decl, e->getType());
    lv.add<AddressorComponent>(decl, e->isSuper(), /*direct*/ true,
                               e->getDecl().getSubstitutions(),
                               typeData, storageType, indexExpr, &index);
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

  lv.add<TupleElementComponent>(index, typeData);
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
  gen.emitBindOptional(e, optAddr.getUnmanagedValue(), e->getDepth());

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

/// Emit an lvalue that directly refers to the given instance variable
/// (without going through getters or setters).   This is designed to work with
/// ManagedValue 'base's that are either +0 or +1.
LValue SILGenFunction::emitDirectIVarLValue(SILLocation loc, ManagedValue base,
                                            VarDecl *ivar,
                                            AccessKind accessKind) {
  SILGenLValue sgl(*this);
  LValue lv;

  auto baseType = base.getType().getSwiftRValueType();
  LValueTypeData baseTypeData = getValueTypeData(base.getValue(), SGM.M);

  // Refer to 'self' as the base of the lvalue.
  lv.add<ValueComponent>(base, baseTypeData);

  auto substFormalType = base.getType().getSwiftRValueType()
    ->getTypeOfMember(F.getModule().getSwiftModule(),
                      ivar, nullptr)
    ->getCanonicalType();
  LValueTypeData typeData = getStorageTypeData(*this, ivar, substFormalType);

  // Find the substituted storage type.
  SILType varStorageType =
    SGM.Types.getSubstitutedStorageType(ivar, substFormalType);

  if (baseType->hasReferenceSemantics())
    lv.add<RefElementComponent>(ivar, varStorageType, typeData);
  else
    lv.add<StructElementComponent>(ivar, varStorageType, typeData);

  if (varStorageType.is<ReferenceStorageType>()) {
    auto formalRValueType =
      ivar->getType()->getRValueType()->getReferenceStorageReferent();
    auto typeData =
      getStorageTypeData(*this, ivar, formalRValueType->getCanonicalType());
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

static void emitUnloweredStoreOfCopy(SILBuilder &B, SILLocation loc,
                                     SILValue value, SILValue addr,
                                     IsInitialization_t isInit) {
  if (isInit)
    B.createStore(loc, value, addr);
  else
    B.createAssign(loc, value, addr);
}

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
                                                 ArgumentSource &&value,
                                                 SILValue dest,
                                                 const TypeLowering &optTL) {
  SILType optType = optTL.getLoweredType();
  OptionalTypeKind optionalKind;
  auto loweredPayloadTy
    = optType.getAnyOptionalObjectType(SGM.M, optionalKind);
  assert(optionalKind != OTK_None);

  // Project out the payload area.
  auto someDecl = getASTContext().getOptionalSomeDecl(optionalKind);
  auto destPayload = B.createInitEnumDataAddr(loc, dest,
                                            someDecl,
                                            loweredPayloadTy.getAddressType());
  
  CanType formalOptType = optType.getSwiftRValueType();
  auto archetype = formalOptType->getNominalOrBoundGenericNominal()
    ->getGenericParams()->getPrimaryArchetypes()[0];
  AbstractionPattern origType(archetype);

  // Emit the value into the payload area.
  TemporaryInitialization emitInto(destPayload, CleanupHandle::invalid());
  auto &payloadTL = getTypeLowering(origType, value.getSubstType());
  std::move(value).forwardInto(*this, origType,
                               &emitInto,
                               payloadTL);
  
  // Inject the tag.
  B.createInjectEnumAddr(loc, dest, someDecl);
}

void SILGenFunction::emitInjectOptionalNothingInto(SILLocation loc, 
                                                   SILValue dest,
                                                   const TypeLowering &optTL) {
  OptionalTypeKind OTK;
  optTL.getLoweredType().getSwiftRValueType()->getAnyOptionalObjectType(OTK);
  assert(OTK != OTK_None);
  
  B.createInjectEnumAddr(loc, dest, getASTContext().getOptionalNoneDecl(OTK));
}

void SILGenFunction::emitPreconditionOptionalHasValue(SILLocation loc,
                                                      SILValue addr) {
  OptionalTypeKind OTK;
  getOptionalValueType(addr.getType().getObjectType(), OTK);

  // Generate code to the optional is present, and if not abort with a message
  // (provided by the stdlib).
  SILBasicBlock *contBB = createBasicBlock();
  SILBasicBlock *failBB = createBasicBlock();

  auto NoneEnumElementDecl = getASTContext().getOptionalNoneDecl(OTK);
  B.createSwitchEnumAddr(loc, addr, /*defaultDest*/contBB,
                         { { NoneEnumElementDecl, failBB }});

  B.emitBlock(failBB);

  // Call the standard library implementation of _diagnoseUnexpectedNilOptional.
  if (auto diagnoseFailure =
        getASTContext().getDiagnoseUnexpectedNilOptional(nullptr)) {
    emitApplyOfLibraryIntrinsic(loc, diagnoseFailure, {}, {},
                                SGFContext());
  }

  B.createUnreachable(loc);
  B.clearInsertionPoint();
  B.emitBlock(contBB);
}

SILValue SILGenFunction::emitDoesOptionalHaveValue(SILLocation loc,
                                                   SILValue addr) {
  SILType optType = addr.getType().getObjectType();
  OptionalTypeKind optionalKind;
  getOptionalValueType(optType, optionalKind);

  auto boolTy = SILType::getBuiltinIntegerType(1, getASTContext());
  SILValue yes = B.createIntegerLiteral(loc, boolTy, 1);
  SILValue no = B.createIntegerLiteral(loc, boolTy, 0);
  auto someDecl = getASTContext().getOptionalSomeDecl(optionalKind);
  return B.createSelectEnumAddr(loc, addr, boolTy, no,
                                std::make_pair(someDecl, yes));
}

ManagedValue SILGenFunction::emitCheckedGetOptionalValueFrom(SILLocation loc,
                                                      ManagedValue src,
                                                      const TypeLowering &optTL,
                                                      SGFContext C) {
  SILType optType = src.getType().getObjectType();
  OptionalTypeKind optionalKind;
  CanType valueType = getOptionalValueType(optType, optionalKind);

  FuncDecl *fn = getASTContext().getGetOptionalValueDecl(nullptr, optionalKind);
  Substitution sub = getSimpleSubstitution(fn, valueType);

  // The intrinsic takes its parameter indirectly.
  if (src.getType().isObject()) {
    auto buf = emitTemporaryAllocation(loc, src.getType());
    B.createStore(loc, src.forward(*this), buf);
    src = emitManagedBufferWithCleanup(buf);
  }

  return emitApplyOfLibraryIntrinsic(loc, fn, sub, src, C);
}

ManagedValue SILGenFunction::emitUncheckedGetOptionalValueFrom(SILLocation loc,
                                                    ManagedValue addr,
                                                    const TypeLowering &optTL,
                                                    SGFContext C) {
  OptionalTypeKind OTK;
  SILType origPayloadTy = addr.getType().getAnyOptionalObjectType(SGM.M, OTK);

  auto formalOptionalTy = addr.getType().getSwiftRValueType();
  auto formalPayloadTy = formalOptionalTy
    ->getAnyOptionalObjectType()
    ->getCanonicalType();
  
  // Take the payload from the optional.
  // Cheat a bit in the +0 case—UncheckedTakeEnumData will never actually
  // invalidate an Optional enum value.
  SILValue payloadVal = B.createUncheckedTakeEnumDataAddr(loc,
                                       addr.forward(*this),
                                       getASTContext().getOptionalSomeDecl(OTK),
                                       origPayloadTy);
  
  if (optTL.isLoadable())
    payloadVal = B.createLoad(loc, payloadVal);
  ManagedValue origPayload;
  if (addr.hasCleanup())
    origPayload = emitManagedRValueWithCleanup(payloadVal);
  else
    origPayload = ManagedValue::forUnmanaged(payloadVal);
  
  // Reabstract it to the substituted form, if necessary.
  return emitOrigToSubstValue(loc, origPayload,
                              AbstractionPattern::getOpaque(),
                              formalPayloadTy, C);
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

ManagedValue SILGenFunction::emitLoadOfLValue(SILLocation loc,
                                              LValue &&src,
                                              SGFContext C) {
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
                    getTypeLowering(src.getTypeOfRValue()), C, IsNotTake);
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
