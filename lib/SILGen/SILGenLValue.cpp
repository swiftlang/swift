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
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "LValue.h"
#include "RValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"
#include "ASTVisitor.h"

using namespace swift;
using namespace Lowering;

/// SILGenLValue - An ASTVisitor for building logical lvalues.
class LLVM_LIBRARY_VISIBILITY SILGenLValue
  : public Lowering::ExprVisitor<SILGenLValue, LValue>
{
public:
  SILGenFunction &gen;
  SILGenLValue(SILGenFunction &gen) : gen(gen) {}
  
  LValue visitRec(Expr *e);
  
  /// Dummy handler to log unimplemented nodes.
  LValue visitExpr(Expr *e);

  // Nodes that form the root of lvalue paths

  LValue visitDeclRefExpr(DeclRefExpr *e);
  LValue visitSuperRefExpr(SuperRefExpr *e);
  LValue visitMaterializeExpr(MaterializeExpr *e);

  // Nodes that make up components of lvalue paths
  
  LValue visitMemberRefExpr(MemberRefExpr *e);
  LValue visitSubscriptExpr(SubscriptExpr *e);
  LValue visitTupleElementExpr(TupleElementExpr *e);
  
  // Expressions that wrap lvalues
  
  LValue visitAddressOfExpr(AddressOfExpr *e);
  LValue visitParenExpr(ParenExpr *e);
  LValue visitRequalifyExpr(RequalifyExpr *e); // FIXME kill lvalue qualifiers
  LValue visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e);
};

Materialize LogicalPathComponent::getMaterialized(SILGenFunction &gen,
                                                  SILLocation loc,
                                                  SILValue base) const {
  ManagedValue value = get(gen, loc, base, SGFContext());
  Materialize temp = gen.emitMaterialize(loc, value);
  if (isSettable())
    gen.pushWritebackIfInScope(loc, *this, base, temp);
  return temp;
}

void
SILGenFunction::pushWritebackIfInScope(SILLocation loc,
                                       const LogicalPathComponent &component,
                                       SILValue base,
                                       Materialize temp) {
  if (InWritebackScope) {
    WritebackStack.emplace_back(loc, component.clone(*this, loc), base, temp);
  }
}

WritebackScope::WritebackScope(SILGenFunction &gen)
  : gen(&gen), wasInWritebackScope(gen.InWritebackScope),
    savedDepth(gen.WritebackStack.size())
{
  gen.InWritebackScope = true;
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
    gen.Cleanups.setCleanupState(valueCleanup, CleanupState::Dead);
  return gen.emitLoad(loc, address, addressTL, SGFContext(), IsTake);
}

WritebackScope::~WritebackScope() {
  if (!gen)
    return;
  
  gen->InWritebackScope = wasInWritebackScope;
  auto i = gen->WritebackStack.end(),
       deepest = gen->
  WritebackStack.begin() + savedDepth;
  while (i-- > deepest) {
    ManagedValue mv = i->temp.claim(*gen, i->loc);
    i->component->set(*gen, i->loc, RValue(*gen, mv, i->loc), i->base);
  }
  
  gen->WritebackStack.erase(deepest, gen->WritebackStack.end());
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

SILGenFunction::Writeback::~Writeback() {}
SILGenFunction::Writeback::Writeback(SILLocation loc,
                                   std::unique_ptr<LogicalPathComponent> &&comp,
                                   SILValue base, Materialize temp)
  : loc(loc), component(std::move(comp)), base(base), temp(temp)
{
}

LValue SILGenFunction::emitLValue(Expr *e) {
  return SILGenLValue(*this).visit(e);
}

RValue SILGenFunction::emitLValueAsRValue(Expr *e) {
  LValue lv = emitLValue(e);
  return RValue(*this, emitAddressOfLValue(e, lv), e);
}

void PathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}

namespace {
  class AddressComponent : public PhysicalPathComponent {
    SILValue address;
  public:
    AddressComponent(SILValue address, SILType typeOfRValue)
      : PhysicalPathComponent(typeOfRValue), address(address) {
      assert(address.getType().isAddress() &&
             "var component value must be an address");
    }
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                    SILValue base) const override {
      assert(!base && "var component must be root of lvalue path");
      return address;
    }
  };
  
  class RefElementComponent : public PhysicalPathComponent {
    VarDecl *Field;
    SILType SubstFieldType;
  public:
    RefElementComponent(VarDecl *field, SILType substFieldType,
                        SILType typeOfRValue)
      : PhysicalPathComponent(typeOfRValue),
        Field(field), SubstFieldType(substFieldType) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc, SILValue base)
      const override
    {
      assert(base.getType().isObject() &&
             "base for ref element component must be an object");
      assert(base.getType().hasReferenceSemantics() &&
             "base for ref element component must be a reference type");
      return gen.B.createRefElementAddr(loc, base, Field, SubstFieldType);
    }
  };
  
  class TupleElementComponent : public PhysicalPathComponent {
    unsigned ElementIndex;
  public:
    TupleElementComponent(unsigned elementIndex, SILType typeOfRValue)
      : PhysicalPathComponent(typeOfRValue), ElementIndex(elementIndex) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                    SILValue base) const override {
      assert(base && "invalid value for element base");
      SILType baseType = base.getType();
      (void)baseType;
      assert(baseType.isAddress() &&
             "base for element component must be an address");
      assert(!baseType.hasReferenceSemantics() &&
             "can't get element from address of ref type");
      return gen.B.createTupleElementAddr(loc, base, ElementIndex,
                                          getTypeOfRValue().getAddressType());
    }
  };
  
  class StructElementComponent : public PhysicalPathComponent {
    VarDecl *Field;
    SILType SubstFieldType;
  public:
    StructElementComponent(VarDecl *field, SILType substFieldType,
                           SILType typeOfRValue)
      : PhysicalPathComponent(typeOfRValue),
        Field(field), SubstFieldType(substFieldType) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                    SILValue base) const override {
      assert(base && "invalid value for element base");
      SILType baseType = base.getType();
      (void)baseType;
      assert(baseType.isAddress() &&
             "base for element component must be an address");
      assert(!baseType.hasReferenceSemantics() &&
             "can't get element from address of ref type");
      return gen.B.createStructElementAddr(loc, base, Field, SubstFieldType);
    }
  };

  class RefComponent : public PhysicalPathComponent {
    SILValue value;
  public:
    RefComponent(ManagedValue value) :
      PhysicalPathComponent(value.getValue().getType()), value(value.getValue()) {
      assert(value.getType().hasReferenceSemantics() &&
             "ref component must be of reference type");
    }
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                    SILValue base) const override {
      assert(!base && "ref component must be root of lvalue path");
      return value;
    }
  };
  
  class GetterSetterComponent : public LogicalPathComponent {
    SILDeclRef getter;
    SILDeclRef setter;
    std::vector<Substitution> substitutions;
    Expr *subscriptExpr;
    mutable RValue origSubscripts;
    Type substType;
    
    struct AccessorArgs {
      RValue base;
      RValue subscripts;
    };
    
    /// Returns a tuple of RValues holding the accessor value, base (retained if
    /// necessary), and subscript arguments, in that order.
    AccessorArgs
    prepareAccessorArgs(SILGenFunction &gen,
                        SILLocation loc,
                        SILValue base) const
    {
      assert((!base || (base.getType().isAddress() ^
                        base.getType().hasReferenceSemantics())) &&
             "base of getter/setter component must be invalid, lvalue, or "
             "of reference type");
      
      AccessorArgs result;      
      if (base) {
        if (base.getType().hasReferenceSemantics()) {
          gen.B.createStrongRetain(loc, base);
          result.base = RValue(gen,
                               gen.emitManagedRValueWithCleanup(base), loc);
        } else {
          result.base = RValue(gen,
                               ManagedValue(base, ManagedValue::LValue), loc);
        }
      }
      
      if (subscriptExpr) {
        if (!origSubscripts)
          origSubscripts = gen.emitRValue(subscriptExpr);
        result.subscripts = origSubscripts.copy(gen, loc);
      }
      
      return result;
    }
    
  public:
    GetterSetterComponent(ValueDecl *decl,
                          ArrayRef<Substitution> substitutions,
                          SILType typeOfRValue)
      : GetterSetterComponent(decl, substitutions, nullptr, typeOfRValue)
    {
    }

    GetterSetterComponent(ValueDecl *decl,
                          ArrayRef<Substitution> substitutions,
                          Expr *subscriptExpr,
                          SILType typeOfRValue)
      : LogicalPathComponent(typeOfRValue),
        getter(SILDeclRef(decl, SILDeclRef::Kind::Getter)),
        setter(decl->isSettable()
                 ? SILDeclRef(decl, SILDeclRef::Kind::Setter)
                 : SILDeclRef()),
        substitutions(substitutions.begin(), substitutions.end()),
        subscriptExpr(subscriptExpr),
        substType(typeOfRValue.getSwiftRValueType())
    {
    }
    
    GetterSetterComponent(const GetterSetterComponent &copied,
                          SILGenFunction &gen,
                          SILLocation loc)
      : LogicalPathComponent(copied.getTypeOfRValue()),
        getter(copied.getter),
        setter(copied.setter),
        substitutions(copied.substitutions),
        subscriptExpr(copied.subscriptExpr),
        origSubscripts(copied.origSubscripts.copy(gen, loc)),
        substType(copied.substType)
    {
    }
    
    bool isSettable() const override {
      return !setter.isNull();
    }
    
    void set(SILGenFunction &gen, SILLocation loc,
             RValue &&rvalue, SILValue base) const override
    {
      assert(!setter.isNull() && "not settable!");      
      auto args = prepareAccessorArgs(gen, loc, base);
      
      return gen.emitSetProperty(loc, setter, substitutions,
                                 std::move(args.base),
                                 std::move(args.subscripts),
                                 std::move(rvalue));
    }
    
    ManagedValue get(SILGenFunction &gen, SILLocation loc,
                     SILValue base, SGFContext c) const override
    {
      auto args = prepareAccessorArgs(gen, loc, base);
      
      return gen.emitGetProperty(loc, getter, substitutions,
                                 std::move(args.base),
                                 std::move(args.subscripts),
                                 substType, c);
    }
    
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen, SILLocation loc) const override {
      LogicalPathComponent *clone = new GetterSetterComponent(*this, gen, loc);
      return std::unique_ptr<LogicalPathComponent>(clone);
    }
  };
}

LValue SILGenLValue::visitRec(Expr *e) {
  if (e->getType()->hasReferenceSemantics()) {
    // Any reference type expression can form the root of a logical lvalue.
    LValue lv;
    lv.add<RefComponent>(gen.emitRValue(e).getAsSingleValue(gen, e));
    return ::std::move(lv);
  } else {
    return visit(e);
  }
}

LValue SILGenLValue::visitExpr(Expr *e) {
  e->dump();
  llvm_unreachable("unimplemented lvalue expr");
}

static SILType getSubstTypeOfRValue(SILGenFunction &gen, Type lvalue) {
  auto objType = cast<LValueType>(lvalue->getCanonicalType()).getObjectType();
  return gen.getLoweredType(objType);
}

static LValue emitLValueForDecl(SILGenLValue &sgl,
                                SILLocation loc, ValueDecl *decl,
                                Type substTypeOfReference) {
  auto substTypeOfRValue = getSubstTypeOfRValue(sgl.gen, substTypeOfReference);
  
  LValue lv;

  // If it's a property, push a reference to the getter and setter.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    if (var->isProperty()) {
      lv.add<GetterSetterComponent>(var,
                                    ArrayRef<Substitution>{},
                                    substTypeOfRValue);
      return ::std::move(lv);
    }
  }

  // If it's a physical value, push its address.
  SILValue address = sgl.gen.emitReferenceToDecl(loc, decl).getUnmanagedValue();
  assert(address.getType().isAddress() &&
         "physical lvalue decl ref must evaluate to an address");
  lv.add<AddressComponent>(address, substTypeOfRValue);
  return ::std::move(lv);
}

LValue SILGenLValue::visitDeclRefExpr(DeclRefExpr *e) {
  return emitLValueForDecl(*this, e, e->getDecl(), e->getType());
}

LValue SILGenLValue::visitSuperRefExpr(SuperRefExpr *e) {
  // The type of reference here is a lie, but it works out given the
  // syntactic constraint on 'super'.
  return emitLValueForDecl(*this, e, e->getSelf(),
                           LValueType::get(e->getSelf()->getType(),
                                           LValueType::Qual::DefaultForVar,
                                           gen.getASTContext()));
}

LValue SILGenLValue::visitMaterializeExpr(MaterializeExpr *e) {
  LValue lv;

  // Evaluate the value, then use it to initialize a new temporary and return
  // the temp's address.
  ManagedValue v = gen.emitRValue(e->getSubExpr()).getAsSingleValue(gen,
                                                              e->getSubExpr());
  SILValue addr = gen.emitMaterialize(e, v).address;
  lv.add<AddressComponent>(addr, getSubstTypeOfRValue(gen, e->getType()));
  return ::std::move(lv);
}

LValue SILGenLValue::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e)
{
  gen.emitRValue(e->getLHS());
  return visitRec(e->getRHS());
}

LValue SILGenLValue::visitMemberRefExpr(MemberRefExpr *e) {
  LValue lv = visitRec(e->getBase());
  CanType baseTy = e->getBase()->getType()->getCanonicalType();

  auto substTypeOfRValue = getSubstTypeOfRValue(gen, e->getType());

  // If this is a physical field, access with a fragile element reference.
  if (VarDecl *var = dyn_cast<VarDecl>(e->getMember().getDecl())) {
    if (!var->isProperty()) {
      // Find the substituted storage type.
      SILType varStorageType =
        gen.SGM.Types.getSubstitutedStorageType(var, e->getType());
      if (!isa<LValueType>(baseTy)) {
        assert(baseTy.hasReferenceSemantics());
        lv.add<RefElementComponent>(var, varStorageType, substTypeOfRValue);
      } else {
        lv.add<StructElementComponent>(var, varStorageType, substTypeOfRValue);
      }
      return ::std::move(lv);
    }
  }

  // Otherwise, use the property accessors.
  lv.add<GetterSetterComponent>(e->getMember().getDecl(),
                                e->getMember().getSubstitutions(),
                                substTypeOfRValue);
  return ::std::move(lv);
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e) {
  auto substTypeOfRValue = getSubstTypeOfRValue(gen, e->getType());

  LValue lv = visitRec(e->getBase());
  lv.add<GetterSetterComponent>(e->getDecl().getDecl(),
                                e->getDecl().getSubstitutions(),
                                e->getIndex(),
                                substTypeOfRValue);
  return ::std::move(lv);
}

LValue SILGenLValue::visitTupleElementExpr(TupleElementExpr *e) {
  LValue lv = visitRec(e->getBase());
  lv.add<TupleElementComponent>(e->getFieldNumber(),
                                getSubstTypeOfRValue(gen, e->getType()));
  return ::std::move(lv);
}

LValue SILGenLValue::visitAddressOfExpr(AddressOfExpr *e) {
  return visitRec(e->getSubExpr());
}

LValue SILGenLValue::visitParenExpr(ParenExpr *e) {
  return visitRec(e->getSubExpr());
}

LValue SILGenLValue::visitRequalifyExpr(RequalifyExpr *e) {
  assert(e->getType()->is<LValueType>() &&
         "non-lvalue requalify in lvalue expression");
  return visitRec(e->getSubExpr());
}

/// Load an r-value out of the given address.
///
/// \param rvalueTL - the type lowering for the type-of-rvalue
///   of the address
ManagedValue SILGenFunction::emitLoad(SILLocation loc,
                                      SILValue addr,
                                      const TypeLowering &rvalueTL,
                                      SGFContext C,
                                      IsTake_t isTake) {
  // Get the lowering for the address type.  We can avoid a re-lookup
  // in the very common case of this being equivalent to the r-value
  // type.
  auto &addrTL =
    (addr.getType() == rvalueTL.getLoweredType().getAddressType()
       ? rvalueTL : getTypeLowering(addr.getType()));

  if (rvalueTL.isAddressOnly()) {
    // Copy the address-only value.
    SILValue copy = getBufferForExprResult(loc, rvalueTL.getLoweredType(), C);
    emitSemanticLoadInto(loc, addr, addrTL, copy, rvalueTL,
                         isTake, IsInitialization);
    return emitManagedRValueWithCleanup(copy, rvalueTL);
  }
  
  // Load the loadable value, and retain it if we aren't taking it.
  SILValue loadedV = emitSemanticLoad(loc, addr, addrTL, rvalueTL, isTake);
  return emitManagedRValueWithCleanup(loadedV, rvalueTL);
}

static void emitUnloweredStoreOfCopy(SILBuilder &B, SILLocation loc,
                                     SILValue value, SILValue addr,
                                     IsInitialization_t isInit) {
  if (isInit) {
    B.createStore(loc, value, addr);
  } else {
    B.createAssign(loc, value, addr);
  }
}

static bool hasDifferentTypeOfRValue(const TypeLowering &srcTL) {
  return srcTL.getLoweredType().is<ReferenceStorageType>();
}

/// Given that the type-of-rvalue differs from the type-of-storage,
/// and given that the type-of-rvalue is loadable, produce a +1 scalar
/// of the type-of-rvalue.
static SILValue emitLoadOfScalarRValue(SILGenFunction &gen,
                                       SILLocation loc,
                                       SILType storageType,
                                       SILValue src,
                                       IsTake_t isTake) {
  // For [weak] types, we need to create an Optional<T>.
  if (storageType.is<WeakStorageType>()) {
    // TODO: actually make an Optional<T>
    return gen.B.createLoadWeak(loc, src, isTake);
  }

  // For [unowned] types, we need to strip the unowned box.
  if (auto unownedType = storageType.getAs<UnownedStorageType>()) {
    auto unownedValue = gen.B.createLoad(loc, src);
    gen.B.createStrongRetainUnowned(loc, unownedValue);
    if (isTake) gen.B.createUnownedRelease(loc, unownedValue);
    return gen.B.createUnownedToRef(loc, unownedValue,
              SILType::getPrimitiveObjectType(unownedType.getReferentType()));
  }

  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Given that the type-of-rvalue differs from the type-of-storage,
/// store a +1 value (possibly not a scalar) of the type-of-rvalue
/// into the given address.
static void emitStoreOfScalarRValue(SILGenFunction &gen,
                                    SILLocation loc,
                                    SILValue value,
                                    SILValue dest,
                                    IsInitialization_t isInit) {
  auto storageType = dest.getType();

  // Function values might need to be generalized.  This is actually
  // pretty broken; the general expression emitter has a
  // responsibility to emit functions with the right type.
  if (storageType.is<AnyFunctionType>()) {
    value = gen.B.emitGeneralizedValue(loc, value);
    emitUnloweredStoreOfCopy(gen.B, loc, value, dest, isInit);
    return;
  }

  // For [weak] types, we need to break down an Optional<T> and then
  // emit the storeWeak ourselves.
  if (storageType.is<WeakStorageType>()) {
    // TODO: actually expect an Optional<T>
    gen.B.createStoreWeak(loc, value, dest, isInit);

    // store_weak doesn't take ownership of the input, so cancel it out.
    gen.B.createStrongRelease(loc, value);

    return;
  }

  // For [unowned] types, we need to enter the unowned box by turning
  // the strong retain into an unowned retain.
  if (storageType.is<UnownedStorageType>()) {
    auto unownedValue =
      gen.B.createRefToUnowned(loc, value, storageType.getObjectType());
    gen.B.createUnownedRetain(loc, unownedValue);
    emitUnloweredStoreOfCopy(gen.B, loc, unownedValue, dest, isInit);
    gen.B.createStrongRelease(loc, value);
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
    assert(!hasDifferentTypeOfRValue(srcTL));
    return srcTL.emitLoadOfCopy(B, loc, src, isTake);
  }

  return emitLoadOfScalarRValue(*this, loc, srcTL.getLoweredType(),
                                src, isTake);
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
    assert(!hasDifferentTypeOfRValue(srcTL));
    return srcTL.emitCopyInto(B, loc, src, dest, isTake, isInit);
  }

  auto rvalue =
    emitLoadOfScalarRValue(*this, loc, srcTL.getLoweredType(), src, isTake);
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
    assert(!hasDifferentTypeOfRValue(destTL));
    assert(destTL.isAddressOnly() == rvalue.getType().isAddress());
    if (rvalue.getType().isAddress()) {
      destTL.emitCopyInto(B, loc, rvalue, dest, IsTake, isInit);
    } else {
      emitUnloweredStoreOfCopy(B, loc, rvalue, dest, isInit);
    }
    return;
  }

  emitStoreOfScalarRValue(*this, loc, rvalue, dest, isInit);
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
  
  // [weak] types are never loadable, so we don't need to handle them here.
  
  // For [unowned] types, place into an unowned box.
  if (storageType.is<UnownedStorageType>()) {
    SILValue unowned = B.createRefToUnowned(loc, semanticValue, storageType);
    B.createUnownedRetain(loc, unowned);
    B.createStrongRelease(loc, semanticValue);
    return unowned;
  }
  
  llvm_unreachable("unexpected storage type that differs from type-of-rvalue");
}

/// Produce a physical address that corresponds to the given l-value
/// component.
static SILValue drillIntoComponent(SILGenFunction &SGF,
                                   SILLocation loc,
                                   const PathComponent &component,
                                   SILValue base) {
  assert(!base ||
         base.getType().isAddress() ||
         base.getType().hasReferenceSemantics());

  SILValue addr;
  if (component.isPhysical()) {
    addr = component.asPhysical().offset(SGF, loc, base);
  } else {
    auto &lcomponent = component.asLogical();
    Materialize temporary = lcomponent.getMaterialized(SGF, loc, base);
    addr = temporary.address;
  }

  assert(addr.getType().isAddress() ||
         addr.getType().hasReferenceSemantics());
  return addr;
}

/// Find the last component of the given lvalue and derive a base
/// location for it.
static const PathComponent &drillToLastComponent(SILGenFunction &SGF,
                                                 SILLocation loc,
                                                 const LValue &lv,
                                                 SILValue &addr) {
  assert(lv.begin() != lv.end() &&
         "lvalue must have at least one component");

  auto component = lv.begin(), next = lv.begin(), end = lv.end();
  ++next;
  for (; next != end; component = next, ++next) {
    addr = drillIntoComponent(SGF, loc, *component, addr);
  }

  return *component;
}

ManagedValue SILGenFunction::emitLoadOfLValue(SILLocation loc,
                                              const LValue &src,
                                              SGFContext C) {
  // No need to write back to a loaded lvalue.
  DisableWritebackScope scope(*this);

  SILValue addr;
  auto &component = drillToLastComponent(*this, loc, src, addr);

  // If the last component is physical, just drill down and load from it.
  if (component.isPhysical()) {
    addr = component.asPhysical().offset(*this, loc, addr);
    return emitLoad(loc, addr, getTypeLowering(src.getTypeOfRValue()),
                    C, IsNotTake);
  }

  // If the last component is logical, just emit a get.
  return component.asLogical().get(*this, loc, addr, C);
}

ManagedValue SILGenFunction::emitAddressOfLValue(SILLocation loc,
                                                 const LValue &src) {
  SILValue addr;
  
  assert(src.begin() != src.end() && "lvalue must have at least one component");
  for (auto &component : src) {
    addr = drillIntoComponent(*this, loc, component, addr);
  }
  assert(addr.getType().isAddress() &&
         "resolving lvalue did not give an address");
  return ManagedValue(addr, ManagedValue::LValue);
}

void SILGenFunction::emitAssignToLValue(SILLocation loc,
                                        RValue &&src, const LValue &dest) {
  WritebackScope scope(*this);
  
  // Resolve all components up to the last, keeping track of value-type logical
  // properties we need to write back to.
  SILValue destAddr;
  auto &component = drillToLastComponent(*this, loc, dest, destAddr);
  
  // Write to the tail component.
  if (component.isPhysical()) {
    SILValue finalDestAddr
      = component.asPhysical().offset(*this, loc, destAddr);
    
    std::move(src).getAsSingleValue(*this, loc)
      .assignInto(*this, loc, finalDestAddr);
  } else {
    component.asLogical().set(*this, loc, std::move(src), destAddr);
  }

  // The writeback scope closing will propagate the value back up through the
  // writeback chain.
}

