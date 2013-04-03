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

#include "SILGen.h"
#include "swift/AST/AST.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "LValue.h"
#include "ManagedValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Lowering;

void PathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}

namespace {
  class AddressComponent : public PhysicalPathComponent {
    Value address;
  public:
    AddressComponent(Value address) : address(address) {
      assert(address.getType().isAddress() &&
             "var component value must be an address");
    }
    
    Value offset(SILGenFunction &gen, SILLocation loc,
                 Value base) const override {
      assert(!base && "var component must be root of lvalue path");
      return address;
    }
  };
  
  class RefElementComponent : public PhysicalPathComponent {
    VarDecl *field;
    SILType type;
  public:
    RefElementComponent(VarDecl *field, SILType type)
      : field(field), type(type) {}
    
    Value offset(SILGenFunction &gen, SILLocation loc, Value base)
      const override
    {
      assert(!base.getType().isAddress() &&
             "base for ref element component can't be an address");
      assert(base.getType().hasReferenceSemantics() &&
             "base for ref element component must be a reference type");
      return gen.B.createRefElementAddr(loc, base, field, type);
    }
  };
  
  class FragileElementComponent : public PhysicalPathComponent {
    unsigned elementIndex;
    SILType type;
  public:
    FragileElementComponent(unsigned elementIndex, SILType type)
      : elementIndex(elementIndex), type(type) {}
    
    Value offset(SILGenFunction &gen, SILLocation loc,
                 Value base) const override {
      assert(base && "invalid value for element base");
      SILType baseType = base.getType();
      (void)baseType;
      assert(baseType.isAddress() &&
             "base for element component must be an address");
      assert(!baseType.hasReferenceSemantics() &&
             "can't get element from address of ref type");
      return gen.B.createElementAddr(loc, base, elementIndex, type);
    }
  };

  class RefComponent : public PhysicalPathComponent {
    Value value;
  public:
    RefComponent(ManagedValue value) : value(value.getValue()) {
      assert(value.getType().hasReferenceSemantics() &&
             "ref component must be of reference type");
    }
    
    Value offset(SILGenFunction &gen, SILLocation loc,
                 Value base) const override {
      assert(!base && "ref component must be root of lvalue path");
      return value;
    }
  };
  
  class GetterSetterComponent : public LogicalPathComponent {
    Value getter;
    Value setter;
    Expr *subscriptExpr;
    
    void getSubscriptArguments(SILGenFunction &gen,
                               llvm::SmallVectorImpl<Value> &args) const {
      gen.emitApplyArguments(subscriptExpr, args);
    }
    
    void prepareAccessorArgs(SILGenFunction &gen, SILLocation loc,
                             Value accessor, Value base,
                             SmallVectorImpl<Value> &subscripts) const
    {
      
      
      assert((!base || (base.getType().isAddress() ^
              base.getType().hasReferenceSemantics())) &&
             "base of getter/setter component must be invalid, lvalue, or "
             "of reference type");
      
      gen.B.createRetain(loc, accessor);
      
      if (base && base.getType().hasReferenceSemantics())
        gen.B.createRetain(loc, base);
      
      if (subscriptExpr)
        getSubscriptArguments(gen, subscripts);
    }
    
  public:
    GetterSetterComponent(Value getter, Value setter)
      : getter(getter), setter(setter), subscriptExpr(nullptr)
    {
      assert(getter && setter &&
             "settable lvalue must have both getter and setter");
    }

    GetterSetterComponent(Value getter, Value setter,
                          Expr *subscriptExpr)
      : getter(getter), setter(setter), subscriptExpr(subscriptExpr)
    {
      assert(getter && setter &&
             "settable lvalue must have both getter and setter");
    }
    
    void storeRValue(SILGenFunction &gen, SILLocation loc,
                     ManagedValue rvalue, Value base) const override
    {
      llvm::SmallVector<Value, 2> subscripts;
      prepareAccessorArgs(gen, loc, setter, base, subscripts);
      
      return gen.emitSetProperty(loc,
                                 ManagedValue(setter, ManagedValue::Unmanaged),
                                 base
                                   ? Optional<ManagedValue>(base,
                                                        ManagedValue::Unmanaged)
                                   : Nothing,
                                 subscriptExpr
                                   ? Optional<ArrayRef<Value>>(subscripts)
                                   : Nothing,
                                 rvalue);
    }
    
    Materialize loadAndMaterialize(SILGenFunction &gen, SILLocation loc,
                                   Value base) const override
    {
      llvm::SmallVector<Value, 2> subscripts;
      prepareAccessorArgs(gen, loc, getter, base, subscripts);
      
      return gen.emitGetProperty(loc,
                                 ManagedValue(getter, ManagedValue::Unmanaged),
                                 base
                                   ? Optional<ManagedValue>(base,
                                                        ManagedValue::Unmanaged)
                                   : Nothing,
                                 subscriptExpr
                                   ? Optional<ArrayRef<Value>>(subscripts)
                                   : Nothing);
    }
  };
}

LValue SILGenLValue::visitRec(Expr *e) {
  if (e->getType()->hasReferenceSemantics()) {
    // Any reference type expression can form the root of a logical lvalue.
    LValue lv;
    lv.add<RefComponent>(gen.visit(e));
    return ::std::move(lv);
  } else {
    return visit(e);
  }
}

LValue SILGenLValue::visitExpr(Expr *e) {
  e->dump();
  llvm_unreachable("unimplemented lvalue expr");
}

LValue SILGenLValue::visitDeclRefExpr(DeclRefExpr *e) {
  LValue lv;
  ValueDecl *decl = e->getDecl();

  // If it's a property, push a reference to the getter and setter.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    if (var->isProperty()) {
      Value get = gen.emitUnmanagedConstantRef(e,
                                  SILConstant(var, SILConstant::Kind::Getter));
      Value set = gen.emitUnmanagedConstantRef(e,
                                  SILConstant(var, SILConstant::Kind::Setter));
      lv.add<GetterSetterComponent>(get, set);
      return ::std::move(lv);
    }
  }

  // If it's a physical value, push its address.
  Value address = gen.emitReferenceToDecl(e, decl).getUnmanagedValue();
  assert(address.getType().isAddress() &&
         "physical lvalue decl ref must evaluate to an address");
  lv.add<AddressComponent>(address);
  return ::std::move(lv);
}

LValue SILGenLValue::visitMaterializeExpr(MaterializeExpr *e) {
  LValue lv;
  Value materialized = gen.visit(e).getUnmanagedValue();
  lv.add<AddressComponent>(materialized);
  return ::std::move(lv);
}

LValue SILGenLValue::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e)
{
  gen.visit(e->getLHS());
  return visitRec(e->getRHS());
}

namespace {
  
template<typename ANY_MEMBER_REF_EXPR>
LValue emitAnyMemberRefExpr(SILGenLValue &sgl,
                            SILGenFunction &gen,
                            ANY_MEMBER_REF_EXPR *e,
                            ArrayRef<Substitution> substitutions) {
  LValue lv = sgl.visitRec(e->getBase());
  ValueDecl *decl = e->getDecl();
  SILType baseTy = gen.getLoweredType(e->getBase()->getType()->getRValueType());

  // If this is a physical field, access with a fragile element reference.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    if (!var->isProperty()) {
      if (baseTy.hasReferenceSemantics()) {
        lv.add<RefElementComponent>(var,
                                    gen.getLoweredType(e->getType()));
      } else {
        SILCompoundTypeInfo *cti = baseTy.getCompoundTypeInfo();
        lv.add<FragileElementComponent>(cti->getIndexOfMemberDecl(var),
                                        gen.getLoweredType(e->getType()));
      }
      return ::std::move(lv);
    }
  }
  
  // Otherwise, use the property accessors.
  ManagedValue get = gen.emitSpecializedPropertyConstantRef(e, e->getBase(),
                                   /*subscriptExpr=*/nullptr,
                                   SILConstant(decl, SILConstant::Kind::Getter),
                                   substitutions);
  ManagedValue set = gen.emitSpecializedPropertyConstantRef(e, e->getBase(),
                                   /*subscriptExpr=*/nullptr,
                                   SILConstant(decl, SILConstant::Kind::Setter),
                                   substitutions);
  lv.add<GetterSetterComponent>(get.getValue(), set.getValue());
  return ::std::move(lv);
}
  
template<typename ANY_SUBSCRIPT_EXPR>
LValue emitAnySubscriptExpr(SILGenLValue &sgl,
                            SILGenFunction &gen,
                            ANY_SUBSCRIPT_EXPR *e,
                            ArrayRef<Substitution> substitutions) {
  LValue lv = sgl.visitRec(e->getBase());
  SubscriptDecl *sd = e->getDecl();
  ManagedValue get = gen.emitSpecializedPropertyConstantRef(e, e->getBase(),
                                     e->getIndex(),
                                     SILConstant(sd, SILConstant::Kind::Getter),
                                     substitutions);
  ManagedValue set = gen.emitSpecializedPropertyConstantRef(e, e->getBase(),
                                     e->getIndex(),
                                     SILConstant(sd, SILConstant::Kind::Setter),
                                     substitutions);
  lv.add<GetterSetterComponent>(get.getValue(), set.getValue(),
                                e->getIndex());
  return ::std::move(lv);
}
  
} // end anonymous namespace

LValue SILGenLValue::visitGenericMemberRefExpr(GenericMemberRefExpr *e) {
  return emitAnyMemberRefExpr(*this, gen, e, e->getSubstitutions());
}

LValue SILGenLValue::visitMemberRefExpr(MemberRefExpr *e) {
  return emitAnyMemberRefExpr(*this, gen, e, {});
}

LValue SILGenLValue::visitGenericSubscriptExpr(GenericSubscriptExpr *e) {
  return emitAnySubscriptExpr(*this, gen, e, e->getSubstitutions());
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e) {
  return emitAnySubscriptExpr(*this, gen, e, {});
}

LValue SILGenLValue::visitTupleElementExpr(TupleElementExpr *e) {
  LValue lv = visitRec(e->getBase());
  // FIXME: address-only tuples
  TypeLoweringInfo const &ti = gen.getTypeLoweringInfo(e->getType());
  assert(ti.isLoadable() &&
         "address-only tuples not yet implemented");
  lv.add<FragileElementComponent>(e->getFieldNumber(),
                                  ti.getLoweredType());
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
