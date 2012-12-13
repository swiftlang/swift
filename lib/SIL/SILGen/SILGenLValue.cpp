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
#include "TypeInfo.h"
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
      assert(address.getType()->is<LValueType>() &&
             "var component value must be an address");
    }
    
    Value offset(SILGenFunction &gen, SILLocation loc,
                 Value base) const override {
      assert(!base && "var component must be root of lvalue path");
      return address;
    }
  };
  
  class FragileElementComponent : public PhysicalPathComponent {
    FragileElement element;
  public:
    FragileElementComponent(FragileElement element) : element(element) {}
    
    Value offset(SILGenFunction &gen, SILLocation loc,
                 Value base) const override {
      assert(base && "invalid value for element base");
      Type baseType = base.getType();
      ASTContext &C = baseType->getASTContext();
      if (LValueType *baseLT = base.getType()->getAs<LValueType>()) {
        return gen.B.createElementAddr(loc, base, element.index,
                                       LValueType::get(element.type,
                                                   baseLT->getQualifiers(), C));
      } else if (baseType->hasReferenceSemantics()) {
        return gen.B.createRefElementAddr(loc, base, element.index,
                  LValueType::get(element.type,
                                  LValueType::Qual::DefaultForMemberAccess, C));
      } else
        llvm_unreachable("base for element component must have ref type "
                         "or be an address");
    }
  };

  class RefComponent : public PhysicalPathComponent {
    Value value;
  public:
    RefComponent(ManagedValue value) : value(value.getValue()) {
      assert(value.getValue().getType()->hasReferenceSemantics() &&
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
      llvm::SmallVector<Writeback, 2> writebacks;
      gen.emitApplyArguments(subscriptExpr, args, writebacks);
      assert(writebacks.empty() && "subscript shouldn't have byref args");
    }
    
    ManagedValue partialApplyAccessor(SILGenFunction &gen, SILLocation loc,
                                      Value accessor, Value base) const {
      assert((!base || base.getType()->is<LValueType>() ||
              base.getType()->hasReferenceSemantics()) &&
             "base of getter/setter component must be invalid, lvalue, or "
             "of reference type");
      gen.B.createRetain(loc, accessor);
      if (base && base.getType()->hasReferenceSemantics())
        gen.B.createRetain(loc, base);
      // Apply the base "this" argument, if any.
      ManagedValue appliedThis = base
        ? gen.emitManagedRValueWithCleanup(gen.B.createApply(loc,
                                                             accessor, base))
        : ManagedValue(accessor);
      // Apply the subscript argument, if any.
      if (subscriptExpr) {
        llvm::SmallVector<Value, 2> args;
        getSubscriptArguments(gen, args);
        Value appliedSubscript = gen.B.createApply(loc,
                                                   appliedThis.forward(gen),
                                                   args);
        return gen.emitManagedRValueWithCleanup(appliedSubscript);
      } else
        return appliedThis;
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
                     Value rvalue, Value base,
                     ShouldPreserveValues preserve) const override {
      ManagedValue appliedSetter = partialApplyAccessor(gen, loc,
                                                        setter, base);
      gen.B.createApply(loc, appliedSetter.forward(gen), rvalue);
    }
    
    Materialize loadAndMaterialize(SILGenFunction &gen, SILLocation loc,
                                   Value base,
                                   ShouldPreserveValues preserve)
                                   const override
    {
      // FIXME: ignores base and preserve
      ManagedValue appliedGetter = partialApplyAccessor(gen, loc,
                                                        getter, base);
      return gen.emitGetProperty(loc, appliedGetter);
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
                                        SILConstant(var, SILConstant::Getter));
      Value set = gen.emitUnmanagedConstantRef(e,
                                        SILConstant(var, SILConstant::Setter));
      lv.add<GetterSetterComponent>(get, set);
      return ::std::move(lv);
    }
  }

  // If it's a physical value, push its address.
  Value address = gen.emitReferenceToDecl(e, decl).getUnmanagedValue();
  assert(address.getType()->is<LValueType>() &&
         "physical lvalue decl ref must evaluate to an address");
  lv.add<AddressComponent>(address);
  return ::std::move(lv);
}

LValue SILGenLValue::visitMaterializeExpr(MaterializeExpr *e) {
  LValue lv;
  Value materialized = gen.visitMaterializeExpr(e).getUnmanagedValue();
  lv.add<AddressComponent>(materialized);
  return ::std::move(lv);
}

LValue SILGenLValue::visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *e)
{
  gen.visit(e->getLHS());
  return visitRec(e->getRHS());
}

LValue SILGenLValue::visitMemberRefExpr(MemberRefExpr *e) {
  LValue lv = visitRec(e->getBase());
  VarDecl *decl = e->getDecl();
  TypeInfo const &ti = gen.getTypeInfo(
                                      e->getBase()->getType()->getRValueType());
  
  if (ti.hasFragileElement(decl->getName())) {
    lv.add<FragileElementComponent>(ti.getFragileElement(decl->getName()));
  } else {
    Value get = gen.emitUnmanagedConstantRef(e,
                                        SILConstant(decl, SILConstant::Getter));
    Value set = gen.emitUnmanagedConstantRef(e,
                                        SILConstant(decl, SILConstant::Setter));
    lv.add<GetterSetterComponent>(get, set);
  }
  
  return ::std::move(lv);
}

LValue SILGenLValue::visitSubscriptExpr(SubscriptExpr *e) {
  LValue lv = visitRec(e->getBase());
  SubscriptDecl *sd = e->getDecl();
  Value get = gen.emitUnmanagedConstantRef(e,
                                       SILConstant(sd, SILConstant::Getter));
  Value set = gen.emitUnmanagedConstantRef(e,
                                       SILConstant(sd, SILConstant::Setter));
  lv.add<GetterSetterComponent>(get, set, e->getIndex());
  return ::std::move(lv);
}

LValue SILGenLValue::visitTupleElementExpr(TupleElementExpr *e) {
  LValue lv = visitRec(e->getBase());
  // FIXME: address-only tuples
  lv.add<FragileElementComponent>(FragileElement{e->getType()->getRValueType(),
                                                 e->getFieldNumber()});
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
