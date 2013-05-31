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
#include "RValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace Lowering;

/// SILGenLValue - An ASTVisitor for building logical lvalues.
/// Used to visit the left-hand sides of AssignStmts and [byref] arguments of
/// ApplyExprs.
class LLVM_LIBRARY_VISIBILITY SILGenLValue
  : public ExprVisitor<SILGenLValue, LValue>
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
  LValue visitGenericMemberRefExpr(GenericMemberRefExpr *e);
  LValue visitSubscriptExpr(SubscriptExpr *e);
  LValue visitGenericSubscriptExpr(GenericSubscriptExpr *e);
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
  ManagedValue value = get(gen, loc, base);
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
    WritebackStack.emplace_back(loc, component.clone(*this), base, temp);
  }
}

WritebackScope::WritebackScope(SILGenFunction &gen)
  : gen(&gen), wasInWritebackScope(gen.InWritebackScope),
    savedDepth(gen.WritebackStack.size())
{
  gen.InWritebackScope = true;
}

ManagedValue Materialize::claim(SILGenFunction &gen, SILLocation loc) {
  if (address.getType().isAddressOnly(gen.SGM.M)) {
    // We can use the temporary as an address-only rvalue directly.
    return ManagedValue(address, valueCleanup);
  }

  if (valueCleanup.isValid())
    gen.Cleanups.setCleanupState(valueCleanup, CleanupState::Dead);
  return gen.emitLoad(loc, address, SGFContext(), /*isTake*/ true);
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
    i->component->set(*gen, i->loc, RValue(*gen, mv), i->base);
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
  return RValue(*this, emitAddressOfLValue(e, lv));
}

void PathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}

namespace {
  class AddressComponent : public PhysicalPathComponent {
    SILValue address;
  public:
    AddressComponent(SILValue address) : address(address) {
      assert(address.getType().isAddress() &&
             "var component value must be an address");
    }
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                 SILValue base) const override {
      assert(!base && "var component must be root of lvalue path");
      return address;
    }
    
    Type getObjectType() const override {
      return address.getType().getSwiftRValueType();
    }
  };
  
  class RefElementComponent : public PhysicalPathComponent {
    VarDecl *field;
    SILType type;
  public:
    RefElementComponent(VarDecl *field, SILType type)
      : field(field), type(type) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc, SILValue base)
      const override
    {
      assert(!base.getType().isAddress() &&
             "base for ref element component can't be an address");
      assert(base.getType().hasReferenceSemantics() &&
             "base for ref element component must be a reference type");
      return gen.B.createRefElementAddr(loc, base, field, type);
    }
    
    Type getObjectType() const override {
      return type.getSwiftRValueType();
    }
  };
  
  class TupleElementComponent : public PhysicalPathComponent {
    unsigned elementIndex;
    SILType type;
  public:
    TupleElementComponent(unsigned elementIndex, SILType type)
      : elementIndex(elementIndex), type(type) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                 SILValue base) const override {
      assert(base && "invalid value for element base");
      SILType baseType = base.getType();
      (void)baseType;
      assert(baseType.isAddress() &&
             "base for element component must be an address");
      assert(!baseType.hasReferenceSemantics() &&
             "can't get element from address of ref type");
      return gen.B.createTupleElementAddr(loc, base, elementIndex, type);
    }
    
    Type getObjectType() const override {
      return type.getSwiftRValueType();
    }
  };
  
  class StructElementComponent : public PhysicalPathComponent {
    VarDecl *field;
    SILType type;
  public:
    StructElementComponent(VarDecl *field, SILType type)
      : field(field), type(type) {}
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                 SILValue base) const override {
      assert(base && "invalid value for element base");
      SILType baseType = base.getType();
      (void)baseType;
      assert(baseType.isAddress() &&
             "base for element component must be an address");
      assert(!baseType.hasReferenceSemantics() &&
             "can't get element from address of ref type");
      return gen.B.createStructElementAddr(loc, base, field, type);
    }
    
    Type getObjectType() const override {
      return type.getSwiftRValueType();
    }
  };

  class RefComponent : public PhysicalPathComponent {
    SILValue value;
  public:
    RefComponent(ManagedValue value) : value(value.getValue()) {
      assert(value.getType().hasReferenceSemantics() &&
             "ref component must be of reference type");
    }
    
    SILValue offset(SILGenFunction &gen, SILLocation loc,
                 SILValue base) const override {
      assert(!base && "ref component must be root of lvalue path");
      return value;
    }

    Type getObjectType() const override {
      return value.getType().getSwiftRValueType();
    }
  };
  
  class GetterSetterComponent : public LogicalPathComponent {
    SILConstant getter;
    SILConstant setter;
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
          gen.B.createRetain(loc, base);
          result.base = RValue(gen, gen.emitManagedRValueWithCleanup(base));
        } else {
          result.base = RValue(gen, ManagedValue(base, ManagedValue::LValue));
        }
      }
      
      if (subscriptExpr) {
        if (!origSubscripts)
          origSubscripts = gen.visit(subscriptExpr);
        result.subscripts = origSubscripts.copy(gen);
      }
      
      return result;
    }
    
  public:
    GetterSetterComponent(ValueDecl *decl,
                          ArrayRef<Substitution> substitutions,
                          Type substType)
      : GetterSetterComponent(decl, substitutions, nullptr, substType)
    {
    }

    GetterSetterComponent(ValueDecl *decl,
                          ArrayRef<Substitution> substitutions,
                          Expr *subscriptExpr,
                          Type substType)
      : getter(SILConstant(decl, SILConstant::Kind::Getter)),
        setter(decl->isSettable()
                 ? SILConstant(decl, SILConstant::Kind::Setter)
                 : SILConstant()),
        substitutions(substitutions.begin(), substitutions.end()),
        subscriptExpr(subscriptExpr),
        substType(substType)
    {
    }
    
    GetterSetterComponent(const GetterSetterComponent &copied,
                          SILGenFunction &gen)
      : getter(copied.getter),
        setter(copied.setter),
        substitutions(copied.substitutions),
        subscriptExpr(copied.subscriptExpr),
        origSubscripts(copied.origSubscripts.copy(gen)),
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
                     SILValue base) const override
    {
      auto args = prepareAccessorArgs(gen, loc, base);
      
      return gen.emitGetProperty(loc, getter, substitutions,
                                 std::move(args.base),
                                 std::move(args.subscripts),
                                 substType);
    }
    
    Type getObjectType() const override {
      return substType;
    }
    
    std::unique_ptr<LogicalPathComponent>
    clone(SILGenFunction &gen) const override {
      LogicalPathComponent *clone = new GetterSetterComponent(*this, gen);
      return std::unique_ptr<LogicalPathComponent>(clone);
    }
  };
}

LValue SILGenLValue::visitRec(Expr *e) {
  if (e->getType()->hasReferenceSemantics()) {
    // Any reference type expression can form the root of a logical lvalue.
    LValue lv;
    lv.add<RefComponent>(gen.visit(e).getAsSingleValue(gen));
    return ::std::move(lv);
  } else {
    return visit(e);
  }
}

LValue SILGenLValue::visitExpr(Expr *e) {
  e->dump();
  llvm_unreachable("unimplemented lvalue expr");
}

static LValue emitLValueForDecl(SILGenLValue &sgl,
                                SILLocation loc, ValueDecl *decl,
                                Type ty) {
  LValue lv;
  
  // If it's a property, push a reference to the getter and setter.
  if (VarDecl *var = dyn_cast<VarDecl>(decl)) {
    if (var->isProperty()) {
      lv.add<GetterSetterComponent>(var,
                                    ArrayRef<Substitution>{},
                                    ty->getRValueType());
      return ::std::move(lv);
    }
  }
  
  // If it's a physical value, push its address.
  SILValue address = sgl.gen.emitReferenceToDecl(loc, decl).getUnmanagedValue();
  assert(address.getType().isAddress() &&
         "physical lvalue decl ref must evaluate to an address");
  lv.add<AddressComponent>(address);
  return ::std::move(lv);
}

LValue SILGenLValue::visitDeclRefExpr(DeclRefExpr *e) {
  return emitLValueForDecl(*this, e, e->getDecl(), e->getType());
}

LValue SILGenLValue::visitSuperRefExpr(SuperRefExpr *e) {
  return emitLValueForDecl(*this, e, e->getThis(), e->getType());
}

LValue SILGenLValue::visitMaterializeExpr(MaterializeExpr *e) {
  LValue lv;

  // Evaluate the value, then use it to initialize a new temporary and return
  // the temp's address.
  ManagedValue v = gen.visit(e->getSubExpr()).getAsSingleValue(gen);
  SILValue addr = gen.emitMaterialize(e, v).address;
  lv.add<AddressComponent>(addr);
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
        lv.add<StructElementComponent>(var,
                                       gen.getLoweredType(e->getType()));
      }
      return ::std::move(lv);
    }
  }
  
  // Otherwise, use the property accessors.
  lv.add<GetterSetterComponent>(decl,
                                substitutions,
                                e->getType()->getRValueType());
  return ::std::move(lv);
}
  
template<typename ANY_SUBSCRIPT_EXPR>
LValue emitAnySubscriptExpr(SILGenLValue &sgl,
                            SILGenFunction &gen,
                            ANY_SUBSCRIPT_EXPR *e,
                            ArrayRef<Substitution> substitutions) {
  LValue lv = sgl.visitRec(e->getBase());
  SubscriptDecl *sd = e->getDecl();
  lv.add<GetterSetterComponent>(sd,
                                substitutions,
                                e->getIndex(),
                                e->getType()->getRValueType());
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
  lv.add<TupleElementComponent>(e->getFieldNumber(),
                                gen.getLoweredType(e->getType()));
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
