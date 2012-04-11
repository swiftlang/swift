//===--- GenInit.cpp - IR Generation for Initialization -------------------===//
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
//  This file implements IR generation for the initialization of
//  local and global variables.
//
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Optional.h"
#include "swift/AST/Pattern.h"
#include "llvm/GlobalVariable.h"

#include "ASTVisitor.h"
#include "Cleanup.h"
#include "Explosion.h"
#include "GenHeap.h"
#include "GenTuple.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Scope.h"

#include "GenInit.h"

using namespace swift;
using namespace irgen;

namespace {
  /// A cleanup to destroy an object whose address isn't actually known yet.
  class UnboundDestroy : public Cleanup {
    const TypeInfo &TI;
    OwnedAddress Addr;

  public:
    UnboundDestroy(const TypeInfo &TI) : TI(TI) {}

    void setAddress(OwnedAddress addr) {
      assert(!Addr.isValid());
      Addr = addr;
    }

    void emit(IRGenFunction &IGF) const {
      assert(Addr.isValid());
      llvm::Value *owner = Addr.getOwner();
      if (!isa<llvm::ConstantPointerNull>(owner)) {
        IGF.emitRelease(owner);
      } else {
        TI.destroy(IGF, Addr);
      }
    }
  };
}

/// Should the given variable be allocated on the heap?
static OnHeap_t isOnHeap(VarDecl *var) {
  return (var->hasFixedLifetime() ? NotOnHeap : OnHeap);
}

/// Register an object with the initialization process.
void Initialization::registerObject(IRGenFunction &IGF, Object object,
                                    OnHeap_t onHeap, const TypeInfo &objectTI) {
  // Create the appropriate destroy cleanup.
  IRGenFunction::CleanupsDepth destroy;

  // We need a destroy cleanup if the object is on the heap or non-POD.
  if (onHeap || !objectTI.isPOD(ResilienceScope::Local)) {
    IGF.pushFullExprCleanupInState<UnboundDestroy>(CleanupState::Dormant,
                                                   objectTI);
    destroy = IGF.getCleanupsDepth();
  } else {
    destroy = IRGenFunction::CleanupsDepth::invalid();
  }

  registerObject(object, destroy);
}

void Initialization::registerObjectWithoutDestroy(Object object) {
  registerObject(object, IRGenFunction::CleanupsDepth::invalid());
}

/// Register an object with the initialization process.
void Initialization::registerObject(Object object,
                                    IRGenFunction::CleanupsDepth destroy) {
  // The invariant is that the cleanup has to be an
  // UnboundDestroy if it's valid.

  ValueRecord record = {
    IRGenFunction::CleanupsDepth::invalid(), destroy
  };
  Records.insert(std::make_pair(object.Opaque, record));
}

/// Mark that an object has been allocated.
void Initialization::markAllocated(IRGenFunction &IGF, Object object,
                                   OwnedAddress address,
                                   IRGenFunction::CleanupsDepth dealloc) {
  ValueRecord &record = Records.find(object.Opaque)->second;
  record.DeallocCleanup = dealloc;

  // Update the destroy cleanup if present.
  if (record.DestroyCleanup.isValid()) {
    UnboundDestroy &destroy =
      static_cast<UnboundDestroy&>(IGF.findCleanup(record.DestroyCleanup));
    destroy.setAddress(address);
  }
}

/// Create a variable in the current scope.  Equivalent to either
/// emitLocalVariable or emitGlobalVariable, depending.
OwnedAddress Initialization::emitVariable(IRGenFunction &IGF, VarDecl *var,
                                          const TypeInfo &type) {
  if (var->isModuleScope())
    return emitGlobalVariable(IGF, var, type);

  OwnedAddress addr =
    emitLocalAllocation(IGF, getObjectForDecl(var), isOnHeap(var), type,
                        var->getName().str());
  IGF.setLocal(var, addr);
  return addr;
}

/// Emit a global variable.
OwnedAddress Initialization::emitGlobalVariable(IRGenFunction &IGF,
                                                VarDecl *var,
                                                const TypeInfo &type) {
  /// Get the global variable.
  Address addr = IGF.IGM.getAddrOfGlobalVariable(var);

  // Add a zero-initializer.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.getStorageType()));

  // TODO: global destructors?
  return OwnedAddress(addr, IGF.IGM.RefCountedNull);
}

/// Create an allocation for an empty object.
static OwnedAddress createEmptyAlloca(IRGenModule &IGM, const TypeInfo &type) {
  llvm::Value *badPointer =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return OwnedAddress(Address(badPointer, type.StorageAlignment),
                      IGM.RefCountedNull);
}

/// Allocate an object in local scope.
OwnedAddress
Initialization::emitLocalAllocation(IRGenFunction &IGF, Object object,
                                    OnHeap_t allocateOnHeap,
                                    const TypeInfo &type, const Twine &name) {

  // If the type is known to be empty, don't actually allocate anything.
  if (type.isEmpty(ResilienceScope::Local)) {
    OwnedAddress addr = createEmptyAlloca(IGF.IGM, type);
    markAllocated(IGF, object, addr, IRGenFunction::CleanupsDepth::invalid());
    return addr;
  }

  // If the object does not need to be allocated on the heap,
  // allocate it on the stack.
  if (!allocateOnHeap) {
    Address rawAddr =
      IGF.createAlloca(type.getStorageType(), type.StorageAlignment, name);
    // TODO: lifetime intrinsics?

    OwnedAddress addr(rawAddr, IGF.IGM.RefCountedNull);
    markAllocated(IGF, object, addr, IRGenFunction::CleanupsDepth::invalid());
    return addr;
  }

  // Lay out the type as a heap object.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, &type);
  assert(!layout.empty() && "non-empty type had empty layout?");
  auto &elt = layout.getElements()[0];

  // Allocate a new object.
  // TODO: lifetime intrinsics?
  llvm:: Value *allocation = IGF.emitUnmanagedAlloc(layout, name + ".alloc");

  // Cast and GEP down to the element.
  Address rawAddr = layout.emitCastOfAlloc(IGF, allocation);
  rawAddr = elt.project(IGF, rawAddr, name);

  // Push a cleanup to dealloc the allocation.
  IRGenFunction::CleanupsDepth deallocCleanup
    = IGF.pushDeallocCleanup(allocation);

  OwnedAddress addr(rawAddr, allocation);
  markAllocated(IGF, object, addr, deallocCleanup);
  return addr;
}

static void maybeSetCleanupState(IRGenFunction &IGF,
                                 IRGenFunction::CleanupsDepth maybeCleanup,
                                 CleanupState newState) {
  if (maybeCleanup.isValid())
    IGF.setCleanupState(maybeCleanup, newState);
}

/// Mark that a value has reached its initialization point.
void Initialization::markInitialized(IRGenFunction &IGF, Object object) {
  auto it = Records.find(object.Opaque);
  assert(it != Records.end());

  // Deactivate the dealloc cleanup.
  maybeSetCleanupState(IGF, it->second.DeallocCleanup, CleanupState::Dead);

  // Activate the destroy cleanup.
  maybeSetCleanupState(IGF, it->second.DestroyCleanup, CleanupState::Active);
}

/// Emit an expression as an initializer for the given location.
void Initialization::emitInit(IRGenFunction &IGF, Object object,
                              Address addr, Expr *E, const TypeInfo &type) {
  // For now, just explode and initialize from that.  This is a
  // bit lame; in particular, we should evaluate indirect call
  // results directly into place.
  Explosion explosion(ExplosionKind::Maximal);
  IGF.emitRValue(E, explosion);
  type.initialize(IGF, explosion, addr);

  // Mark as initialized.  This assumes that calls to
  // TypeInfo::initialize are atomic w.r.t. exceptions and
  // other control flow.
  markInitialized(IGF, object);
}

/// Emit an r-value directly into memory as an initialization.
/// Enable the given cleanup as soon as it's complete.
void IRGenFunction::emitInit(Expr *E, Address addr, const TypeInfo &type) {
  Initialization I;
  auto object = I.getObjectForTemporary();
  I.registerObjectWithoutDestroy(object);
  I.emitInit(*this, object, addr, E, type);
}

/// Zero-initialize the given memory location.
void Initialization::emitZeroInit(IRGenFunction &IGF, Object object,
                                  Address addr, const TypeInfo &type) {
  // Zero-initialization always has trivial outwards control flow; go
  // ahead and immediately switch the cleanups.
  markInitialized(IGF, object);

  // No work is necessary if the type is empty or the address is global.
  if (type.isEmpty(ResilienceScope::Local) ||
      isa<llvm::Constant>(addr.getAddress()))
    return;

  ExplosionSchema schema(ExplosionKind::Maximal);
  type.getSchema(schema);

  // Try to fill the value in with stores if that doesn't make for a
  // ridiculous amount of IR.  This is impossible if the schema
  // contains an aggregate;  otherwise, 4 is just a number.
  if (!schema.containsAggregate() && schema.size() <= 4) {
    Explosion explosion(schema.getKind());
    for (auto elt : schema) {
      explosion.addUnmanaged(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    type.initialize(IGF, explosion, addr);
    return;
  }

  // Otherwise, just do a memset.
  IGF.Builder.CreateMemSet(IGF.Builder.CreateBitCast(addr.getAddress(),
                                                     IGF.IGM.Int8PtrTy),
                           IGF.Builder.getInt8(0),
                           IGF.Builder.getInt64(type.StorageSize.getValue()),
                           addr.getAlignment().getValue(),
                           /*volatile*/ false);
}

namespace {
  /// A visitor for initializing a pattern with an exploded r-value.
  struct InitPatternWithRValue
      : public irgen::PatternVisitor<InitPatternWithRValue> {
    IRGenFunction &IGF;
    Initialization &I;
    Explosion &Values;

    InitPatternWithRValue(IRGenFunction &IGF, Initialization &I,
                          Explosion &values)
      : IGF(IGF), I(I), Values(values) {}

    void visitAnyPattern(AnyPattern *P) {
      const TypeInfo &type = IGF.getFragileTypeInfo(P->getType());
      Values.ignoreAndDestroy(IGF, type.getExplosionSize(Values.getKind()));
    }

    void visitTuplePattern(TuplePattern *TP) {
      for (auto &field : TP->getFields())
        visit(field.getPattern());
    }

    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();
      const TypeInfo &type = IGF.getFragileTypeInfo(var->getType());
      Address addr = I.emitVariable(IGF, var, type);
      type.initialize(IGF, Values, addr);
    }
  };

  /// A visitor for initializing a pattern with an expression.  This does
  /// not occur within the initializer's full-expression;  that should
  /// be pushed at the appropriate moment.
  struct InitPatternWithExpr
      : public irgen::PatternVisitor<InitPatternWithExpr> {
    IRGenFunction &IGF;
    Initialization &I;
    Expr *Init;
    InitPatternWithExpr(IRGenFunction &IGF, Initialization &I, Expr *init)
      : IGF(IGF), I(I), Init(init) {}

    // Bind to a wildcard pattern by ignoring the initializer.
    void visitAnyPattern(AnyPattern *P) {
      if (Init) {
        FullExpr scope(IGF);
        IGF.emitIgnored(Init);
      }
    }

    // Bind to a named pattern by emitting the initializer into place.
    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();
      const TypeInfo &type = IGF.getFragileTypeInfo(var->getType());

      FullExpr scope(IGF);
      Address addr = I.emitVariable(IGF, var, type);

      if (Init) {
        I.emitInit(IGF, I.getObjectForDecl(var), addr, Init, type);
      } else {
        I.emitZeroInit(IGF, I.getObjectForDecl(var), addr, type);
      }
    }

    /// Try to initialize the distinct elements of a tuple pattern
    /// independently.
    bool tryInitTupleElementsIndependently(TuplePattern *P) {
      // Skip sugar.
      Expr *E = Init->getSemanticsProvidingExpr();

      // If we can break the initializer down into a literal, that's great.
      if (TupleExpr *literal = dyn_cast<TupleExpr>(E)) {
        assert(literal->getNumElements() == P->getNumFields());
        
        for (unsigned i = 0, e = literal->getNumElements(); i != e; ++i) {
          Init = literal->getElement(i);
          assert(Init && "no expression for tuple element!");
          visit(P->getFields()[i].getPattern());
        }
        return true;
      }

      // TODO: there are other possibilities here, e.g. with shuffles
      // around tuple literals.
      return false;
    }

    // Bind to a tuple pattern by first trying to see if we can emit
    // the initializers independently.
    void visitTuplePattern(TuplePattern *P) {
      // If we have no initializer, just emit the subpatterns using
      // the missing initializer.
      if (!Init) {
        for (auto &elt : P->getFields())
          visit(elt.getPattern());
        return;
      }

      // Otherwise, try to initialize the tuple elements independently.
      if (tryInitTupleElementsIndependently(P))
        return;

      // Otherwise, a single expression will initialize multiple
      // tuple elements.
      FullExpr scope(IGF);

      const TypeInfo &TI = IGF.getFragileTypeInfo(P->getType());

      // If we can emit the expression as an address, we can copy from
      // there into the tuple.
      if (Optional<Address> addr = IGF.tryEmitAsAddress(Init, TI)) {
        emitTuplePatternInitFromAddress(IGF, I, addr.getValue(), P, TI);
        return;
      }

      // Otherwise, we have to explode.
      Explosion explosion(ExplosionKind::Maximal);
      IGF.emitRValue(Init, explosion);
      InitPatternWithRValue(IGF, I, explosion).visitTuplePattern(P);
    }
  };

  /// A visitor for registering all the destroy cleanups
  /// required for the variables in a pattern.
  struct RegisterPattern : irgen::PatternVisitor<RegisterPattern> {
    IRGenFunction &IGF;
    Initialization &I;
    RegisterPattern(IRGenFunction &IGF, Initialization &I) : IGF(IGF), I(I) {}

    void visitTuplePattern(TuplePattern *P) {
      for (auto &elt : P->getFields())
        visit(elt.getPattern());
    }
    void visitAnyPattern(AnyPattern *P) {}

    void visitNamedPattern(NamedPattern *P) {
      VarDecl *var = P->getDecl();

      // There's never a destroy cleanup for global declarations.
      if (var->isModuleScope())
        return I.registerObjectWithoutDestroy(I.getObjectForDecl(var));

      const TypeInfo &varTI = IGF.getFragileTypeInfo(var->getType());
      I.registerObject(IGF, I.getObjectForDecl(var), isOnHeap(var), varTI);
    }
  };
}

void IRGenFunction::emitPatternBindingDecl(PatternBindingDecl *D) {
  Initialization I;
  RegisterPattern(*this, I).visit(D->getPattern());
  InitPatternWithExpr(*this, I, D->getInit()).visit(D->getPattern());
}
