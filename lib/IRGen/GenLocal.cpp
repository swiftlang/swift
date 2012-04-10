//===--- GenLocal.cpp - IR Generation for Local Declarations --------------===//
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
//  This file implements IR generation for local declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "GenHeap.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitLocal(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Import:
    llvm_unreachable("declaration cannot appear in local scope");

  // Type aliases require IR-gen support if they're really
  // struct/oneof declarations.
  case DeclKind::TypeAlias:
    return IGM.emitTypeAlias(cast<TypeAliasDecl>(D)->getUnderlyingType());

  case DeclKind::OneOfElement:
    // no IR generation support required.
    return;

  case DeclKind::Var:
    return emitLocalVar(cast<VarDecl>(D));

  case DeclKind::Extension:
    unimplemented(D->getLocStart(), "local extension emission");
    return;

  case DeclKind::Func:
    unimplemented(D->getLocStart(), "local function emission");
    return;

  case DeclKind::PatternBinding:
    emitPatternBindingInit(cast<PatternBindingDecl>(D), /*isGlobal*/false);
    return;

  }
  llvm_unreachable("bad declaration kind!");
}

/// emitLocalVar - Emit a local variable.
void IRGenFunction::emitLocalVar(VarDecl *var) {
  const TypeInfo &typeInfo = getFragileTypeInfo(var->getType());

  OwnedAddress addr = createScopeAlloca(typeInfo,
                                        var->hasFixedLifetime()
                                          ? NotOnHeap : OnHeap,
                                        var->getName().str());

  setLocal(var, addr);
};

OwnedAddress IRGenFunction::getLocal(ValueDecl *D) {
  auto I = Locals.find(D);
  assert(I != Locals.end() && "no entry in local map!");
  return I->second.Var.Addr;
}

void IRGenFunction::setLocal(ValueDecl *D, OwnedAddress addr) {
  assert(!Locals.count(D));

  std::pair<ValueDecl*, LocalRecord> entry;
  entry.first = D;
  entry.second.Var.Addr = addr;

  Locals.insert(entry);
}

/// Create an allocation for an empty object.
static OwnedAddress createEmptyAlloca(IRGenModule &IGM, const TypeInfo &type) {
  llvm::Value *badPointer =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return OwnedAddress(Address(badPointer, type.StorageAlignment),
                      IGM.RefCountedNull);
}

/// Create an allocation on the stack.
static OwnedAddress createStackAlloca(IRGenFunction &IGF,
                                      const TypeInfo &type,
                                      const llvm::Twine &name,
                                      llvm::Instruction *allocaIP) {
  llvm::AllocaInst *alloca =
    new llvm::AllocaInst(type.getStorageType(), name, allocaIP);
  alloca->setAlignment(type.StorageAlignment.getValue());

  // TODO: lifetime intrinsics.
  return OwnedAddress(Address(alloca, type.StorageAlignment),
                      IGF.IGM.RefCountedNull);
}

/// Create an allocation on the heap.
static OwnedAddress createHeapAlloca(IRGenFunction &IGF,
                                     const TypeInfo &type,
                                     const llvm::Twine &name) {
  // Create the type as appropriate.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, &type);
  assert(!layout.empty() && "non-empty type had empty layout?");
  auto &elt = layout.getElements()[0];

  // Allocate a new object.  FIXME: exceptions.
  ManagedValue allocation = IGF.emitAlloc(layout, name + ".alloc");

  // Cast and GEP down to the element.
  Address address = layout.emitCastOfAlloc(IGF, allocation.getValue());
  address = elt.project(IGF, address, name);

  // TODO: lifetime intrinsics.
  return OwnedAddress(address, allocation.getValue());
}

/// Common code for creating an allocation on the stack or heap.
static OwnedAddress createAlloca(IRGenFunction &IGF, 
                                 const TypeInfo &type,
                                 OnHeap_t onHeap,
                                 const llvm::Twine &name,
                                 llvm::Instruction *allocaIP) {
  assert(allocaIP && "alloca insertion point has not been initialized!");

  // If the type is known to be empty, don't actually allocate anything.
  if (type.isEmpty(ResilienceScope::Local))
    return createEmptyAlloca(IGF.IGM, type);

  if (onHeap) return createHeapAlloca(IGF, type, name);
  return createStackAlloca(IGF, type, name, allocaIP);
}

/// Create an alloca whose lifetime is the duration of the current
/// full-expression.
OwnedAddress IRGenFunction::createFullExprAlloca(const TypeInfo &type,
                                                 OnHeap_t onHeap,
                                                 const llvm::Twine &name) {
  return createAlloca(*this, type, onHeap, name, AllocaIP);
}

/// Create an alloca whose lifetime is the duration of the current
/// scope.
OwnedAddress IRGenFunction::createScopeAlloca(const TypeInfo &type,
                                              OnHeap_t onHeap,
                                              const llvm::Twine &name) {
  return createAlloca(*this, type, onHeap, name, AllocaIP);
}
