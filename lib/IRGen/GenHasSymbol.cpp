//===--- GenHasSymbol.cpp - IR Generation for #_hasSymbol queries ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for `if #_hasSymbol` condition queries.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTMangler.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/IRGen/IRSymbolVisitor.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILFunctionBuilder.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILSymbolVisitor.h"

#include "GenDecl.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

/// Wrapper for IRGenModule::getAddrOfLLVMVariable() that also handles a few
/// additional types of entities that the main utility cannot.
static llvm::Constant *getAddrOfLLVMVariable(IRGenModule &IGM,
                                             LinkEntity entity) {
  if (entity.isTypeMetadataAccessFunction())
    return IGM.getAddrOfTypeMetadataAccessFunction(entity.getType(),
                                                   NotForDefinition);
  if (entity.isDispatchThunk())
    return IGM.getAddrOfDispatchThunk(entity.getSILDeclRef(), NotForDefinition);

  return IGM.getAddrOfLLVMVariable(entity, ConstantInit(), DebugTypeInfo());
}

class HasSymbolIRGenVisitor : public IRSymbolVisitor {
  IRGenModule &IGM;
  llvm::SmallVector<llvm::Constant *, 4> &Addrs;

  void addFunction(StringRef name) {
    SILFunction *silFn = IGM.getSILModule().lookUpFunction(name);
    // Definitions for each SIL function should have been emitted by SILGen.
    assert(silFn && "missing SIL function?");
    if (silFn) {
      Addrs.emplace_back(IGM.getAddrOfSILFunction(silFn, NotForDefinition));
    }
  }

public:
  HasSymbolIRGenVisitor(IRGenModule &IGM,
                        llvm::SmallVector<llvm::Constant *, 4> &Addrs)
      : IGM{IGM}, Addrs{Addrs} {};

  void addFunction(SILDeclRef declRef) override {
    addFunction(declRef.mangle());
  }

  void addFunction(StringRef name, SILDeclRef declRef) override {
    addFunction(name);
  }

  void addGlobalVar(VarDecl *VD) override {
    // FIXME: Handle global vars
    llvm::report_fatal_error("unhandled global var");
  }

  void addLinkEntity(LinkEntity entity) override {
    auto constant = getAddrOfLLVMVariable(IGM, entity);
    auto global = cast<llvm::GlobalValue>(constant);
    Addrs.emplace_back(global);
  }

  void addProtocolWitnessThunk(RootProtocolConformance *C,
                               ValueDecl *requirementDecl) override {
    // FIXME: Handle protocol witness thunks
    llvm::report_fatal_error("unhandled protocol witness thunk");
  }
};

llvm::Function *IRGenModule::emitHasSymbolFunction(ValueDecl *decl) {

  PrettyStackTraceDecl trace("emitting #_hasSymbol query for", decl);
  Mangle::ASTMangler mangler;

  auto func = cast<llvm::Function>(getOrCreateHelperFunction(
      mangler.mangleHasSymbolQuery(decl), Int1Ty, {},
      [decl, this](IRGenFunction &IGF) {
        SILSymbolVisitorOptions opts;
        opts.VisitMembers = false;
        auto silCtx = SILSymbolVisitorContext(getSwiftModule(), opts);
        auto linkInfo = UniversalLinkageInfo(*this);
        auto symbolVisitorCtx = IRSymbolVisitorContext(linkInfo, silCtx);
        auto &Builder = IGF.Builder;
        llvm::SmallVector<llvm::Constant *, 4> addrs;
        HasSymbolIRGenVisitor(*this, addrs).visit(decl, symbolVisitorCtx);

        llvm::Value *ret = nullptr;
        for (llvm::Constant *addr : addrs) {
          assert(cast<llvm::GlobalValue>(addr)->hasExternalWeakLinkage());

          auto isNonNull = IGF.Builder.CreateIsNotNull(addr);
          ret = (ret) ? IGF.Builder.CreateAnd(ret, isNonNull) : isNonNull;
        }

        if (ret) {
          Builder.CreateRet(ret);
        } else {
          // There were no addresses produced by the visitor, return true.
          Builder.CreateRet(llvm::ConstantInt::get(Int1Ty, 1));
        }
      },
      /*IsNoInline*/ false));

  func->setDoesNotThrow();
  func->setCallingConv(DefaultCC);
  func->addFnAttr(llvm::Attribute::ReadOnly);

  return func;
}
