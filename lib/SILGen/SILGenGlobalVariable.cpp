//===--- SILGenGlobalVariable.cpp - Lowering for global variables ---------===//
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

#include "SILGenFunction.h"
#include "ManagedValue.h"
#include "Scope.h"
#include "swift/AST/AST.h"
#include "swift/AST/Mangle.h"
#include "swift/SIL/FormalLinkage.h"

using namespace swift;
using namespace Mangle;
using namespace Lowering;

/// Get or create SILGlobalVariable for a given global VarDecl.
SILGlobalVariable *SILGenModule::getSILGlobalVariable(VarDecl *gDecl,
                                                      ForDefinition_t forDef) {
  // First, get a mangled name for the declaration.
  llvm::SmallString<32> mangledName; {
    llvm::raw_svector_ostream buffer(mangledName);
    Mangler mangler(buffer);
    mangler.mangleGlobalVariableFull(gDecl);
  }

  // Check if it is already created, and update linkage if necessary.
  if (auto gv = M.lookUpGlobalVariable(mangledName)) {
    // Update the SILLinkage here if this is a definition.
    if (forDef == ForDefinition) {
      gv->setLinkage(getSILLinkage(getDeclLinkage(gDecl), ForDefinition));
      gv->setDeclaration(false);
    }
    return gv;
  }

  // Get the linkage for SILGlobalVariable.
  SILLinkage link = getSILLinkage(getDeclLinkage(gDecl), forDef);
  SILType silTy = M.Types.getLoweredTypeOfGlobal(gDecl);

  auto *silGlobal = SILGlobalVariable::create(M, link,
                                              makeModuleFragile ? IsFragile : IsNotFragile,
                                              mangledName, silTy,
                                              None, gDecl);
  silGlobal->setDeclaration(!forDef);

  return silGlobal;
}

/// True if the global stored property requires lazy initialization.
static bool isGlobalLazilyInitialized(VarDecl *var) {
  assert(!var->getDeclContext()->isLocalContext() &&
         "not a global variable!");
  assert(var->hasStorage() &&
         "not a stored global variable!");

  // Imports from C are never lazily initialized.
  if (var->hasClangNode())
    return false;

  if (var->isDebuggerVar())
    return false;

  // Top-level global variables in the main source file and in the REPL are not
  // lazily initialized.
  auto sourceFileContext = dyn_cast<SourceFile>(var->getDeclContext());
  if (!sourceFileContext)
    return true;

  return !sourceFileContext->isScriptMode();
}

ManagedValue
SILGenFunction::emitGlobalVariableRef(SILLocation loc, VarDecl *var) {
  assert(!VarLocs.count(var));

  if (isGlobalLazilyInitialized(var)) {
    // Call the global accessor to get the variable's address.
    SILFunction *accessorFn = SGM.getFunction(
                            SILDeclRef(var, SILDeclRef::Kind::GlobalAccessor),
                                                  NotForDefinition);
    SILValue accessor = B.createFunctionRef(loc, accessorFn);
    auto accessorTy = accessor.getType().castTo<SILFunctionType>();
    (void)accessorTy;
    assert(!accessorTy->isPolymorphic()
           && "generic global variable accessors not yet implemented");
    SILValue addr = B.createApply(loc, accessor, accessor.getType(),
                              accessor.getType().castTo<SILFunctionType>()
                                      ->getResult().getSILType(),
                              {}, {});
    // FIXME: It'd be nice if the result of the accessor was natively an
    // address.
    addr = B.createPointerToAddress(loc, addr,
                             getLoweredType(var->getType()).getAddressType());
    return ManagedValue::forLValue(addr);
  }

  // Global variables can be accessed directly with global_addr.  Emit this
  // instruction into the prolog of the function so we can memoize/CSE it in
  // VarLocs.
  auto &entryBB = getFunction().getBlocks().front();
  SILGenBuilder prologueB(*this, &entryBB, entryBB.begin());
  prologueB.setTrackingList(B.getTrackingList());

  auto *silG = SGM.getSILGlobalVariable(var, NotForDefinition);
  SILValue addr = prologueB.createGlobalAddr(var, silG);

  VarLocs[var] = SILGenFunction::VarLoc::get(addr);
  return ManagedValue::forLValue(addr);
}

//===----------------------------------------------------------------------===//
// Global initialization
//===----------------------------------------------------------------------===//

namespace {

/// A visitor for traversing a pattern, creating
/// global accessor functions for all of the global variables declared inside.
struct GenGlobalAccessors : public PatternVisitor<GenGlobalAccessors>
{
  /// The module generator.
  SILGenModule &SGM;
  /// The Builtin.once token guarding the global initialization.
  SILGlobalVariable *OnceToken;
  /// The function containing the initialization code.
  SILFunction *OnceFunc;

  /// A reference to the Builtin.once declaration.
  FuncDecl *BuiltinOnceDecl;

  GenGlobalAccessors(SILGenModule &SGM,
                     SILGlobalVariable *OnceToken,
                     SILFunction *OnceFunc)
    : SGM(SGM), OnceToken(OnceToken), OnceFunc(OnceFunc)
  {
    // Find Builtin.once.
    auto &C = SGM.M.getASTContext();
    SmallVector<ValueDecl*, 2> found;
    C.TheBuiltinModule
      ->lookupValue({}, C.getIdentifier("once"),
                    NLKind::QualifiedLookup, found);

    assert(found.size() == 1 && "didn't find Builtin.once?!");

    BuiltinOnceDecl = cast<FuncDecl>(found[0]);
  }

  // Walk through non-binding patterns.
  void visitParenPattern(ParenPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitTypedPattern(TypedPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitVarPattern(VarPattern *P) {
    return visit(P->getSubPattern());
  }
  void visitTuplePattern(TuplePattern *P) {
    for (auto &elt : P->getElements())
      visit(elt.getPattern());
  }
  void visitAnyPattern(AnyPattern *P) {}

  // When we see a variable binding, emit its global accessor.
  void visitNamedPattern(NamedPattern *P) {
    SGM.emitGlobalAccessor(P->getDecl(), OnceToken, OnceFunc);
  }

#define INVALID_PATTERN(Id, Parent) \
  void visit##Id##Pattern(Id##Pattern *) { \
    llvm_unreachable("pattern not valid in argument or var binding"); \
  }
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) INVALID_PATTERN(Id, Parent)
#include "swift/AST/PatternNodes.def"
#undef INVALID_PATTERN
};

} // end anonymous namespace

/// Emit a global initialization.
void SILGenModule::emitGlobalInitialization(PatternBindingDecl *pd,
                                            unsigned pbdEntry) {
  // Generic and dynamic static properties require lazy initialization, which
  // isn't implemented yet.
  if (pd->isStatic()) {
    auto theType = pd->getDeclContext()->getDeclaredTypeInContext();
    assert(!theType->is<BoundGenericType>()
           && "generic static properties not implemented");
    (void)theType;
  }

  // Emit the lazy initialization token for the initialization expression.
  auto counter = anonymousSymbolCounter++;

  // Pick one variable of the pattern. Usually it's only one variable, but it
  // can also be something like: var (a, b) = ...
  Pattern *pattern = pd->getPattern(pbdEntry);
  VarDecl *varDecl = nullptr;
  pattern->forEachVariable([&](VarDecl *D) {
    varDecl = D;
  });
  assert(varDecl);

  llvm::SmallString<20> onceTokenBuffer; {
    llvm::raw_svector_ostream onceTokenStream(onceTokenBuffer);
    Mangler tokenMangler(onceTokenStream);
    tokenMangler.mangleGlobalInit(varDecl, counter, false);
  }

  auto onceTy = BuiltinIntegerType::getWordType(M.getASTContext());
  auto onceSILTy
    = SILType::getPrimitiveObjectType(onceTy->getCanonicalType());

  // TODO: include the module in the onceToken's name mangling.
  // Then we can make it fragile.
  auto onceToken = SILGlobalVariable::create(M, SILLinkage::Private,
                                             makeModuleFragile,
                                             onceTokenBuffer, onceSILTy);
  onceToken->setDeclaration(false);

  // Emit the initialization code into a function.
  llvm::SmallString<20> onceFuncBuffer;
  llvm::raw_svector_ostream onceFuncStream(onceFuncBuffer);
  Mangler funcMangler(onceFuncStream);
  funcMangler.mangleGlobalInit(varDecl, counter, true);

  SILFunction *onceFunc = emitLazyGlobalInitializer(onceFuncStream.str(), pd,
                                                    pbdEntry);

  // Generate accessor functions for all of the declared variables, which
  // Builtin.once the lazy global initializer we just generated then return
  // the address of the individual variable.
  GenGlobalAccessors(*this, onceToken, onceFunc)
    .visit(pd->getPattern(pbdEntry));
}

void SILGenFunction::emitLazyGlobalInitializer(PatternBindingDecl *binding) {
  {
    Scope scope(Cleanups, binding);

    // Emit the initialization sequence.
    visit(binding);
  }

  // Return void.
  auto ret = emitEmptyTuple(binding);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(binding), ret);
}

static void emitOnceCall(SILGenFunction &gen, VarDecl *global,
                         SILGlobalVariable *onceToken, SILFunction *onceFunc) {
  SILType rawPointerSILTy
    = gen.getLoweredLoadableType(gen.getASTContext().TheRawPointerType);

  // Emit a reference to the global token.
  SILValue onceTokenAddr = gen.B.createGlobalAddr(global, onceToken);
  onceTokenAddr = gen.B.createAddressToPointer(global, onceTokenAddr,
                                               rawPointerSILTy);

  // Emit a reference to the function to execute.
  SILValue onceFuncRef = gen.B.createFunctionRef(global, onceFunc);

  // Call Builtin.once.
  SILValue onceArgs[] = {onceTokenAddr, onceFuncRef};
  gen.B.createBuiltin(global, gen.getASTContext().getIdentifier("once"),
                      gen.SGM.Types.getEmptyTupleType(), {}, onceArgs);
}

void SILGenFunction::emitGlobalAccessor(VarDecl *global,
                                        SILGlobalVariable *onceToken,
                                        SILFunction *onceFunc) {
  emitOnceCall(*this, global, onceToken, onceFunc);

  // Return the address of the global variable.
  // FIXME: It'd be nice to be able to return a SIL address directly.
  auto *silG = SGM.getSILGlobalVariable(global, NotForDefinition);
  SILValue addr = B.createGlobalAddr(global, silG);

  SILType rawPointerSILTy
    = getLoweredLoadableType(getASTContext().TheRawPointerType);
  addr = B.createAddressToPointer(global, addr, rawPointerSILTy);
  B.createReturn(global, addr);
  if (!MainScope)
    MainScope = F.getDebugScope();
  setDebugScopeForInsertedInstrs(MainScope);
}

void SILGenFunction::emitGlobalGetter(VarDecl *global,
                                      SILGlobalVariable *onceToken,
                                      SILFunction *onceFunc) {
  emitOnceCall(*this, global, onceToken, onceFunc);

  auto *silG = SGM.getSILGlobalVariable(global, NotForDefinition);
  SILValue addr = B.createGlobalAddr(global, silG);

  auto refType = global->getType()->getCanonicalType();
  ManagedValue value = emitLoad(global, addr, getTypeLowering(refType),
                                SGFContext(), IsNotTake);
  SILValue result = value.forward(*this);
  B.createReturn(global, result);
}

