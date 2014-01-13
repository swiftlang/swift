//===--- GenDecl.cpp - IR Generation for Declarations ---------------------===//
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
//  This file implements IR generation for local and global
//  declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeMemberVisitor.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Path.h"

#include "CallingConvention.h"
#include "Explosion.h"
#include "FormalType.h"
#include "GenClass.h"
#include "GenObjC.h"
#include "GenMeta.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

/// Generates a function to call +load on all the given classes.
static llvm::Function *emitObjCClassInitializer(IRGenModule &IGM,
                                                ArrayRef<llvm::WeakVH> classes){
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(llvm::Type::getVoidTy(IGM.LLVMContext), false);
  llvm::Function *initFn =
    llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                           "_swift_initObjCClasses", &IGM.Module);

  IRGenFunction initIGF(IGM, initFn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(initIGF, initFn);

  llvm::Value *loadSel = initIGF.emitObjCSelectorRefLoad("load");

  llvm::Type *msgSendParams[] = {
    IGM.ObjCPtrTy,
    IGM.ObjCSELTy
  };
  llvm::FunctionType *msgSendType =
    llvm::FunctionType::get(llvm::Type::getVoidTy(IGM.LLVMContext),
                            msgSendParams, false);
  llvm::Constant *msgSend =
    llvm::ConstantExpr::getBitCast(IGM.getObjCMsgSendFn(),
                                   msgSendType->getPointerTo());

  for (auto nextClass : classes) {
    llvm::Constant *receiver =
      llvm::ConstantExpr::getBitCast(cast<llvm::Constant>(nextClass),
                                     IGM.ObjCPtrTy);
    initIGF.Builder.CreateCall2(msgSend, receiver, loadSel);
  }

  initIGF.Builder.CreateRetVoid();

  return initFn;
}

namespace {
  
class CategoryInitializerVisitor
  : public ClassMemberVisitor<CategoryInitializerVisitor>
{
  IRGenFunction &IGF;
  
  llvm::Constant *class_replaceMethod;
  llvm::Constant *class_addProtocol;
  
  llvm::Constant *classMetadata;
  llvm::Constant *metaclassMetadata;
  
public:
  CategoryInitializerVisitor(IRGenFunction &IGF, ExtensionDecl *ext)
    : IGF(IGF)
  {
    class_replaceMethod = IGF.IGM.getClassReplaceMethodFn();
    class_addProtocol = IGF.IGM.getClassAddProtocolFn();

    CanType origTy = ext->getDeclaredTypeOfContext()->getCanonicalType();
    classMetadata = tryEmitConstantHeapMetadataRef(IGF.IGM, origTy);
    assert(classMetadata &&
           "extended objc class doesn't have constant metadata?!");
    classMetadata = llvm::ConstantExpr::getBitCast(classMetadata,
                                                   IGF.IGM.TypeMetadataPtrTy);
    metaclassMetadata = IGF.IGM.getAddrOfMetaclassObject(
                                       origTy->getClassOrBoundGenericClass(),
                                                         NotForDefinition);
    metaclassMetadata = llvm::ConstantExpr::getBitCast(metaclassMetadata,
                                                   IGF.IGM.TypeMetadataPtrTy);

    // Register ObjC protocol conformances.
    for (auto *p : ext->getProtocols()) {
      if (!p->isObjC())
        continue;
      
      auto proto = IGF.IGM.getAddrOfObjCProtocolRecord(p, NotForDefinition);
      IGF.Builder.CreateCall2(class_addProtocol, classMetadata, proto);
    }
  }
  
  void visitMembers(ExtensionDecl *ext) {
    for (Decl *member : ext->getMembers())
      visit(member);
  }
  
  void visitFuncDecl(FuncDecl *method) {
    if (!requiresObjCMethodDescriptor(method)) return;
    llvm::Constant *name, *imp, *types;
    emitObjCMethodDescriptorParts(IGF.IGM, method, name, types, imp);
    
    // When generating JIT'd code, we need to call sel_registerName() to force
    // the runtime to unique the selector.
    llvm::Value *sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                              name);
    
    llvm::Value *args[] = {
      method->isStatic() ? metaclassMetadata : classMetadata,
      sel,
      imp,
      types
    };
    
    IGF.Builder.CreateCall(class_replaceMethod, args);
  }

  void visitConstructorDecl(ConstructorDecl *constructor) {
    if (!requiresObjCMethodDescriptor(constructor)) return;
    llvm::Constant *name, *imp, *types;
    emitObjCMethodDescriptorParts(IGF.IGM, constructor, name, types, imp);

    // When generating JIT'd code, we need to call sel_registerName() to force
    // the runtime to unique the selector.
    llvm::Value *sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                              name);

    llvm::Value *args[] = {
      classMetadata,
      sel,
      imp,
      types
    };

    IGF.Builder.CreateCall(class_replaceMethod, args);
  }

  void visitVarDecl(VarDecl *prop) {
    if (!requiresObjCPropertyDescriptor(prop)) return;
    
    llvm::Constant *name, *imp, *types;
    emitObjCGetterDescriptorParts(IGF.IGM, prop,
                                  name, types, imp);
    // When generating JIT'd code, we need to call sel_registerName() to force
    // the runtime to unique the selector.
    llvm::Value *sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                              name);
    llvm::Value *getterArgs[] = {classMetadata, sel, imp, types};
    IGF.Builder.CreateCall(class_replaceMethod, getterArgs);

    if (prop->isSettable()) {
      emitObjCSetterDescriptorParts(IGF.IGM, prop,
                                    name, types, imp);
      sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                   name);
      llvm::Value *setterArgs[] = {classMetadata, sel, imp, types};
      
      IGF.Builder.CreateCall(class_replaceMethod, setterArgs);
    }

    // FIXME: register property metadata in addition to the methods.
  }

  void visitSubscriptDecl(SubscriptDecl *subscript) {
    if (!requiresObjCSubscriptDescriptor(subscript)) return;
    
    llvm::Constant *name, *imp, *types;
    emitObjCGetterDescriptorParts(IGF.IGM, subscript,
                                  name, types, imp);
    // When generating JIT'd code, we need to call sel_registerName() to force
    // the runtime to unique the selector.
    llvm::Value *sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                              name);
    llvm::Value *getterArgs[] = {classMetadata, sel, imp, types};
    IGF.Builder.CreateCall(class_replaceMethod, getterArgs);

    if (subscript->isSettable()) {
      emitObjCSetterDescriptorParts(IGF.IGM, subscript,
                                    name, types, imp);
      sel = IGF.Builder.CreateCall(IGF.IGM.getObjCSelRegisterNameFn(),
                                   name);
      llvm::Value *setterArgs[] = {classMetadata, sel, imp, types};
      
      IGF.Builder.CreateCall(class_replaceMethod, setterArgs);
    }
  }
};

} // end anonymous namespace

static llvm::Function *emitObjCCategoryInitializer(IRGenModule &IGM,
                                         ArrayRef<ExtensionDecl*> categories) {
  llvm::FunctionType *fnType =
    llvm::FunctionType::get(llvm::Type::getVoidTy(IGM.LLVMContext), false);
  llvm::Function *initFn =
    llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                           "_swift_initObjCCategories", &IGM.Module);
  
  IRGenFunction initIGF(IGM, initFn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(initIGF, initFn);
  
  for (ExtensionDecl *ext : categories) {
    CategoryInitializerVisitor(initIGF, ext).visitMembers(ext);
  }
  
  initIGF.Builder.CreateRetVoid();
  return initFn;
}

namespace {
class PrettySourceFileEmission : public llvm::PrettyStackTraceEntry {
  const SourceFile &SF;
public:
  explicit PrettySourceFileEmission(const SourceFile &SF) : SF(SF) {}

  virtual void print(raw_ostream &os) const override {
    os << "While emitting IR for source file " << SF.getFilename() << '\n';
  }
};
} // end anonymous namespace

/// Emit all the top-level code in the source file.
void IRGenModule::emitSourceFile(SourceFile &SF, unsigned StartElem) {
  PrettySourceFileEmission StackEntry(SF);

  // Emit types and other global decls.
  for (unsigned i = StartElem, e = SF.Decls.size(); i != e; ++i)
    emitGlobalDecl(SF.Decls[i]);

  SF.forAllVisibleModules([&](swift::Module::ImportedModule import) {
    import.second->collectLinkLibraries([this](LinkLibrary linkLib) {
      this->addLinkLibrary(linkLib);
    });
  });

  // Library files have no entry point.
  if (!SF.isScriptMode()) {
    assert(!Module.getFunction(SWIFT_ENTRY_POINT_FUNCTION)
           && "libraries should not have an entry point");
    return;
  }
  
  // Emit main().
  // FIXME: We should only emit this in non-JIT modes.
  llvm::Function *topLevelCodeFn
    = Module.getFunction(SWIFT_ENTRY_POINT_FUNCTION);

  llvm::Type* argcArgvTypes[2] = {
    llvm::TypeBuilder<llvm::types::i<32>, true>::get(LLVMContext),
    llvm::TypeBuilder<llvm::types::i<8>**, true>::get(LLVMContext)
  };

  llvm::Function *mainFn =
    llvm::Function::Create(
      llvm::FunctionType::get(Int32Ty, argcArgvTypes, false),
        llvm::GlobalValue::ExternalLinkage, "main", &Module);

  IRGenFunction mainIGF(*this, mainFn);
  if (DebugInfo) {
    // Emit at least the return type.
    SILParameterInfo paramTy(CanType(BuiltinIntegerType::get(32, Context)),
                             ParameterConvention::Direct_Unowned);
    SILResultInfo retTy(TupleType::getEmpty(Context),
                        ResultConvention::Unowned);
    auto extInfo = SILFunctionType::ExtInfo(AbstractCC::Freestanding,
                                            /*thin*/ true,
                                            /*noreturn*/ false);
    auto fnTy = SILFunctionType::get(nullptr, extInfo,
                                     ParameterConvention::Direct_Unowned,
                                     paramTy, retTy, Context);
    auto silFnTy = SILType::getPrimitiveLocalStorageType(fnTy);
    DebugInfo->emitArtificialFunction(mainIGF, mainFn, silFnTy);
  }

  // Poke argc and argv into variables declared in the Swift stdlib
  auto args = mainFn->arg_begin();
  
  auto accessorTy
    = llvm::FunctionType::get(Int8PtrTy, {}, /*varArg*/ false);
  
  for (auto varNames : {
    // global accessor for swift.C_ARGC : CInt
    std::make_pair("argc", "_TFSsa6C_ARGCVSs5Int32"),
    // global accessor for swift.C_ARGV : UnsafePointer<CString>
    std::make_pair("argv", "_TFSsa6C_ARGVGVSs13UnsafePointerVSs7CString_")
  }) {
    StringRef fnParameterName;
    StringRef accessorName;
    std::tie(fnParameterName, accessorName) = varNames;
    
    llvm::Value* fnParameter = args++;
    fnParameter->setName(fnParameterName);

    // Access the address of the global.
    auto accessor = Module.getOrInsertFunction(accessorName, accessorTy);
    llvm::Value *ptr = mainIGF.Builder.CreateCall(accessor);
    // Cast to the type of the parameter we're storing.
    ptr = mainIGF.Builder.CreateBitCast(ptr,
                                  fnParameter->getType()->getPointerTo());
    mainIGF.Builder.CreateStore(fnParameter, ptr);
  }

  // Emit Objective-C runtime interop setup for immediate-mode code.
  if (ObjCInterop && Opts.UseJIT) {
    if (!ObjCClasses.empty()) {
      // Emit an initializer for the Objective-C classes.
      mainIGF.Builder.CreateCall(emitObjCClassInitializer(*this,ObjCClasses));
    }
    if (!ObjCCategoryDecls.empty()) {
      // Emit an initializer to add declarations from category decls.
      mainIGF.Builder.CreateCall(emitObjCCategoryInitializer(*this,
                                                          ObjCCategoryDecls));
    }
  }
  
  // Call the top-level code.
  if (topLevelCodeFn)
    mainIGF.Builder.CreateCall(topLevelCodeFn);
  mainIGF.Builder.CreateRet(mainIGF.Builder.getInt32(0));
}

/// Add the given global value to @llvm.used.
void IRGenModule::addUsedGlobal(llvm::GlobalValue *global) {
  assert(!global->isDeclaration() &&
         "Only globals with definition can force usage.");
  LLVMUsed.push_back(global);
}

/// Add the given global value to the Objective-C class list.
void IRGenModule::addObjCClass(llvm::Constant *classPtr) {
  ObjCClasses.push_back(classPtr);
}

/// Emit a global list, i.e. a global constant array holding all of a
/// list of values.  Generally these lists are for various LLVM
/// metadata or runtime purposes.
static void emitGlobalList(IRGenModule &IGM, ArrayRef<llvm::WeakVH> handles,
                           StringRef name, StringRef section,
                           llvm::GlobalValue::LinkageTypes linkage) {
  // Do nothing if the list is empty.
  if (handles.empty()) return;

  // For global lists that actually get linked (as opposed to notional
  // ones like @llvm.used), it's important to set an explicit alignment
  // so that the linker doesn't accidentally put padding in the list.
  Alignment alignment = IGM.getPointerAlignment();
  auto eltTy = IGM.Int8PtrTy;

  // We have an array of value handles, but we need an array of constants.
  SmallVector<llvm::Constant*, 8> elts;
  elts.reserve(handles.size());
  for (auto &handle : handles) {
    auto elt = cast<llvm::Constant>(&*handle);
    elt = llvm::ConstantExpr::getBitCast(elt, eltTy);
    elts.push_back(elt);
  }

  auto varTy = llvm::ArrayType::get(eltTy, elts.size());
  auto init = llvm::ConstantArray::get(varTy, elts);
  auto var = new llvm::GlobalVariable(IGM.Module, varTy, false, linkage,
                                      init, name);
  var->setSection(section);
  var->setAlignment(alignment.getValue());

  // Mark the variable as used if doesn't have external linkage.
  // (Note that we'd specifically like to not put @llvm.used in itself.)
  if (llvm::GlobalValue::isLocalLinkage(linkage))
    IGM.addUsedGlobal(var);
}

void IRGenModule::emitGlobalLists() {
  // Objective-C class references go in a variable with a meaningless
  // name but a magic section.
  emitGlobalList(*this, ObjCClasses, "objc_classes",
                 "__DATA, __objc_classlist, regular, no_dead_strip",
                 llvm::GlobalValue::InternalLinkage);
  // So do categories.
  emitGlobalList(*this, ObjCCategories, "objc_categories",
                 "__DATA, __objc_catlist, regular, no_dead_strip",
                 llvm::GlobalValue::InternalLinkage);

  // FIXME: We also emit the class references in a second magic section to make
  // sure they are "realized" by the Objective-C runtime before any instances
  // are allocated.
  emitGlobalList(*this, ObjCClasses, "objc_non_lazy_classes",
                 "__DATA, __objc_nlclslist, regular, no_dead_strip",
                 llvm::GlobalValue::InternalLinkage);

  // @llvm.used
  emitGlobalList(*this, LLVMUsed, "llvm.used", "llvm.metadata",
                 llvm::GlobalValue::AppendingLinkage);
}

void IRGenModule::emitGlobalTopLevel() {
  // Emit global variables.
  for (VarDecl *global : SILMod->getGlobals()) {
    auto &ti = getTypeInfoForUnlowered(global->getType());
    emitGlobalVariable(global, ti);
  }
  
  for (SILGlobalVariable &v : SILMod->getSILGlobals())
    emitSILGlobalVariable(&v);
  
  // Emit SIL functions.
  for (SILFunction &f : *SILMod)
    emitSILFunction(&f);

  // Emit witness tables.
  for (SILWitnessTable &wt : SILMod->getWitnessTableList())
    emitSILWitnessTable(&wt);
  
  // Emit the implicit import of the swift standard libary.
  if (DebugInfo) {
    std::pair<swift::Identifier, swift::SourceLoc> AccessPath[] = {
      { Context.StdlibModuleName, swift::SourceLoc() }
    };

    auto Imp = ImportDecl::create(Context,
                                  SILMod->getSwiftModule(),
                                  SourceLoc(),
                                  ImportKind::Module, SourceLoc(),
                                  false, AccessPath);
    DebugInfo->emitImport(Imp);
  }

  // Emit external definitions used by this module.
  for (auto def : Context.ExternalDefinitions) {
    emitExternalDefinition(def);
  }
}

static SILLinkage getSILLinkage(FormalLinkage linkage,
                                ForDefinition_t forDefinition) {
  switch (linkage) {
  case FormalLinkage::PublicUnique:
    return (forDefinition ? SILLinkage::Public : SILLinkage::PublicExternal);

  case FormalLinkage::PublicNonUnique:
    // FIXME: any place we have to do this that actually requires
    // uniqueness is buggy.
    return (forDefinition ? SILLinkage::Shared : SILLinkage::PublicExternal);
    return SILLinkage::Shared;

  case FormalLinkage::HiddenUnique:
    return (forDefinition ? SILLinkage::Hidden : SILLinkage::HiddenExternal);

  case FormalLinkage::HiddenNonUnique: 
    return (forDefinition ? SILLinkage::Shared : SILLinkage::HiddenExternal);

  case FormalLinkage::Private:
    return SILLinkage::Private;
  }
  llvm_unreachable("bad formal linkage");
}

/// Get SIL-linkage for something that's not required to be visible
/// and doesn't actually need to be uniqued.
static SILLinkage getNonUniqueSILLinkage(FormalLinkage linkage,
                                         ForDefinition_t forDefinition) {
  switch (linkage) {
  case FormalLinkage::PublicUnique:
  case FormalLinkage::PublicNonUnique:
    return (forDefinition ? SILLinkage::Shared : SILLinkage::PublicExternal);

  case FormalLinkage::HiddenUnique:
  case FormalLinkage::HiddenNonUnique:
    return (forDefinition ? SILLinkage::Shared : SILLinkage::HiddenExternal);

  case FormalLinkage::Private:
    return SILLinkage::Private;
  }
  llvm_unreachable("bad formal linkage");
}

SILLinkage LinkEntity::getLinkage(ForDefinition_t forDefinition) const {
  switch (getKind()) {
  // Most type metadata depend on the formal linkage of their type.
  case Kind::ValueWitnessTable:
  case Kind::TypeMetadata:
  case Kind::TypeMangling:
    return getSILLinkage(getTypeLinkage(getType()), forDefinition);

  // ...but we don't actually expose individual value witnesses (right now).
  case Kind::ValueWitness:
    return getNonUniqueSILLinkage(getTypeLinkage(getType()), forDefinition);

  case Kind::WitnessTableOffset:
  case Kind::Constructor:
  case Kind::Destructor:
  case Kind::Function:
  case Kind::Getter:
  case Kind::Setter:
  case Kind::Other:
  case Kind::ObjCClass:
  case Kind::ObjCMetaclass:
  case Kind::SwiftMetaclassStub:
  case Kind::FieldOffset:
  case Kind::NominalTypeDescriptor:
  case Kind::ProtocolDescriptor:
    return getSILLinkage(getDeclLinkage(getDecl()), forDefinition);

  case Kind::DirectProtocolWitnessTable:
  case Kind::LazyProtocolWitnessTableAccessor:
  case Kind::DependentProtocolWitnessTableGenerator:
    return getSILLinkage(getConformanceLinkage(getProtocolConformance()),
                         forDefinition);

  case Kind::LazyProtocolWitnessTableTemplate:
  case Kind::DependentProtocolWitnessTableTemplate:
    return SILLinkage::Private;
  
  case Kind::AnonymousFunction:
    return SILLinkage::Private;

  case Kind::BridgeToBlockConverter:
    // Bridge-to-block shims are currently always provided from a stub.
    return SILLinkage::PublicExternal;

  case Kind::SILFunction:
    return getSILFunction()->getLinkage();
      
  case Kind::SILGlobalVariable:
    return getSILGlobalVariable()->getLinkage();
  }
  llvm_unreachable("bad link entity kind");
}

static std::pair<llvm::GlobalValue::LinkageTypes,
                 llvm::GlobalValue::VisibilityTypes>
getIRLinkage(SILLinkage linkage, ForDefinition_t isDefinition) {
  switch (linkage) {
#define RESULT(LINKAGE, VISIBILITY)        \
    { llvm::GlobalValue::LINKAGE##Linkage, \
      llvm::GlobalValue::VISIBILITY##Visibility }

  case SILLinkage::Public: return RESULT(External, Default);
  case SILLinkage::Shared: return RESULT(LinkOnceODR, Hidden);
  case SILLinkage::Hidden: return RESULT(External, Hidden);
  case SILLinkage::Private: return RESULT(Internal, Default);
  case SILLinkage::PublicExternal:
    if (isDefinition) return RESULT(AvailableExternally, Default);
    return RESULT(External, Default);
  case SILLinkage::HiddenExternal:
    if (isDefinition) return RESULT(AvailableExternally, Hidden);
    return RESULT(External, Hidden);
  }
  llvm_unreachable("bad SIL linkage");
}

/// Given that we're going to define a global value but already have a
/// forward-declaration of it, update its linkage.
static void updateLinkageForDefinition(llvm::GlobalValue *global,
                                       const LinkEntity &entity) {
  // TODO: there are probably cases where we can avoid redoing the
  // entire linkage computation.
  auto linkage = getIRLinkage(entity.getLinkage(ForDefinition), ForDefinition);
  global->setLinkage(linkage.first);
  global->setVisibility(linkage.second);
}

LinkInfo LinkInfo::get(IRGenModule &IGM, const LinkEntity &entity,
                       ForDefinition_t isDefinition) {
  LinkInfo result;

  entity.mangle(result.Name);

  llvm::tie(result.Linkage, result.Visibility) =
    getIRLinkage(entity.getLinkage(isDefinition), isDefinition);

  return result;
}

static bool isPointerTo(llvm::Type *ptrTy, llvm::Type *objTy) {
  return cast<llvm::PointerType>(ptrTy)->getElementType() == objTy;
}

/// Get or create an LLVM function with these linkage rules.
llvm::Function *LinkInfo::createFunction(IRGenModule &IGM,
                                         llvm::FunctionType *fnType,
                                         llvm::CallingConv::ID cc,
                                         const llvm::AttributeSet &attrs) {
  llvm::Function *existing = IGM.Module.getFunction(getName());
  if (existing) {
    if (isPointerTo(existing->getType(), fnType))
      return cast<llvm::Function>(existing);

    IGM.error(SourceLoc(),
              "program too clever: function collides with existing symbol "
                + getName());

    // Note that this will implicitly unique if the .unique name is also taken.
    existing->setName(getName() + ".unique");
  }

  llvm::Function *fn
    = llvm::Function::Create(fnType, getLinkage(), getName(), &IGM.Module);
  fn->setVisibility(getVisibility());
  fn->setCallingConv(cc);
  if (!attrs.isEmpty())
    fn->setAttributes(attrs);
  return fn;
}

/// Get or create an LLVM global variable with these linkage rules.
llvm::GlobalVariable *LinkInfo::createVariable(IRGenModule &IGM,
                                               llvm::Type *storageType,
                                               DebugTypeInfo DebugType,
                                               Optional<SILLocation> DebugLoc,
                                               StringRef DebugName) {
  llvm::GlobalValue *existing = IGM.Module.getNamedGlobal(getName());
  if (existing) {
    if (isa<llvm::GlobalVariable>(existing) &&
        isPointerTo(existing->getType(), storageType))
      return cast<llvm::GlobalVariable>(existing);

    IGM.error(SourceLoc(),
              "program too clever: variable collides with existing symbol "
                + getName());

    // Note that this will implicitly unique if the .unique name is also taken.
    existing->setName(getName() + ".unique");
  }

  llvm::GlobalVariable *var
    = new llvm::GlobalVariable(IGM.Module, storageType, /*constant*/ false,
                               getLinkage(), /*initializer*/ nullptr,
                               getName());
  var->setVisibility(getVisibility());

  if (IGM.DebugInfo)
    IGM.DebugInfo->
      emitGlobalVariableDeclaration(var,
                                    DebugName.empty() ? getName() : DebugName,
                                    getName(), DebugType, DebugLoc);

  return var;
}

/// Emit a global declaration.
void IRGenModule::emitGlobalDecl(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Extension:
    return emitExtension(cast<ExtensionDecl>(D));

  case DeclKind::Protocol:
    return emitProtocolDecl(cast<ProtocolDecl>(D));
      
  case DeclKind::PatternBinding:
    // The global initializations are in SIL.
    return;

  case DeclKind::Subscript:
    llvm_unreachable("there are no global subscript operations");
      
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
    llvm_unreachable("there are no global enum elements");

  case DeclKind::Constructor:
    llvm_unreachable("there are no global constructor");

  case DeclKind::Destructor:
    llvm_unreachable("there are no global destructor");

  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
    return;

  case DeclKind::Enum:
    return emitEnumDecl(cast<EnumDecl>(D));

  case DeclKind::Struct:
    return emitStructDecl(cast<StructDecl>(D));

  case DeclKind::Class:
    return emitClassDecl(cast<ClassDecl>(D));

  // These declarations are only included in the debug info.
  case DeclKind::Import:
    if (DebugInfo)
      DebugInfo->emitImport(cast<ImportDecl>(D));
    return;

  // We emit these as part of the PatternBindingDecl.
  case DeclKind::Var:
    return;

  case DeclKind::Func:
    // Emit local definitions from the function body.
    return emitLocalDecls(cast<FuncDecl>(D));

  case DeclKind::TopLevelCode:
    // All the top-level code will be lowered separately.
    return;
      
  // Operator decls aren't needed for IRGen.
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    return;
  }

  llvm_unreachable("bad decl kind!");
}

void IRGenModule::emitExternalDefinition(Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::EnumCase:
  case DeclKind::EnumElement:
  case DeclKind::TopLevelCode:
  case DeclKind::TypeAlias:
  case DeclKind::GenericTypeParam:
  case DeclKind::AssociatedType:
  case DeclKind::Var:
  case DeclKind::Import:
  case DeclKind::Subscript:
  case DeclKind::Destructor:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("Not a valid external definition for IRgen");

  case DeclKind::Func:
    return emitLocalDecls(cast<FuncDecl>(D));
  case DeclKind::Constructor:
    return emitLocalDecls(cast<ConstructorDecl>(D));
    
  case DeclKind::Struct:
    // Emit Swift metadata for the external struct.
    emitStructMetadata(*this, cast<StructDecl>(D));
    break;
  case DeclKind::Enum:
    // Emit Swift metadata for the external enum.
    emitEnumMetadata(*this, cast<EnumDecl>(D));
    break;

  case DeclKind::Class:
    // No need to emit Swift metadata for external ObjC classes.
    break;

  case DeclKind::Protocol:
    // Emit Swift metadata for the protocol type.
    emitProtocolDecl(cast<ProtocolDecl>(D));
    break;
  }
}

Address IRGenModule::getAddrOfSILGlobalVariable(SILGlobalVariable *var,
                                                ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forSILGlobalVariable(var);

  // Check whether we've created the global variable already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  auto gvar = Module.getGlobalVariable(var->getName(), /*allowInternal*/ true);
  if (gvar) {
    if (forDefinition) updateLinkageForDefinition(gvar, entity);
    return Address(gvar, Alignment(gvar->getAlignment()));
  }

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  auto &ti = getTypeInfo(var->getLoweredType());
  // TODO: Debug info needs to be able to use a SILGlobalVariable.
  // We also ought to have a better debug name.
  DebugTypeInfo DbgTy(var->getLoweredType().getSwiftRValueType(), ti, nullptr);
  Optional<SILLocation> loc;
  if (var->hasLocation())
    loc = var->getLocation();
  gvar = link.createVariable(*this, ti.StorageType, DbgTy,
                             loc, var->getName());
  
  // Set the alignment from the TypeInfo.
  Address gvarAddr = ti.getAddressForPointer(gvar);
  gvar->setAlignment(gvarAddr.getAlignment().getValue());
  
  return gvarAddr;
}

/// Find the address of a (fragile, constant-size) global variable
/// declaration.  The address value is always an llvm::GlobalVariable*.
Address IRGenModule::getAddrOfGlobalVariable(VarDecl *var,
                                             ForDefinition_t forDefinition) {
  // Check whether we've cached this.
  LinkEntity entity = LinkEntity::forNonFunction(var);
  llvm::GlobalVariable *&entry = GlobalVars[entity];
  if (entry) {
    llvm::GlobalVariable *gv = cast<llvm::GlobalVariable>(entry);
    if (forDefinition) updateLinkageForDefinition(gv, entity);
    return Address(gv, Alignment(gv->getAlignment()));
  }

  const TypeInfo &type = getTypeInfoForUnlowered(var->getType());

  // Okay, we need to rebuild it.
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  DebugTypeInfo DbgTy(var, type);
  auto addr = link.createVariable(*this, type.StorageType,
                                  DbgTy, var, var->getName().str());
  // Ask the type to give us an Address.
  Address result = type.getAddressForPointer(addr);

  // Set that alignment back on the global variable.
  addr->setAlignment(result.getAlignment().getValue());

  // Write this to the cache and return.
  entry = addr;
  return result;
}

/// Find the entry point for a SIL function.
llvm::Function *IRGenModule::getAddrOfSILFunction(SILFunction *f,
                                                  ExplosionKind level,
                                                  ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forSILFunction(f, level);

  // Check whether we've created the function already.
  // FIXME: We should integrate this into the LinkEntity cache more cleanly.
  llvm::Function *fn = Module.getFunction(f->getName());
  if (fn) {
    if (forDefinition) updateLinkageForDefinition(fn, entity);
    return fn;
  }
    
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType = getFunctionType(f->getLoweredFunctionType(),
                                               level,
                                               ExtraData::None,
                                               attrs);
  
  auto cc = expandAbstractCC(*this, f->getAbstractCC());
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);

  fn = link.createFunction(*this, fnType, cc, attrs);

  // Unless this is an external reference, emit debug info for it.
  // FIXME: Or if this is a witness. DebugInfo doesn't have an interface to
  // correctly handle the generic parameters of a witness, which can come from
  // both the requirement and witness contexts.
  if (DebugInfo && !f->isExternalDeclaration()
      && f->getAbstractCC() != AbstractCC::WitnessMethod)
    DebugInfo->emitFunction(f, fn);

  return fn;
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfFunction(FunctionRef fn,
                                               ExtraData extraData,
                                               ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forFunction(fn);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }

  SILDeclRef silFn = SILDeclRef(fn.getDecl(), SILDeclRef::Kind::Func,
                                fn.getUncurryLevel(),
                                /*foreign*/ false);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);

  // A bit of a hack here. SIL represents closure functions with their context
  // expanded out and uses a partial application function to construct the
  // context. IRGen previously set up local functions to expect their extraData
  // prepackaged.
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType =
    getFunctionType(silFnType, fn.getExplosionLevel(), extraData, attrs);

  auto cc = expandAbstractCC(*this, silFnType->getAbstractCC());

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// getAddrOfGlobalInjectionFunction - Get the address of the function to
/// perform a particular injection into an enum type.
llvm::Function *IRGenModule::getAddrOfInjectionFunction(EnumElementDecl *D,
                                              ForDefinition_t forDefinition) {
  // TODO: emit at more optimal explosion kinds when reasonable!
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = D->hasArgumentType() ? 1 : 0;

  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forEnumElement(D, explosionLevel,
                                                     uncurryLevel));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }

  SILDeclRef silFn = SILDeclRef(D, SILDeclRef::Kind::EnumElement,
                                uncurryLevel, /*foreign*/ false);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);

  llvm::AttributeSet attrs;
  auto cc = expandAbstractCC(*this, silFnType->getAbstractCC());

  llvm::FunctionType *fnType =
    getFunctionType(silFnType, explosionLevel, ExtraData::None, attrs);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

static SILDeclRef::Kind getSILDeclRefKind(ConstructorKind ctorKind) {
  switch (ctorKind) {
  case ConstructorKind::Allocating: return SILDeclRef::Kind::Allocator;
  case ConstructorKind::Initializing: return SILDeclRef::Kind::Initializer;
  }
  llvm_unreachable("bad constructor kind");
}

static SILDeclRef::Kind getSILDeclRefKind(DestructorKind dtorKind) {
  switch (dtorKind) {
  case DestructorKind::Destroying: return SILDeclRef::Kind::Destroyer;
  case DestructorKind::Deallocating: return SILDeclRef::Kind::Deallocator;
  }
  llvm_unreachable("bad denstructor kind");
}

/// Fetch the declaration of the given known function.
llvm::Function *IRGenModule::getAddrOfConstructor(ConstructorDecl *ctor,
                                                  ConstructorKind ctorKind,
                                                  ExplosionKind explodeLevel,
                                               ForDefinition_t forDefinition) {
  unsigned uncurryLevel = 1;
  auto codeRef = CodeRef::forConstructor(ctor, explodeLevel, uncurryLevel);
  LinkEntity entity = LinkEntity::forConstructor(codeRef, ctorKind);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return cast<llvm::Function>(entry);
  }

  SILDeclRef silFn = SILDeclRef(ctor, getSILDeclRefKind(ctorKind),
                                uncurryLevel, /*foreign*/ false);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);

  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType =
    getFunctionType(silFnType, explodeLevel, ExtraData::None, attrs);

  auto cc = expandAbstractCC(*this, silFnType->getAbstractCC());

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Get or create a llvm::GlobalVariable.
///
/// If a definition type is given, the result will always be an
/// llvm::GlobalVariable of that type.  Otherwise, the result will
/// have type pointerToDefaultType and may involve bitcasts.
static llvm::Constant *getAddrOfLLVMVariable(IRGenModule &IGM,
                     llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> &globals,
                                             LinkEntity entity,
                                             llvm::Type *definitionType,
                                             llvm::Type *defaultType,
                                             llvm::Type *pointerToDefaultType,
                                             DebugTypeInfo debugType) {
  auto &entry = globals[entity];
  if (entry) {
    // If we're looking to define something, we may need to replace a
    // forward declaration.
    if (definitionType) {
      assert(entry->getType() == pointerToDefaultType);
      updateLinkageForDefinition(entry, entity);

      // If the type is right, we're done.
      if (definitionType == defaultType)
        return entry;

      // Fall out to the case below, clearing the name so that
      // createVariable doesn't detect a collision.
      entry->setName("");

    // Otherwise, we have a previous declaration or definition which
    // we need to ensure has the right type.
    } else {
      return llvm::ConstantExpr::getBitCast(entry, pointerToDefaultType);
    }
  }

  ForDefinition_t forDefinition = (ForDefinition_t) (definitionType != nullptr);

  // If we're not defining the object now
  if (!definitionType) definitionType = defaultType;

  // Create the variable.
  LinkInfo link = LinkInfo::get(IGM, entity, forDefinition);
  auto var = link.createVariable(IGM, definitionType, debugType);

  // If we have an existing entry, destroy it, replacing it with the
  // new variable.
  if (entry) {
    auto castVar = llvm::ConstantExpr::getBitCast(var, pointerToDefaultType);
    entry->replaceAllUsesWith(castVar);
    entry->eraseFromParent();
  }

  // Cache and return.
  entry = var;
  return var;
}

static llvm::Constant *getAddrOfLLVMVariable(IRGenModule &IGM,
                     llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> &globals,
                                             LinkEntity entity,
                                             ForDefinition_t forDefinition,
                                             llvm::Type *defaultType,
                                             llvm::Type *pointerToDefaultType,
                                             DebugTypeInfo debugType) {
  llvm::Type *definitionType = (forDefinition ? defaultType : nullptr);
  return getAddrOfLLVMVariable(IGM, globals, entity, definitionType,
                               defaultType, pointerToDefaultType, debugType);
}

/// Fetch a global reference to the given Objective-C class.  The
/// result is always a TypeMetadataPtrTy, but it may not be compatible
/// with IR-generation.
llvm::Constant *IRGenModule::getAddrOfObjCClass(ClassDecl *theClass,
                                                ForDefinition_t forDefinition) {
  assert(ObjCInterop && "getting address of ObjC class in no-interop mode");
  LinkEntity entity = LinkEntity::forObjCClass(theClass);
  DebugTypeInfo DbgTy(theClass, getPointerSize(), getPointerAlignment());
  auto addr = getAddrOfLLVMVariable(*this, GlobalVars, entity,
                                    forDefinition, TypeMetadataStructTy,
                                    TypeMetadataPtrTy, DbgTy);
  return addr;
}

/// Fetch a global reference to the given Objective-C metaclass.
/// The result is always a GlobalVariable of ObjCClassPtrTy.
llvm::Constant *IRGenModule::getAddrOfObjCMetaclass(ClassDecl *theClass,
                                                ForDefinition_t forDefinition) {
  assert(ObjCInterop && "getting address of ObjC metaclass in no-interop mode");
  LinkEntity entity = LinkEntity::forObjCMetaclass(theClass);
  DebugTypeInfo DbgTy(theClass, getPointerSize(), getPointerAlignment());
  auto addr = getAddrOfLLVMVariable(*this, GlobalVars, entity,
                                    forDefinition, ObjCClassStructTy,
                                    ObjCClassPtrTy, DbgTy);
  return addr;
}

/// Fetch the declaration of the metaclass stub for the given class type.
/// The result is always a GlobalVariable of ObjCClassPtrTy.
llvm::Constant *IRGenModule::getAddrOfSwiftMetaclassStub(ClassDecl *theClass,
                                                ForDefinition_t forDefinition) {
  assert(ObjCInterop && "getting address of metaclass stub in no-interop mode");
  LinkEntity entity = LinkEntity::forSwiftMetaclassStub(theClass);
  DebugTypeInfo DbgTy(theClass, getPointerSize(), getPointerAlignment());
  auto addr = getAddrOfLLVMVariable(*this, GlobalVars, entity,
                                    forDefinition, ObjCClassStructTy,
                                    ObjCClassPtrTy, DbgTy);
  return addr;
}

/// Fetch the declaration of a metaclass object.  This performs either
/// getAddrOfSwiftMetaclassStub or getAddrOfObjCMetaclass, depending
/// on whether the class is published as an ObjC class.
llvm::Constant *IRGenModule::getAddrOfMetaclassObject(ClassDecl *decl,
                                                ForDefinition_t forDefinition) {
  if (decl->isObjC() || decl->hasClangNode()) {
    return getAddrOfObjCMetaclass(decl, forDefinition);
  } else {
    return getAddrOfSwiftMetaclassStub(decl, forDefinition);
  }
}

/// Fetch the declaration of the metadata (or metadata template) for a
/// class.
///
/// If the definition type is specified, the result will always be a
/// GlobalVariable of the given type, which may not be at the
/// canonical address point for a type metadata.
///
/// If the definition type is not specified, then:
///   - if the metadata is indirect, then the result will not be adjusted
///     and it will have the type pointer-to-T, where T is the type
///     of a direct metadata;
///   - if the metadata is a pattern, then the result will not be
///     adjusted and it will have TypeMetadataPatternPtrTy;
///   - otherwise it will be adjusted to the canonical address point
///     for a type metadata and it will have type TypeMetadataPtrTy.
llvm::Constant *IRGenModule::getAddrOfTypeMetadata(CanType concreteType,
                                                   bool isIndirect,
                                                   bool isPattern,
                                                   llvm::Type *definitionType) {
  assert(isPattern || !isa<UnboundGenericType>(concreteType));

  llvm::Type *defaultVarTy;
  llvm::Type *defaultVarPtrTy;
  unsigned adjustmentIndex;
  ClassDecl *ObjCClass = nullptr;
  
  // Patterns use the pattern type and no adjustment.
  if (isPattern) {
    defaultVarTy = TypeMetadataPatternStructTy;
    defaultVarPtrTy = TypeMetadataPatternPtrTy;
    adjustmentIndex = 0;

  // Objective-C classes use the generic metadata type and need no adjustment.
  } else if (isa<ClassType>(concreteType) &&
             !hasKnownSwiftMetadata(*this,
                                    cast<ClassType>(concreteType)->getDecl())) {
    defaultVarTy = TypeMetadataStructTy;
    defaultVarPtrTy = TypeMetadataPtrTy;
    adjustmentIndex = 0;
    ObjCClass = cast<ClassType>(concreteType)->getDecl();
  // Class direct metadata use the heap type and require a two-word
  // adjustment (due to the heap-metadata header).
  } else if (isa<ClassType>(concreteType) ||
             isa<BoundGenericClassType>(concreteType)) {
    defaultVarTy = FullHeapMetadataStructTy;
    defaultVarPtrTy = FullHeapMetadataPtrTy;
    adjustmentIndex = 2;

  // All other non-pattern direct metadata use the full type and
  // require an adjustment.
  } else {
    defaultVarTy = FullTypeMetadataStructTy;
    defaultVarPtrTy = FullTypeMetadataPtrTy;
    adjustmentIndex = 1;
  }

  // When indirect, this is always a pointer variable and has no
  // adjustment.
  if (isIndirect) {
    defaultVarTy = defaultVarPtrTy;
    defaultVarPtrTy = defaultVarTy->getPointerTo();
    adjustmentIndex = 0;
  }

  LinkEntity entity
    = ObjCClass? LinkEntity::forObjCClass(ObjCClass)
               : LinkEntity::forTypeMetadata(concreteType, isIndirect,
                                             isPattern);

  auto DbgTy = ObjCClass 
    ? DebugTypeInfo(ObjCClass, getPointerSize(), getPointerAlignment())
    : DebugTypeInfo(MetatypeType::get(concreteType, Context), 0, 1, nullptr);

  auto addr = getAddrOfLLVMVariable(*this, GlobalVars, entity,
                                    definitionType, defaultVarTy,
                                    defaultVarPtrTy, DbgTy);

  // Do an adjustment if necessary.
  if (adjustmentIndex && !definitionType) {
    llvm::Constant *indices[] = {
      llvm::ConstantInt::get(Int32Ty, 0),
      llvm::ConstantInt::get(Int32Ty, adjustmentIndex)
    };
    addr = llvm::ConstantExpr::getInBoundsGetElementPtr(addr, indices);
  }

  return addr;
}

/// Return the address of a nominal type descriptor.  Right now, this
/// must always be for purposes of defining it.
llvm::Constant *IRGenModule::getAddrOfNominalTypeDescriptor(NominalTypeDecl *D,
                                                  llvm::Type *definitionType) {
  assert(definitionType && "not defining nominal type descriptor?");
  auto entity = LinkEntity::forNominalTypeDescriptor(D);
  return getAddrOfLLVMVariable(*this, GlobalVars, entity,
                               definitionType, definitionType,
                               definitionType->getPointerTo(),
                               DebugTypeInfo());
}

llvm::Constant *IRGenModule::getAddrOfProtocolDescriptor(ProtocolDecl *D,
                                                ForDefinition_t forDefinition) {
  if (D->isObjC())
    return getAddrOfObjCProtocolRecord(D, forDefinition);
  
  auto entity = LinkEntity::forProtocolDescriptor(D);
  auto ty = ProtocolDescriptorStructTy;
  return getAddrOfLLVMVariable(*this, GlobalVars, entity,
                               forDefinition, ty, ty->getPointerTo(),
                               DebugTypeInfo());
}

/// Fetch the declaration of the given function-to-block converter.
llvm::Function *
IRGenModule::getAddrOfBridgeToBlockConverter(SILType blockType,
                                             ForDefinition_t forDefinition)
{
  LinkEntity entity
    = LinkEntity::forBridgeToBlockConverter(blockType);
  
  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }
  
  // The block converter is a C function with signature
  // __typeof__(R (^)(A...)) converter(R (*)(A..., swift_refcounted*),
  //                                   swift_refcounted*)
  // We simplify that to the llvm type %objc(i8*, %swift.refcounted*)*.
  llvm::Type *fnParams[] = {Int8PtrTy, RefCountedPtrTy};
  llvm::FunctionType *fnType = llvm::FunctionType::get(ObjCPtrTy,
                                                       fnParams,
                                                       /*isVarArg=*/ false);
  
  
  llvm::AttributeSet attrs;
  auto cc = expandAbstractCC(*this, AbstractCC::C);
  
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Fetch the declaration of the given known destructor.
llvm::Function *IRGenModule::getAddrOfDestructor(ClassDecl *cd,
                                                 DestructorKind kind,
                                                 ForDefinition_t forDefinition,
                                                 bool isForeign) {
  auto dd = cd->getDestructor();
  auto codeRef = CodeRef::forDestructor(dd, Mangle::ExplosionKind::Minimal, 0);
  LinkEntity entity = LinkEntity::forDestructor(codeRef, kind);

  // Check whether we've cached this.
  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return cast<llvm::Function>(entry);
  }


  SILDeclRef silFn = SILDeclRef(dd, getSILDeclRefKind(kind),
                                0, isForeign);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType =
    getFunctionType(silFnType, ExplosionKind::Minimal, ExtraData::None, attrs);

  auto cc = expandAbstractCC(*this, silFnType->getAbstractCC());

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, cc, attrs);
  return entry;
}

/// Fetch the declaration of the ivar initializer for the given class.
Optional<llvm::Function*> IRGenModule::getAddrOfObjCIVarInitDestroy(
                            ClassDecl *cd,
                            bool isDestroyer,
                            ForDefinition_t forDefinition) {
  SILDeclRef silRef(cd, 
                    isDestroyer? SILDeclRef::Kind::IVarDestroyer
                               : SILDeclRef::Kind::IVarInitializer, 
                    SILDeclRef::ConstructAtNaturalUncurryLevel, 
                    /*isForeign=*/true);
  llvm::SmallString<64> ivarInitDestroyNameBuffer;
  auto ivarInitDestroyName = silRef.mangle(ivarInitDestroyNameBuffer);
  // Find the SILFunction for the ivar initializer or destroyer.
  // FIXME: This linear scan is awful. Fortunately, it only happens
  // once per definition of an Objective-C-derived class.
  for (auto &silFn : SILMod->getFunctions()) {
    if (silFn.getName() == ivarInitDestroyName)
      return getAddrOfSILFunction(&silFn, ExplosionKind::Minimal,
                                  forDefinition);
  }

  return Nothing;
}

/// Returns the address of a value-witness function.
llvm::Function *IRGenModule::getAddrOfValueWitness(CanType abstractType,
                                                   ValueWitness index,
                                                ForDefinition_t forDefinition) {
  // We shouldn't emit value witness symbols for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting value witness for generic type instance?!");
  
  LinkEntity entity = LinkEntity::forValueWitness(abstractType, index);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }

  // Find the appropriate function type.
  llvm::FunctionType *fnType =
    cast<llvm::FunctionType>(
      cast<llvm::PointerType>(getValueWitnessTy(index))
        ->getElementType());
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, RuntimeCC, llvm::AttributeSet());
  return entry;
}

/// Returns the address of a value-witness table.  If a definition
/// type is provided, the table is created with that type; the return
/// value will be an llvm::GlobalVariable.  Otherwise, the result will
/// have type WitnessTablePtrTy.
llvm::Constant *IRGenModule::getAddrOfValueWitnessTable(CanType concreteType,
                                                  llvm::Type *definitionType) {
  LinkEntity entity = LinkEntity::forValueWitnessTable(concreteType);
  DebugTypeInfo DbgTy(concreteType, getPointerSize(), getPointerAlignment(),
                      nullptr);
  return getAddrOfLLVMVariable(*this, GlobalVars, entity, definitionType,
                               WitnessTableTy, WitnessTablePtrTy, DbgTy);
}

static CanType addOwnerArgument(DeclContext *DC, CanType resultType) {
  Type argType = DC->getDeclaredTypeInContext();
  if (!argType->hasReferenceSemantics())
    argType = InOutType::get(argType);
  if (auto params = DC->getGenericParamsOfContext())
    return PolymorphicFunctionType::get(argType, resultType, params)
             ->getCanonicalType();
  return CanType(FunctionType::get(CanType(argType), resultType));
}

static AbstractCC addOwnerArgument(ValueDecl *value,
                                   CanType &resultType, unsigned &uncurryLevel) {
  DeclContext *DC = value->getDeclContext();
  switch (DC->getContextKind()) {
  case DeclContextKind::Module:
  case DeclContextKind::FileUnit:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::Initializer:
    return AbstractCC::Freestanding;

  case DeclContextKind::ExtensionDecl:
  case DeclContextKind::NominalTypeDecl:
    resultType = addOwnerArgument(DC, resultType);
    uncurryLevel++;
    return AbstractCC::Method;
  }
  llvm_unreachable("bad decl context");
}

/// Add the 'index' argument to a getter or setter.
static void addIndexArgument(ValueDecl *value,
                             CanType &formalType, unsigned &uncurryLevel) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(value)) {
    formalType = FunctionType::get(sub->getIndices()->getType(),
                                   formalType)->getCanonicalType();
    uncurryLevel++;
  }
}

static CanType getObjectType(ValueDecl *decl) {
  if (SubscriptDecl *sub = dyn_cast<SubscriptDecl>(decl))
    return sub->getElementType()->getCanonicalType();
  return decl->getType()->getCanonicalType();
}

/// getTypeOfGetter - Return the formal type of a getter for a
/// variable or subscripted object.
FormalType IRGenModule::getTypeOfGetter(ValueDecl *value) {
  // The formal type of a getter function is one of:
  //   S -> () -> T (for a nontype member)
  //   A -> S -> () -> T (for a type member)
  // where T is the value type of the object and S is the index type
  // (this clause is skipped for a non-subscript getter).
  unsigned uncurryLevel = 0;
  CanType formalType = CanType(FunctionType::get(TupleType::getEmpty(Context),
                                                 getObjectType(value)));
  addIndexArgument(value, formalType, uncurryLevel);
  AbstractCC cc = addOwnerArgument(value, formalType, uncurryLevel);

  return FormalType(formalType, cc, uncurryLevel);
}

/// getAddrOfGetter - Get the address of the function which performs a
/// get of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfGetter(ValueDecl *value,
                                             ExplosionKind explosionLevel,
                                             ForDefinition_t forDefinition) {
  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forGetter(value, explosionLevel, 0));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }

  SILDeclRef silFn = SILDeclRef(value, SILDeclRef::Kind::Getter,
                                SILDeclRef::ConstructAtNaturalUncurryLevel,
                                /*foreign*/ false);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);

  llvm::AttributeSet attrs;
  auto convention = expandAbstractCC(*this, silFnType->getAbstractCC());
  llvm::FunctionType *fnType =
    getFunctionType(silFnType, explosionLevel, ExtraData::None, attrs);

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, convention, attrs);
  return entry;
}

/// getTypeOfSetter - Return the formal type of a setter for a
/// variable or subscripted object.
FormalType IRGenModule::getTypeOfSetter(ValueDecl *value) {
  // The formal type of a setter function is one of:
  //   S -> T -> () (for a nontype member)
  //   A -> S -> T -> () (for a type member)
  // where T is the value type of the object and S is the index type
  // (this clause is skipped for a non-subscript setter).
  unsigned uncurryLevel = 0;
  CanType argType = getObjectType(value);
  CanType formalType = CanType(FunctionType::get(argType,
                                                 TupleType::getEmpty(Context)));
  addIndexArgument(value, formalType, uncurryLevel);
  auto cc = addOwnerArgument(value, formalType, uncurryLevel);

  return FormalType(formalType, cc, uncurryLevel);
}

/// getAddrOfSetter - Get the address of the function which performs a
/// set of a variable or subscripted object.
llvm::Function *IRGenModule::getAddrOfSetter(ValueDecl *value,
                                             ExplosionKind explosionLevel,
                                             ForDefinition_t forDefinition) {
  LinkEntity entity =
    LinkEntity::forFunction(CodeRef::forSetter(value, explosionLevel, 0));

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return entry;
  }

  SILDeclRef silFn = SILDeclRef(value, SILDeclRef::Kind::Setter,
                                SILDeclRef::ConstructAtNaturalUncurryLevel,
                                /*foreign*/ false);
  auto silFnType = SILMod->Types.getConstantFunctionType(silFn);

  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType =
    getFunctionType(silFnType, explosionLevel, ExtraData::None, attrs);

  auto convention = expandAbstractCC(*this, silFnType->getAbstractCC());

  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);
  entry = link.createFunction(*this, fnType, convention, attrs);
  return entry;
}

static Address getAddrOfSimpleVariable(IRGenModule &IGM,
                    llvm::DenseMap<LinkEntity, llvm::GlobalVariable*> &cache,
                                       LinkEntity entity,
                                       llvm::Type *type,
                                       Alignment alignment,
                                       ForDefinition_t forDefinition) {
  // Check whether it's already cached.
  llvm::GlobalVariable *&entry = cache[entity];
  if (entry) {
    assert(alignment == Alignment(entry->getAlignment()));
    if (forDefinition) updateLinkageForDefinition(entry, entity);
    return Address(entry, alignment);
  }

  // Otherwise, we need to create it.
  LinkInfo link = LinkInfo::get(IGM, entity, forDefinition);
  auto addr = link.createVariable(IGM, type);
  addr->setConstant(true);

  addr->setAlignment(alignment.getValue());

  entry = addr;
  return Address(addr, alignment);
}

/// getAddrOfWitnessTableOffset - Get the address of the global
/// variable which contains an offset within a witness table for the
/// value associated with the given function.
Address IRGenModule::getAddrOfWitnessTableOffset(CodeRef code,
                                                ForDefinition_t forDefinition) {
  LinkEntity entity =
    LinkEntity::forWitnessTableOffset(code.getDecl(), code.getExplosionLevel(),
                                      code.getUncurryLevel());
  return getAddrOfSimpleVariable(*this, GlobalVars, entity,
                                 SizeTy, getPointerAlignment(),
                                 forDefinition);
}

/// getAddrOfWitnessTableOffset - Get the address of the global
/// variable which contains an offset within a witness table for the
/// value associated with the given member variable..
Address IRGenModule::getAddrOfWitnessTableOffset(VarDecl *field,
                                                ForDefinition_t forDefinition) {
  LinkEntity entity =
    LinkEntity::forWitnessTableOffset(field, ExplosionKind::Minimal, 0);
  return ::getAddrOfSimpleVariable(*this, GlobalVars, entity,
                                   SizeTy, getPointerAlignment(),
                                   forDefinition);
}

/// getAddrOfFieldOffset - Get the address of the global variable
/// which contains an offset to apply to either an object (if direct)
/// or a metadata object in order to find an offset to apply to an
/// object (if indirect).
///
/// The result is always a GlobalVariable.
Address IRGenModule::getAddrOfFieldOffset(VarDecl *var, bool isIndirect,
                                          ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forFieldOffset(var, isIndirect);
  return getAddrOfSimpleVariable(*this, GlobalVars, entity,
                                 SizeTy, getPointerAlignment(),
                                 forDefinition);
}

static bool protocolExtensionRequiresCategory(ProtocolDecl *protocol,
                                            ProtocolConformance *conformance) {
  if (protocol->isObjC())
    return true;
  for (auto &inherited : conformance->getInheritedConformances())
    if (protocolExtensionRequiresCategory(inherited.first, inherited.second))
      return true;
  return false;
}

/// Emit a type extension.
void IRGenModule::emitExtension(ExtensionDecl *ext) {
  for (Decl *member : ext->getMembers()) {
    switch (member->getKind()) {
    case DeclKind::Import:
    case DeclKind::EnumCase:
    case DeclKind::EnumElement:
    case DeclKind::TopLevelCode:
    case DeclKind::Protocol:
    case DeclKind::Extension:
    case DeclKind::Destructor:
    case DeclKind::InfixOperator:
    case DeclKind::PrefixOperator:
    case DeclKind::PostfixOperator:
      llvm_unreachable("decl not allowed in extension!");

    // PatternBindingDecls don't really make sense here, but we
    // produce one as a side-effect of parsing a var property.
    // Just ignore it.
    case DeclKind::PatternBinding:
      continue;

    case DeclKind::Subscript:
      // Getter/setter will be handled separately.
      continue;
    case DeclKind::TypeAlias:
    case DeclKind::GenericTypeParam:
    case DeclKind::AssociatedType:
      continue;
    case DeclKind::Enum:
      emitEnumDecl(cast<EnumDecl>(member));
      continue;
    case DeclKind::Struct:
      emitStructDecl(cast<StructDecl>(member));
      continue;
    case DeclKind::Class:
      emitClassDecl(cast<ClassDecl>(member));
      continue;
    case DeclKind::Var:
      if (cast<VarDecl>(member)->isComputed())
        // Getter/setter will be handled separately.
        continue;
      llvm_unreachable("decl not allowed in extension!");
    case DeclKind::Func:
      emitLocalDecls(cast<FuncDecl>(member));
      continue;
    case DeclKind::Constructor:
      emitLocalDecls(cast<ConstructorDecl>(member));
      continue;
    }
    llvm_unreachable("bad extension member kind");
  }
  
  // If the original class is ObjC, or the extension either introduces a
  // conformance to an ObjC protocol or introduces a method that requires an
  // Objective-C entry point, generate a category.
  ClassDecl *origClass = ext->getDeclaredTypeInContext()
    ->getClassOrBoundGenericClass();
  if (!origClass)
    return;
  bool needsCategory = origClass->isObjC();
  if (!needsCategory) {
    for (unsigned i = 0, size = ext->getProtocols().size(); i < size; ++i)
      if (protocolExtensionRequiresCategory(ext->getProtocols()[i],
                                            ext->getConformances()[i])) {
        needsCategory = true;
        break;
      }
  }
  if (!needsCategory) {
    for (auto member : ext->getMembers()) {
      if (auto func = dyn_cast<FuncDecl>(member)) {
        if (requiresObjCMethodDescriptor(func)) {
          needsCategory = true;
          break;
        }
        continue;
      }

      if (auto constructor = dyn_cast<ConstructorDecl>(member)) {
        if (requiresObjCMethodDescriptor(constructor)) {
          needsCategory = true;
          break;
        }
        continue;
      }

      if (auto var = dyn_cast<VarDecl>(member)) {
        if (requiresObjCPropertyDescriptor(var)) {
          needsCategory = true;
          break;
        }
        continue;
      }

      if (auto subscript = dyn_cast<SubscriptDecl>(member)) {
        if (requiresObjCSubscriptDescriptor(subscript)) {
          needsCategory = true;
          break;
        }
        continue;
      }
    }
  }
  
  if (needsCategory) {
    llvm::Constant *category = emitCategoryData(*this, ext);
    category = llvm::ConstantExpr::getBitCast(category, Int8PtrTy);
    ObjCCategories.push_back(category);
    ObjCCategoryDecls.push_back(ext);
  }
}


/// Create an allocation on the stack.
Address IRGenFunction::createAlloca(llvm::Type *type,
                                    Alignment alignment,
                                    const llvm::Twine &name) {
  llvm::AllocaInst *alloca = new llvm::AllocaInst(type, name, AllocaIP);
  alloca->setAlignment(alignment.getValue());
  return Address(alloca, alignment);
}

/// Get or create a global string constant.
///
/// \returns an i8* with a null terminator; note that embedded nulls
///   are okay
llvm::Constant *IRGenModule::getAddrOfGlobalString(StringRef data) {
  // Check whether this string already exists.
  auto &entry = GlobalStrings[data];
  if (entry) return entry;

  // If not, create it.  This implicitly adds a trailing null.
  auto init = llvm::ConstantDataArray::getString(LLVMContext, data);
  auto global = new llvm::GlobalVariable(Module, init->getType(), true,
                                         llvm::GlobalValue::PrivateLinkage,
                                         init);
  global->setUnnamedAddr(true);

  // Drill down to make an i8*.
  auto zero = llvm::ConstantInt::get(SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  auto address = llvm::ConstantExpr::getInBoundsGetElementPtr(global, indices);

  // Cache and return.
  entry = address;
  return address;
}

/// Get or create a global UTF-16 string constant.
///
/// \returns an i16* with a null terminator; note that embedded nulls
///   are okay
llvm::Constant *IRGenModule::getAddrOfGlobalUTF16String(StringRef utf8) {
  // Check whether this string already exists.
  auto &entry = GlobalUTF16Strings[utf8];
  if (entry) return entry;

  // If not, first transcode it to UTF16.
  SmallVector<UTF16, 128> buffer(utf8.size() + 1); // +1 for ending nulls.
  const UTF8 *fromPtr = (const UTF8 *) utf8.data();
  UTF16 *toPtr = &buffer[0];
  (void) ConvertUTF8toUTF16(&fromPtr, fromPtr + utf8.size(),
                            &toPtr, toPtr + utf8.size(),
                            strictConversion);

  // The length of the transcoded string in UTF-8 code points.
  size_t utf16Length = toPtr - &buffer[0];

  // Null-terminate the UTF-16 string.
  *toPtr = 0;
  ArrayRef<UTF16> utf16(&buffer[0], utf16Length + 1);

  auto init = llvm::ConstantDataArray::get(LLVMContext, utf16);
  auto global = new llvm::GlobalVariable(Module, init->getType(), true,
                                         llvm::GlobalValue::PrivateLinkage,
                                         init);
  global->setUnnamedAddr(true);

  // Drill down to make an i16*.
  auto zero = llvm::ConstantInt::get(SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  auto address = llvm::ConstantExpr::getInBoundsGetElementPtr(global, indices);

  // Cache and return.
  entry = address;
  return address;
}

/// Mangle the name of a type.
StringRef IRGenModule::mangleType(CanType type, SmallVectorImpl<char> &buffer) {
  LinkEntity::forTypeMangling(type).mangle(buffer);
  return StringRef(buffer.data(), buffer.size());
}

/// Is the given declaration resilient?
bool IRGenModule::isResilient(Decl *theDecl, ResilienceScope scope) {
  // Classes defined by Clang are resilient.
  if (auto theClass = dyn_cast<ClassDecl>(theDecl)) {
    return theClass->hasClangNode();
  }

  return false;
}

/// Look up the address of a witness table.
///
/// TODO: This needs to take a flag for the access mode of the witness table,
/// which may be direct, lazy, or a runtime instantiation template.
llvm::Constant*
IRGenModule::getAddrOfWitnessTable(const NormalProtocolConformance *C,
                                   llvm::Type *storageTy) {
  auto entity = LinkEntity::forDirectProtocolWitnessTable(C);
  return getAddrOfLLVMVariable(*this, GlobalVars, entity,
                               storageTy, WitnessTableTy, WitnessTablePtrTy,
                               DebugTypeInfo());
}
