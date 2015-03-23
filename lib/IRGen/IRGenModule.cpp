//===--- IRGenModule.cpp - Swift Global LLVM IR Generation ----------------===//
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
//  This file implements IR generation for global declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/DiagnosticsIRGen.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/CharInfo.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/CodeGen/CodeGenABITypes.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"

#include "GenEnum.h"
#include "GenType.h"
#include "IRGenModule.h"
#include "IRGenDebugInfo.h"
#include "Linking.h"

#include <initializer_list>

using namespace swift;
using namespace irgen;
using clang::CodeGen::CodeGenABITypes;
using llvm::Attribute;

const unsigned DefaultAS = 0;

/// A helper for creating LLVM struct types.
static llvm::StructType *createStructType(IRGenModule &IGM,
                                          StringRef name,
                                  std::initializer_list<llvm::Type*> types) {
  return llvm::StructType::create(IGM.getLLVMContext(),
                                  ArrayRef<llvm::Type*>(types.begin(),
                                                        types.size()),
                                  name);
};

/// A helper for creating pointer-to-struct types.
static llvm::PointerType *createStructPointerType(IRGenModule &IGM,
                                                  StringRef name,
                                  std::initializer_list<llvm::Type*> types) {
  return createStructType(IGM, name, types)->getPointerTo(DefaultAS);
};

static clang::CodeGenerator *createClangCodeGenerator(ASTContext &Context,
                                                 llvm::LLVMContext &LLVMContext,
                                                      IRGenOptions &Opts,
                                                      StringRef ModuleName) {
  auto Loader = Context.getClangModuleLoader();
  auto *Importer = static_cast<ClangImporter*>(&*Loader);
  assert(Importer && "No clang module loader!");
  auto &ClangContext = Importer->getClangASTContext();

  auto &CGO = Importer->getClangCodeGenOpts();
  CGO.OptimizationLevel = Opts.Optimize ? 3 : 0;
  CGO.DisableFPElim = Opts.DisableFPElim;
  auto *ClangCodeGen = clang::CreateLLVMCodeGen(ClangContext.getDiagnostics(),
                                                ModuleName, CGO, LLVMContext);
  ClangCodeGen->Initialize(ClangContext);

  return ClangCodeGen;
}

IRGenModule::IRGenModule(ASTContext &Context,
                         llvm::LLVMContext &LLVMContext,
                         IRGenOptions &Opts, StringRef ModuleName,
                         const llvm::DataLayout &DataLayout,
                         const llvm::Triple &Triple,
                         llvm::TargetMachine *TargetMachine,
                         SILModule *SILMod,
                         StringRef OutputFilename)
  : Context(Context), Opts(Opts),
    ClangCodeGen(createClangCodeGenerator(Context, LLVMContext, Opts, ModuleName)),
    Module(*ClangCodeGen->GetModule()),
    LLVMContext(Module.getContext()), DataLayout(DataLayout),
    Triple(Triple), TargetMachine(TargetMachine),
    SILMod(SILMod), OutputFilename(OutputFilename), dispatcher(nullptr),
    TargetInfo(SwiftTargetInfo::get(*this)),
    DebugInfo(0), ObjCInterop(Context.LangOpts.EnableObjCInterop),
    Types(*new TypeConverter(*this))
{
  VoidTy = llvm::Type::getVoidTy(getLLVMContext());
  Int1Ty = llvm::Type::getInt1Ty(getLLVMContext());
  Int8Ty = llvm::Type::getInt8Ty(getLLVMContext());
  Int16Ty = llvm::Type::getInt16Ty(getLLVMContext());
  Int32Ty = llvm::Type::getInt32Ty(getLLVMContext());
  Int64Ty = llvm::Type::getInt64Ty(getLLVMContext());
  ObjCBoolTy = llvm::IntegerType::get(getLLVMContext(),
                                      TargetInfo.ObjCBoolTypeSize);
  Int8PtrTy = llvm::Type::getInt8PtrTy(getLLVMContext());
  Int8PtrPtrTy = Int8PtrTy->getPointerTo(0);
  SizeTy = DataLayout.getIntPtrType(getLLVMContext(), /*addrspace*/ 0);

  RefCountedStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.refcounted");
  RefCountedPtrTy = RefCountedStructTy->getPointerTo(/*addrspace*/ 0);
  RefCountedNull = llvm::ConstantPointerNull::get(RefCountedPtrTy);

  // For now, native weak references are just a pointer.
  WeakReferencePtrTy =
    createStructPointerType(*this, "swift.weak", { RefCountedPtrTy });

  // A type metadata record is the structure pointed to by the canonical
  // address point of a type metadata.  This is at least one word, and
  // potentially more than that, past the start of the actual global
  // structure.
  TypeMetadataStructTy = createStructType(*this, "swift.type", {
    MetadataKindTy          // MetadataKind Kind;
  });
  TypeMetadataPtrTy = TypeMetadataStructTy->getPointerTo(DefaultAS);

  // A protocol descriptor describes a protocol. It is not type metadata in
  // and of itself, but is referenced in the structure of existential type
  // metadata records.
  ProtocolDescriptorStructTy = createStructType(*this, "swift.protocol", {
    Int8PtrTy,              // objc isa
    Int8PtrTy,              // name
    Int8PtrTy,              // inherited protocols
    Int8PtrTy,              // required objc instance methods
    Int8PtrTy,              // required objc class methods
    Int8PtrTy,              // optional objc instance methods
    Int8PtrTy,              // optional objc class methods
    Int8PtrTy,              // objc properties
    Int32Ty,                // size
    Int32Ty                 // flags
  });
  
  ProtocolDescriptorPtrTy = ProtocolDescriptorStructTy->getPointerTo();
  
  // A tuple type metadata record has a couple extra fields.
  auto tupleElementTy = createStructType(*this, "swift.tuple_element_type", {
    TypeMetadataPtrTy,      // Metadata *Type;
    SizeTy                  // size_t Offset;
  });
  TupleTypeMetadataPtrTy = createStructPointerType(*this, "swift.tuple_type", {
    TypeMetadataStructTy,   // (base)
    SizeTy,                 // size_t NumElements;
    Int8PtrTy,              // const char *Labels;
    llvm::ArrayType::get(tupleElementTy, 0) // Element Elements[];
  });

  // A full type metadata record is basically just an adjustment to the
  // address point of a type metadata.  Resilience may cause
  // additional data to be laid out prior to this address point.
  FullTypeMetadataStructTy = createStructType(*this, "swift.full_type", {
    WitnessTablePtrTy,
    TypeMetadataStructTy
  });
  FullTypeMetadataPtrTy = FullTypeMetadataStructTy->getPointerTo(DefaultAS);

  // A metadata pattern is a structure from which generic type
  // metadata are allocated.  We leave this struct type intentionally
  // opaque, because the compiler basically never needs to access
  // anything from one.
  TypeMetadataPatternStructTy =
    llvm::StructType::create(getLLVMContext(), "swift.type_pattern");
  TypeMetadataPatternPtrTy =
    TypeMetadataPatternStructTy->getPointerTo(DefaultAS);

  DeallocatingDtorTy = llvm::FunctionType::get(VoidTy, RefCountedPtrTy, false);
  llvm::Type *dtorPtrTy = DeallocatingDtorTy->getPointerTo();

  // A full heap metadata is basically just an additional small prefix
  // on a full metadata, used for metadata corresponding to heap
  // allocations.
  FullHeapMetadataStructTy =
                  createStructType(*this, "swift.full_heapmetadata", {
    dtorPtrTy,
    WitnessTablePtrTy,
    TypeMetadataStructTy
  });
  FullHeapMetadataPtrTy = FullHeapMetadataStructTy->getPointerTo(DefaultAS);

  llvm::Type *refCountedElts[] = { TypeMetadataPtrTy, Int32Ty, Int32Ty };
  RefCountedStructTy->setBody(refCountedElts);

  PtrSize = Size(DataLayout.getPointerSize(DefaultAS));

  FunctionPairTy = createStructType(*this, "swift.function", {
    FunctionPtrTy,
    RefCountedPtrTy,
  });
  
  WitnessFunctionPairTy = createStructType(*this, "swift.witness_function",  {
    FunctionPtrTy,
    TypeMetadataPtrTy,
  });
  
  OpaquePtrTy = llvm::StructType::create(LLVMContext, "swift.opaque")
                  ->getPointerTo(DefaultAS);

  ProtocolConformanceRecordTy
    = createStructType(*this, "swift.protocol_conformance", {
      ProtocolDescriptorPtrTy,
      OpaquePtrTy,
      OpaquePtrTy,
      Int32Ty
    });
  ProtocolConformanceRecordPtrTy
    = ProtocolConformanceRecordTy->getPointerTo(DefaultAS);

  FixedBufferTy = nullptr;
  for (unsigned i = 0; i != MaxNumValueWitnesses; ++i)
    ValueWitnessTys[i] = nullptr;

  ObjCPtrTy = llvm::StructType::create(getLLVMContext(), "objc_object")
                ->getPointerTo(DefaultAS);
  BridgeObjectPtrTy = llvm::StructType::create(getLLVMContext(), "swift.bridge")
                ->getPointerTo(DefaultAS);

  ObjCClassStructTy = llvm::StructType::create(LLVMContext, "objc_class");
  ObjCClassPtrTy = ObjCClassStructTy->getPointerTo(DefaultAS);
  llvm::Type *objcClassElts[] = {
    ObjCClassPtrTy,
    ObjCClassPtrTy,
    OpaquePtrTy,
    OpaquePtrTy,
    IntPtrTy
  };
  ObjCClassStructTy->setBody(objcClassElts);

  ObjCSuperStructTy = llvm::StructType::create(LLVMContext, "objc_super");
  ObjCSuperPtrTy = ObjCSuperStructTy->getPointerTo(DefaultAS);
  llvm::Type *objcSuperElts[] = {
    ObjCPtrTy,
    ObjCClassPtrTy
  };
  ObjCSuperStructTy->setBody(objcSuperElts);
  
  ObjCBlockStructTy = llvm::StructType::create(LLVMContext, "objc_block");
  ObjCBlockPtrTy = ObjCBlockStructTy->getPointerTo(DefaultAS);
  llvm::Type *objcBlockElts[] = {
    ObjCClassPtrTy, // isa
    Int32Ty,        // flags
    Int32Ty,        // reserved
    FunctionPtrTy,  // invoke function pointer
    Int8PtrTy,      // TODO: block descriptor pointer.
                    // We will probably need a struct type for that at some
                    // point too.
  };
  ObjCBlockStructTy->setBody(objcBlockElts);
  
  // TODO: use "tinycc" on platforms that support it
  RuntimeCC = llvm::CallingConv::C;

  auto CI = static_cast<ClangImporter*>(&*Context.getClangModuleLoader());
  assert(CI && "no clang module loader");

  auto &clangASTContext = CI->getClangASTContext();
  ABITypes = new CodeGenABITypes(clangASTContext, Module, DataLayout);

  if (Opts.DebugInfoKind != IRGenDebugInfoKind::None) {
    DebugInfo = new IRGenDebugInfo(Opts, *CI, *this, Module);
  }

  initClangTypeConverter();
}

IRGenModule::~IRGenModule() {
  destroyClangTypeConverter();
  delete &Types;
  if (DebugInfo)
    delete DebugInfo;
  delete ABITypes;
}

static llvm::Constant *getRuntimeFn(IRGenModule &IGM,
                      llvm::Constant *&cache,
                      char const *name,
                      llvm::CallingConv::ID cc,
                      std::initializer_list<llvm::Type*> retTypes,
                      std::initializer_list<llvm::Type*> argTypes,
                      std::initializer_list<Attribute::AttrKind> attrs
                         = std::initializer_list<Attribute::AttrKind>()) {
  if (cache)
    return cache;
  
  llvm::Type *retTy;
  if (retTypes.size() == 1)
    retTy = *retTypes.begin();
  else
    retTy = llvm::StructType::get(IGM.LLVMContext,
                                  {retTypes.begin(), retTypes.end()},
                                  /*packed*/ false);
  auto fnTy = llvm::FunctionType::get(retTy,
                                      {argTypes.begin(), argTypes.end()},
                                      /*isVararg*/ false);

  cache = IGM.Module.getOrInsertFunction(name, fnTy);

  // Add any function attributes and set the calling convention.
  if (auto fn = dyn_cast<llvm::Function>(cache)) {
    fn->setCallingConv(cc);

    llvm::AttrBuilder b;

    for (auto Attr : attrs)
      b.addAttribute(Attr);

    fn->getAttributes().
      addAttributes(IGM.LLVMContext,
                    llvm::AttributeSet::FunctionIndex,
                    llvm::AttributeSet::get(IGM.LLVMContext,
                                            llvm::AttributeSet::FunctionIndex,
                                            b));
  }

  return cache;
}


// Explicitly listing these constants is an unfortunate compromise for
// making the database file much more compact.
//
// They have to be non-local because otherwise we'll get warnings when
// a particular x-macro expansion doesn't use one.
namespace RuntimeConstants {
  const auto ReadNone = llvm::Attribute::ReadNone;
  const auto ReadOnly = llvm::Attribute::ReadOnly;
  const auto NoUnwind = llvm::Attribute::NoUnwind;
  const auto C_CC = llvm::CallingConv::C;
}

#define RETURNS(...) { __VA_ARGS__ }
#define ARGS(...) { __VA_ARGS__ }
#define NO_ARGS {}
#define ATTRS(...) { __VA_ARGS__ }
#define NO_ATTRS {}
#define FUNCTION(ID, NAME, CC, RETURNS, ARGS, ATTRS)       \
llvm::Constant *IRGenModule::get##ID##Fn() {               \
  using namespace RuntimeConstants;                        \
  return getRuntimeFn(*this, ID##Fn, #NAME, CC,            \
                      RETURNS, ARGS, ATTRS);               \
}
#include "RuntimeFunctions.def"

llvm::Constant *IRGenModule::getEmptyTupleMetadata() {
  if (EmptyTupleMetadata)
    return EmptyTupleMetadata;

  return EmptyTupleMetadata =
    Module.getOrInsertGlobal("_TMdT_", FullTypeMetadataStructTy);
}

llvm::Constant *IRGenModule::getObjCEmptyCachePtr() {
  if (ObjCEmptyCachePtr) return ObjCEmptyCachePtr;

  if (ObjCInterop) {
    // struct objc_cache _objc_empty_cache;
    ObjCEmptyCachePtr = Module.getOrInsertGlobal("_objc_empty_cache",
                                                 OpaquePtrTy->getElementType());
  } else {
    // FIXME: Remove even the null value per rdar://problem/18801263
    ObjCEmptyCachePtr = llvm::ConstantPointerNull::get(OpaquePtrTy);
  }
  return ObjCEmptyCachePtr;
}

llvm::Constant *IRGenModule::getObjCEmptyVTablePtr() {
  if (ObjCEmptyVTablePtr) return ObjCEmptyVTablePtr;

  // IMP _objc_empty_vtable;

  // On recent Darwin platforms, this symbol is actually defined at
  // runtime as an absolute symbol with the value of null.  On some
  // older platforms, that wasn't true, and it isn't clear that the
  // ObjC runtime is willing to make a *guarantee* that it's true, so
  // in general we still use the symbol.  However, there are a number
  // of (non-ABI) environments that don't actually support absolute
  // symbols correctly, such as the iOS simulator, and for these we
  // have to fill in null directly.

  if (!ObjCInterop || TargetInfo.ObjCUseNullForEmptyVTable) {
    ObjCEmptyVTablePtr = llvm::ConstantPointerNull::get(OpaquePtrTy);
  } else {
    ObjCEmptyVTablePtr = Module.getOrInsertGlobal("_objc_empty_vtable",
                                                  OpaquePtrTy->getElementType());
  }
  return ObjCEmptyVTablePtr;
}

Address IRGenModule::getAddrOfObjCISAMask() {
  // This symbol is only exported by the runtime if the platform uses
  // isa masking.
  assert(TargetInfo.hasISAMasking());
  if (!ObjCISAMaskPtr) {
    ObjCISAMaskPtr =  Module.getOrInsertGlobal("swift_isaMask", IntPtrTy);
  }
  return Address(ObjCISAMaskPtr, getPointerAlignment());
}

llvm::Module *IRGenModule::getModule() const {
  return ClangCodeGen->GetModule();
}

llvm::Module *IRGenModule::releaseModule() {
  return ClangCodeGen->ReleaseModule();
}

llvm::AttributeSet IRGenModule::getAllocAttrs() {
  if (AllocAttrs.isEmpty()) {
    AllocAttrs = llvm::AttributeSet::get(LLVMContext,
                                         llvm::AttributeSet::ReturnIndex,
                                         llvm::Attribute::NoAlias);
    AllocAttrs = AllocAttrs.addAttribute(LLVMContext,
                               llvm::AttributeSet::FunctionIndex,
                               llvm::Attribute::NoUnwind);
  }
  return AllocAttrs;
}

llvm::Constant *IRGenModule::getSize(Size size) {
  return llvm::ConstantInt::get(SizeTy, size.getValue());
}

static void appendEncodedName(raw_ostream &os, StringRef name) {
  if (clang::isValidIdentifier(name)) {
    os << "_" << name;
  } else {
    for (auto c : name)
      os.write_hex(static_cast<uint8_t>(c));
  }
}

static void appendEncodedName(llvm::SmallVectorImpl<char> &buf,
                              StringRef name) {
  llvm::raw_svector_ostream os{buf};
  appendEncodedName(os, name);
}

static StringRef encodeForceLoadSymbolName(llvm::SmallVectorImpl<char> &buf,
                                           StringRef name) {
  llvm::raw_svector_ostream os{buf};
  os << "_swift_FORCE_LOAD_$";
  appendEncodedName(os, name);
  return os.str();
}

void IRGenModule::addLinkLibrary(const LinkLibrary &linkLib) {
  llvm::LLVMContext &ctx = Module.getContext();

  switch (linkLib.getKind()) {
  case LibraryKind::Library: {
    // FIXME: Use target-independent linker option.
    // Clang uses CGM.getTargetCodeGenInfo().getDependentLibraryOption(...).
    llvm::SmallString<32> buf;
    buf += "-l";
    buf += linkLib.getName();
    auto flag = llvm::MDString::get(ctx, buf);
    AutolinkEntries.push_back(llvm::MDNode::get(ctx, flag));
    break;
  }
  case LibraryKind::Framework:
    llvm::Metadata *args[] = {
      llvm::MDString::get(ctx, "-framework"),
      llvm::MDString::get(ctx, linkLib.getName())
    };
    AutolinkEntries.push_back(llvm::MDNode::get(ctx, args));
    break;
  }

  if (linkLib.shouldForceLoad()) {
    llvm::SmallString<64> buf;
    encodeForceLoadSymbolName(buf, linkLib.getName());
    auto symbolAddr = Module.getOrInsertGlobal(buf.str(), Int1Ty);

    buf += "_$";
    appendEncodedName(buf, Opts.ModuleName);

    if (!Module.getGlobalVariable(buf.str())) {
      auto ref = new llvm::GlobalVariable(Module, symbolAddr->getType(),
                                          /*constant=*/true,
                                          llvm::GlobalValue::WeakAnyLinkage,
                                          symbolAddr, buf.str());
      ref->setVisibility(llvm::GlobalValue::HiddenVisibility);
      auto casted = llvm::ConstantExpr::getBitCast(ref, Int8PtrTy);
      LLVMUsed.push_back(casted);
    }
  }
}

// FIXME: This should just be the implementation of
// llvm::array_pod_sort_comparator. The only difference is that it uses
// std::less instead of operator<.
template <typename T>
static int pointerPODSortComparator(T * const *lhs, T * const *rhs) {
  std::less<T *> lt;
  if (lt(*lhs, *rhs))
    return -1;
  if (lt(*rhs, *lhs))
    return -1;
  return 0;
}

void IRGenModule::emitAutolinkInfo() {
  // Remove duplicates.
  llvm::SmallPtrSet<llvm::Metadata*, 4> knownAutolinkEntries;
  AutolinkEntries.erase(std::remove_if(AutolinkEntries.begin(),
                                       AutolinkEntries.end(),
                                       [&](llvm::Metadata *entry) -> bool {
                                         return !knownAutolinkEntries.insert(
                                                   entry).second;
                                       }),
                        AutolinkEntries.end());

  switch (TargetInfo.OutputObjectFormat) {
  case llvm::Triple::MachO: {
    // FIXME: This constant should be vended by LLVM somewhere.
    static const char * const LinkerOptionsFlagName = "Linker Options";

    llvm::LLVMContext &ctx = Module.getContext();
    Module.addModuleFlag(llvm::Module::AppendUnique, LinkerOptionsFlagName,
                         llvm::MDNode::get(ctx, AutolinkEntries));

    break;
  }
  case llvm::Triple::ELF: {
    // Merge the entries into null-separated string.
    llvm::SmallString<64> EntriesString;
    for (auto &EntryNode : AutolinkEntries) {
      const llvm::MDNode *MD = cast<llvm::MDNode>(EntryNode);
      for (auto &Entry : MD->operands()) {
        const llvm::MDString *MS = cast<llvm::MDString>(Entry);
        EntriesString += MS->getString();
        EntriesString += '\0';
      }
    }
    auto EntriesConstant = llvm::ConstantDataArray::getString(
      LLVMContext, EntriesString, /*AddNull=*/false);

    auto var = new llvm::GlobalVariable(*getModule(), 
                                        EntriesConstant->getType(), true,
                                        llvm::GlobalValue::PrivateLinkage,
                                        EntriesConstant, 
                                        "_swift1_autolink_entries");
    var->setSection(".swift1_autolink_entries");
    var->setAlignment(getPointerAlignment().getValue());

    addUsedGlobal(var);
    break;
  }
  default:
    llvm_unreachable("Don't know how to emit autolink entries for "
                     "the selected object format.");
  }

  if (!Opts.ForceLoadSymbolName.empty()) {
    llvm::SmallString<64> buf;
    encodeForceLoadSymbolName(buf, Opts.ForceLoadSymbolName);
    (void)new llvm::GlobalVariable(Module, Int1Ty, /*constant=*/false,
                                   llvm::GlobalValue::CommonLinkage,
                                   llvm::Constant::getNullValue(Int1Ty),
                                   buf.str());
  }
}

void IRGenModule::finalize() {
  emitAutolinkInfo();
  emitGlobalLists();
  if (DebugInfo)
    DebugInfo->finalize();
}

void IRGenModule::unimplemented(SourceLoc loc, StringRef message) {
  Context.Diags.diagnose(loc, diag::irgen_unimplemented, message);
}

void IRGenModule::fatal_unimplemented(SourceLoc loc, StringRef message) {
  Context.Diags.diagnose(loc, diag::irgen_unimplemented, message);
  llvm::report_fatal_error(llvm::Twine("unimplemented IRGen feature! ") +
                             message);
}

void IRGenModule::error(SourceLoc loc, const Twine &message) {
  SmallVector<char, 128> buffer;
  Context.Diags.diagnose(loc, diag::irgen_failure,
                         message.toStringRef(buffer));
}

void IRGenModuleDispatcher::addGenModule(SourceFile *SF, IRGenModule *IGM) {
  assert(GenModules.count(SF) == 0);
  GenModules[SF] = IGM;
  if (!PrimaryIGM) {
    PrimaryIGM = IGM;
  }
  IGM->dispatcher = this;
  Queue.push_back(IGM);
}

