//===--- IRGenDebugInfo.h - Debug Info Support ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines IR codegen support for debug information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DEBUGINFO_H
#define SWIFT_IRGEN_DEBUGINFO_H

#include "DebugTypeInfo.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "swift/SIL/SILLocation.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Support/Allocator.h"

#include <set>

namespace swift {

class ASTContext;
class AllocStackInst;
class ClangImporter;
class IRGenOptions;
class SILArgument;
class SILDebugScope;
class SILModule;

enum class SILFunctionTypeRepresentation : uint8_t;

namespace irgen {

class IRGenFunction;

typedef llvm::DenseMap<const llvm::MDString *, llvm::TrackingMDNodeRef>
    TrackingDIRefMap;

enum IndirectionKind : bool { DirectValue = false, IndirectValue = true };
enum ArtificialKind : bool { RealValue = false, ArtificialValue = true };

/// Helper object that keeps track of the current CompileUnit, File,
/// LexicalScope, and knows how to translate a \c SILLocation into an
/// \c llvm::DebugLoc.
class IRGenDebugInfo {
  friend class ArtificialLocation;
  const IRGenOptions &Opts;
  ClangImporter &CI;
  SourceManager &SM;
  llvm::Module &M;
  llvm::DIBuilder DBuilder;
  IRGenModule &IGM;

  // Various caches.
  llvm::DenseMap<const SILDebugScope *, llvm::TrackingMDNodeRef> ScopeCache;
  llvm::DenseMap<llvm::StringRef, llvm::TrackingMDNodeRef> DIFileCache;
  llvm::DenseMap<TypeBase *, llvm::TrackingMDNodeRef> DITypeCache;
  llvm::StringMap<llvm::TrackingMDNodeRef> DIModuleCache;
  TrackingDIRefMap DIRefMap;
  std::vector<std::pair<const SILDebugScope *, llvm::TrackingMDNodeRef>>
      LastInlineChain;

  /// A list of replaceable fwddecls that need to be RAUWed at the end.
  std::vector<std::pair<TypeBase *, llvm::TrackingMDRef>> ReplaceMap;

  llvm::BumpPtrAllocator DebugInfoNames;
  StringRef CWDName;                    /// The current working directory.
  llvm::DICompileUnit *TheCU = nullptr; /// The current compilation unit.
  llvm::DIFile *MainFile = nullptr;     /// The main file.
  llvm::DIModule *MainModule = nullptr; /// The current module.
  llvm::DIScope *EntryPointFn =
      nullptr;                          /// Scope of SWIFT_ENTRY_POINT_FUNCTION.
  TypeAliasDecl *MetadataTypeDecl;      /// The type decl for swift.type.
  llvm::DIType *InternalType; /// Catch-all type for opaque internal types.

  SILLocation::DebugLoc LastDebugLoc; /// The last location that was emitted.
  const SILDebugScope *LastScope;     /// The scope of that last location.
#ifndef NDEBUG
  /// The basic block where the location was last changed.
  llvm::BasicBlock *LastBasicBlock;
  bool lineNumberIsSane(IRBuilder &Builder, unsigned Line);
#endif

  /// Used by pushLoc.
  SmallVector<std::pair<SILLocation::DebugLoc, const SILDebugScope *>, 8>
      LocationStack;

public:
  IRGenDebugInfo(const IRGenOptions &Opts, ClangImporter &CI, IRGenModule &IGM,
                 llvm::Module &M, SourceFile *SF);

  /// Finalize the llvm::DIBuilder owned by this object.
  void finalize();

  /// Update the IRBuilder's current debug location to the location
  /// Loc and the lexical scope DS.
  void setCurrentLoc(IRBuilder &Builder, const SILDebugScope *DS,
                     Optional<SILLocation> Loc = None);

  void clearLoc(IRBuilder &Builder) {
    LastDebugLoc = {};
    LastScope = nullptr;
    Builder.SetCurrentDebugLocation(llvm::DebugLoc());
  }

  /// Push the current debug location onto a stack and initialize the
  /// IRBuilder to an empty location.
  void pushLoc() {
    LocationStack.push_back(std::make_pair(LastDebugLoc, LastScope));
    LastDebugLoc = {};
    LastScope = nullptr;
  }

  /// Restore the current debug location from the stack.
  void popLoc() {
    std::tie(LastDebugLoc, LastScope) = LocationStack.pop_back_val();
  }

  /// Emit the final line 0 location for the unified trap block at the
  /// end of the function.
  void setArtificialTrapLocation(IRBuilder &Builder,
                                 const SILDebugScope *Scope) {
    auto DL = llvm::DebugLoc::get(0, 0, getOrCreateScope(Scope));
    Builder.SetCurrentDebugLocation(DL);
  }

  /// Set the location for SWIFT_ENTRY_POINT_FUNCTION.
  void setEntryPointLoc(IRBuilder &Builder);

  /// Return the scope for SWIFT_ENTRY_POINT_FUNCTION.
  llvm::DIScope *getEntryPointFn();
  
  /// Emit debug info for an import declaration.
  ///
  /// The DWARF output for import decls is similar to that of a using
  /// directive in C++:
  ///   import Foundation
  ///   -->
  ///   0: DW_TAG_imported_module
  ///        DW_AT_import(*1)
  ///   1: DW_TAG_module // instead of DW_TAG_namespace.
  ///        DW_AT_name("Foundation")
  ///
  void emitImport(ImportDecl *D);

  /// Emit debug info for the given function.
  /// \param DS The parent scope of the function.
  /// \param Fn The IR representation of the function.
  /// \param Rep The calling convention of the function.
  /// \param Ty The signature of the function.
  llvm::DISubprogram *emitFunction(const SILDebugScope *DS, llvm::Function *Fn,
                                   SILFunctionTypeRepresentation Rep,
                                   SILType Ty, DeclContext *DeclCtx = nullptr,
                                   GenericEnvironment *GE = nullptr);

  /// Emit debug info for a given SIL function.
  llvm::DISubprogram *emitFunction(SILFunction &SILFn, llvm::Function *Fn);

  /// Convenience function useful for functions without any source
  /// location. Internally calls emitFunction, emits a debug
  /// scope, and finally sets it using setCurrentLoc.
  inline void emitArtificialFunction(IRGenFunction &IGF, llvm::Function *Fn,
                                     SILType SILTy = SILType()) {
    emitArtificialFunction(IGF.Builder, Fn, SILTy);
  }

  void emitArtificialFunction(IRBuilder &Builder,
                              llvm::Function *Fn, SILType SILTy = SILType());

  /// Emit a dbg.declare intrinsic at the current insertion point and
  /// the Builder's current debug location.
  void emitVariableDeclaration(IRBuilder &Builder,
                               ArrayRef<llvm::Value *> Storage,
                               DebugTypeInfo Ty, const SILDebugScope *DS,
                               ValueDecl *VarDecl, StringRef Name,
                               unsigned ArgNo = 0,
                               IndirectionKind = DirectValue,
                               ArtificialKind = RealValue);

  /// Emit a dbg.declare or dbg.value intrinsic, depending on Storage.
  void emitDbgIntrinsic(IRBuilder &Builder, llvm::Value *Storage,
                        llvm::DILocalVariable *Var, llvm::DIExpression *Expr,
                        unsigned Line, unsigned Col, llvm::DILocalScope *Scope,
                        const SILDebugScope *DS);

  /// Create debug metadata for a global variable.
  void emitGlobalVariableDeclaration(llvm::GlobalVariable *Storage,
                                     StringRef Name, StringRef LinkageName,
                                     DebugTypeInfo DebugType,
                                     bool IsLocalToUnit,
                                     Optional<SILLocation> Loc);

  /// Emit debug metadata for type metadata (for generic types). So meta.
  void emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata,
                        StringRef Name);

  /// Return the DIBuilder.
  llvm::DIBuilder &getBuilder() { return DBuilder; }

private:
  StringRef BumpAllocatedString(const char *Data, size_t Length);
  StringRef BumpAllocatedString(std::string S);
  StringRef BumpAllocatedString(StringRef S);

  /// Construct a DIType from a DebugTypeInfo object.
  ///
  /// At this point we do not plan to emit full DWARF for all swift
  /// types, the goal is to emit only the name and provenance of the
  /// type, where possible. A can import the type definition directly
  /// from the module/framework/source file the type is specified in.
  /// For this reason we emit the fully qualified (=mangled) name for
  /// each type whenever possible.
  ///
  /// The ultimate goal is to emit something like a
  /// DW_TAG_APPLE_ast_ref_type (an external reference) instead of a
  /// local reference to the type.
  llvm::DIType *createType(DebugTypeInfo DbgTy, StringRef MangledName,
                           llvm::DIScope *Scope, llvm::DIFile *File);
  /// Get a previously created type from the cache.
  llvm::DIType *getTypeOrNull(TypeBase *Ty);
  /// Get the DIType corresponding to this DebugTypeInfo from the cache,
  /// or build a fresh DIType otherwise.  There is the underlying
  /// assumption that no two types that share the same canonical type
  /// can have different storage size or alignment.
  llvm::DIType *getOrCreateType(DebugTypeInfo DbgTy);
  /// Translate a SILDebugScope into an llvm::DIDescriptor.
  llvm::DIScope *getOrCreateScope(const SILDebugScope *DS);
  /// Build the context chain for a given DeclContext.
  llvm::DIScope *getOrCreateContext(DeclContext *DC);

  /// Construct an LLVM inlined-at location from a SILDebugScope,
  /// reversing the order in the process.
  llvm::MDNode *createInlinedAt(const SILDebugScope *CallSite);

  /// Translate filenames into DIFiles.
  llvm::DIFile *getOrCreateFile(StringRef Filename);
  /// Return a DIType for Ty reusing any DeclContext found in DbgTy.
  llvm::DIType *getOrCreateDesugaredType(Type Ty, DebugTypeInfo DbgTy);

  /// Attempt to figure out the unmangled name of a function.
  StringRef getName(const FuncDecl &FD);
  /// Attempt to figure out the unmangled name of a function.
  StringRef getName(SILLocation L);
  StringRef getMangledName(TypeAliasDecl *Decl);
  /// Return the mangled name of any nominal type, including the global
  /// _Tt prefix, which marks the Swift namespace for types in DWARF.
  StringRef getMangledName(DebugTypeInfo DbgTy);
  /// Create the array of function parameters for a function type.
  llvm::DITypeRefArray createParameterTypes(CanSILFunctionType FnTy,
                                            DeclContext *DeclCtx,
                                            GenericEnvironment *GE);
  /// Create the array of function parameters for FnTy. SIL Version.
  llvm::DITypeRefArray createParameterTypes(SILType SILTy, DeclContext *DeclCtx,
                                            GenericEnvironment *GE);
  /// Create a single parameter type and push it.
  void createParameterType(llvm::SmallVectorImpl<llvm::Metadata *> &Parameters,
                           SILType CanTy, DeclContext *DeclCtx,
                           GenericEnvironment *GE);
  /// Return an array with the DITypes for each of a tuple's elements.
  llvm::DINodeArray
  getTupleElements(TupleType *TupleTy, llvm::DIScope *Scope, llvm::DIFile *File,
                   llvm::DINode::DIFlags Flags, DeclContext *DeclContext,
                   GenericEnvironment *GE, unsigned &SizeInBits);
  llvm::DIFile *getFile(llvm::DIScope *Scope);
  llvm::DIModule *getOrCreateModule(ModuleDecl::ImportedModule M);
  /// Return a cached module for an access path or create a new one.
  llvm::DIModule *getOrCreateModule(StringRef Key, llvm::DIScope *Parent,
                                    StringRef Name, StringRef IncludePath);
  llvm::DIScope *getModule(StringRef MangledName);
  /// Return an array with the DITypes for each of a struct's elements.
  llvm::DINodeArray getStructMembers(NominalTypeDecl *D, Type BaseTy,
                                     llvm::DIScope *Scope, llvm::DIFile *File,
                                     llvm::DINode::DIFlags Flags,
                                     unsigned &SizeInBits);
  /// Create a temporary forward declaration for a struct and add it to
  /// the type cache so we can safely build recursive types.
  llvm::DICompositeType *
  createStructType(DebugTypeInfo DbgTy, NominalTypeDecl *Decl, Type BaseTy,
                   llvm::DIScope *Scope, llvm::DIFile *File, unsigned Line,
                   unsigned SizeInBits, unsigned AlignInBits,
                   llvm::DINode::DIFlags Flags,
                   llvm::DIType *DerivedFrom, unsigned RuntimeLang,
                   StringRef UniqueID);

  /// Create a member of a struct, class, tuple, or enum.
  llvm::DIDerivedType *createMemberType(DebugTypeInfo DbgTy, StringRef Name,
                                        unsigned &OffsetInBits,
                                        llvm::DIScope *Scope,
                                        llvm::DIFile *File,
                                        llvm::DINode::DIFlags Flags);
  /// Return an array with the DITypes for each of an enum's elements.
  llvm::DINodeArray getEnumElements(DebugTypeInfo DbgTy, EnumDecl *D,
                                    llvm::DIScope *Scope, llvm::DIFile *File,
                                    llvm::DINode::DIFlags Flags);
  /// Create a temporary forward declaration for an enum and add it to
  /// the type cache so we can safely build recursive types.
  llvm::DICompositeType *createEnumType(DebugTypeInfo DbgTy, EnumDecl *Decl,
                                        StringRef MangledName,
                                        llvm::DIScope *Scope,
                                        llvm::DIFile *File, unsigned Line,
                                        llvm::DINode::DIFlags Flags);
  /// Convenience function that creates a forward declaration for PointeeTy.
  llvm::DIType *createPointerSizedStruct(llvm::DIScope *Scope, StringRef Name,
                                         llvm::DIFile *File, unsigned Line,
                                         llvm::DINode::DIFlags Flags,
                                         StringRef MangledName);
  /// Create a pointer-sized struct with a mangled name and a single
  /// member of PointeeTy.
  llvm::DIType *createPointerSizedStruct(llvm::DIScope *Scope, StringRef Name,
                                         llvm::DIType *PointeeTy,
                                         llvm::DIFile *File, unsigned Line,
                                         llvm::DINode::DIFlags Flags,
                                         StringRef MangledName);
  /// Create a 2*pointer-sized struct with a mangled name and a single
  /// member of PointeeTy.
  llvm::DIType *createDoublePointerSizedStruct(
      llvm::DIScope *Scope, StringRef Name, llvm::DIType *PointeeTy,
      llvm::DIFile *File, unsigned Line, llvm::DINode::DIFlags Flags,
      StringRef MangledName);

  /// Create DWARF debug info for a function pointer type.
  llvm::DIType *createFunctionPointer(DebugTypeInfo DbgTy, llvm::DIScope *Scope,
                                      unsigned SizeInBits, unsigned AlignInBits,
                                      llvm::DINode::DIFlags Flags,
                                      StringRef MangledName);

  /// Create DWARF debug info for a tuple type.
  llvm::DIType *createTuple(DebugTypeInfo DbgTy, llvm::DIScope *Scope,
                            unsigned SizeInBits, unsigned AlignInBits,
                            llvm::DINode::DIFlags Flags, StringRef MangledName);

  /// Create an opaque struct with a mangled name.
  llvm::DIType *createOpaqueStruct(llvm::DIScope *Scope, StringRef Name,
                                   llvm::DIFile *File, unsigned Line,
                                   unsigned SizeInBits, unsigned AlignInBits,
                                   llvm::DINode::DIFlags Flags,
                                   StringRef MangledName);
  uint64_t getSizeOfBasicType(DebugTypeInfo DbgTy);
  TypeAliasDecl *getMetadataType();
};

/// An RAII object that autorestores the debug location.
class AutoRestoreLocation {
  IRGenDebugInfo *DI;
  IRBuilder &Builder;
  llvm::DebugLoc SavedLocation;

public:
  AutoRestoreLocation(IRGenDebugInfo *DI, IRBuilder &Builder)
      : DI(DI), Builder(Builder) {
    if (DI)
      SavedLocation = Builder.getCurrentDebugLocation();
  }

  /// Autorestore everything back to normal.
  ~AutoRestoreLocation() {
    if (DI)
      Builder.SetCurrentDebugLocation(SavedLocation);
  }
};

/// An RAII object that temporarily switches to an artificial debug
/// location that has a valid scope, but no line information. This is
/// useful when emitting compiler-generated instructions (e.g.,
/// ARC-inserted calls to release()) that have no source location
/// associated with them. The DWARF specification allows the compiler
/// to use the special line number 0 to indicate code that cannot be
/// attributed to any source location.
class ArtificialLocation : public AutoRestoreLocation {
public:
  /// Set the current location to line 0, but within scope DS.
  ArtificialLocation(const SILDebugScope *DS, IRGenDebugInfo *DI,
                     IRBuilder &Builder)
      : AutoRestoreLocation(DI, Builder) {
    if (DI) {
      auto DL = llvm::DebugLoc::get(0, 0, DI->getOrCreateScope(DS));
      Builder.SetCurrentDebugLocation(DL);
    }
  }
};

/// An RAII object that temporarily switches to an empty
/// location. This is how the function prologue is represented.
class PrologueLocation : public AutoRestoreLocation {
public:
  /// Set the current location to an empty location.
  PrologueLocation(IRGenDebugInfo *DI, IRBuilder &Builder)
      : AutoRestoreLocation(DI, Builder) {
    if (DI)
      DI->clearLoc(Builder);
  }
};

} // irgen
} // swift

#endif
