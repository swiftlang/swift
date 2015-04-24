//===--- IRGenDebugInfo.h - Debug Info Support-------------------*- C++ -*-===//
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
// This file defines IR codegen support for debug information.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DEBUGINFO_H
#define SWIFT_IRGEN_DEBUGINFO_H

#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/Support/Allocator.h"

#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILBasicBlock.h"

#include "DebugTypeInfo.h"
#include "IRBuilder.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenType.h"

#include <set>

namespace llvm {
class DIBuilder;
}

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

typedef struct {
  unsigned Line, Col;
  const char *Filename;
} Location;

typedef struct { Location LocForLinetable, Loc; } FullLocation;

typedef llvm::DenseMap<const llvm::MDString *, llvm::TrackingMDNodeRef>
    TrackingDIRefMap;

enum IndirectionKind : bool { DirectValue = false, IndirectValue = true };
enum ArtificialKind : bool { RealValue = false, ArtificialValue = true };

/// IRGenDebugInfo - Helper object that keeps track of the current
/// CompileUnit, File, LexicalScope, and translates SILLocations into
/// <llvm::DebugLoc>s.
class IRGenDebugInfo {
  friend class ArtificialLocation;
  const IRGenOptions &Opts;
  ClangImporter &CI;
  SourceManager &SM;
  llvm::Module &M;
  llvm::DIBuilder DBuilder;
  IRGenModule &IGM;

  // Various caches.
  llvm::DenseMap<SILDebugScope *, llvm::TrackingMDNodeRef> ScopeCache;
  llvm::DenseMap<const char *, llvm::TrackingMDNodeRef> DIFileCache;
  llvm::DenseMap<TypeBase *, llvm::TrackingMDNodeRef> DITypeCache;
  std::map<std::string, llvm::TrackingMDNodeRef> DIModuleCache;
  TrackingDIRefMap DIRefMap;

  llvm::SmallString<256> MainFilename;
  llvm::BumpPtrAllocator DebugInfoNames;
  StringRef CWDName;               /// The current working directory.
  llvm::MDCompileUnit *TheCU = nullptr; /// The current compilation unit.
  llvm::MDFile *MainFile     = nullptr; /// The main file.
  llvm::MDModule *MainModule = nullptr; /// The current module.
  llvm::MDNode *EntryPointFn;      /// Scope of SWIFT_ENTRY_POINT_FUNCTION.
  TypeAliasDecl *MetadataTypeDecl; /// The type decl for swift.type.
  llvm::MDType *InternalType;      /// Catch-all type for opaque internal types.

  Location LastDebugLoc;    /// The last location that was emitted.
  SILDebugScope *LastScope; /// The scope of that last location.
  bool IsLibrary;           /// Whether this is a libary or a top level module.
#ifndef NDEBUG
  /// The basic block where the location was last changed.
  llvm::BasicBlock *LastBasicBlock;
  bool lineNumberIsSane(IRBuilder &Builder, unsigned Line);
#endif

  /// Used by pushLoc.
  SmallVector<std::pair<Location, SILDebugScope *>, 8> LocationStack;

  // FIXME: move this to something more local in type generation.
  CanGenericSignature CurGenerics;
  class GenericsRAII {
    IRGenDebugInfo &Self;
    GenericContextScope Scope;
    CanGenericSignature OldGenerics;
  public:
    GenericsRAII(IRGenDebugInfo &self, CanGenericSignature generics)
      : Self(self), Scope(self.IGM, generics), OldGenerics(self.CurGenerics) {
      if (generics) self.CurGenerics = generics;
    }

    ~GenericsRAII() {
      Self.CurGenerics = OldGenerics;
    }
  };

public:
  IRGenDebugInfo(const IRGenOptions &Opts, ClangImporter &CI, IRGenModule &IGM,
                 llvm::Module &M);

  /// Finalize the llvm::DIBuilder owned by this object.
  void finalize();

  /// Update the IRBuilder's current debug location to the location
  /// Loc and the lexical scope DS.
  void setCurrentLoc(IRBuilder &Builder, SILDebugScope *DS,
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
  void setArtificialTrapLocation(IRBuilder &Builder, SILDebugScope *Scope) {
    auto DL = llvm::DebugLoc::get(0, 0, getOrCreateScope(Scope));
    Builder.SetCurrentDebugLocation(DL);
  }

  /// Emit debug info for an import declaration.
  void emitImport(ImportDecl *D);

  /// Emit debug info for the given function.
  /// \param DS The parent scope of the function.
  /// \param Fn The IR representation of the function.
  /// \param Rep The calling convention of the function.
  /// \param Ty The signature of the function.
  llvm::MDSubprogram *emitFunction(SILModule &SILMod, SILDebugScope *DS,
                                   llvm::Function *Fn,
                                   SILFunctionTypeRepresentation Rep,
                                   SILType Ty,
                                   DeclContext *DeclCtx = nullptr);

  /// Emit debug info for a given SIL function.
  llvm::MDSubprogram *emitFunction(SILFunction &SILFn, llvm::Function *Fn);

  /// Convenience function useful for functions without any source
  /// location. Internally calls emitFunction, emits a debug
  /// scope, and finally sets it using setCurrentLoc.
  inline void emitArtificialFunction(IRGenFunction &IGF, llvm::Function *Fn,
                                     SILType SILTy = SILType()) {
    emitArtificialFunction(*IGF.IGM.SILMod, IGF.Builder, Fn, SILTy);
  }

  void emitArtificialFunction(SILModule &SILMod, IRBuilder &Builder,
                              llvm::Function *Fn, SILType SILTy = SILType());

  /// Emit a dbg.declare instrinsic at the current insertion point and
  /// the Builder's current debug location.
  /// \param Tag The DWARF tag that should be used.
  void emitVariableDeclaration(IRBuilder &Builder,
                               ArrayRef<llvm::Value *> Storage,
                               DebugTypeInfo Ty, SILDebugScope *DS,
                               StringRef Name, unsigned Tag, unsigned ArgNo = 0,
                               IndirectionKind = DirectValue,
                               ArtificialKind = RealValue);

  /// Convenience function for stack-allocated variables. Calls
  /// emitVariableDeclaration internally.
  void emitStackVariableDeclaration(IRBuilder &Builder,
                                    ArrayRef<llvm::Value *> Storage,
                                    DebugTypeInfo Ty, SILDebugScope *DS,
                                    StringRef Name,
                                    IndirectionKind Indirection = DirectValue);

  /// Convenience function for variables that are function arguments.
  void emitArgVariableDeclaration(IRBuilder &Builder,
                                  ArrayRef<llvm::Value *> Storage,
                                  DebugTypeInfo Ty, SILDebugScope *DS,
                                  StringRef Name, unsigned ArgNo,
                                  IndirectionKind = DirectValue,
                                  ArtificialKind = RealValue);

  /// Emit a dbg.declare or dbg.value intrinsic, depending on Storage.
  void emitDbgIntrinsic(llvm::BasicBlock *BB, llvm::Value *Storage,
                        llvm::MDLocalVariable *Var, llvm::MDExpression *Expr,
                        unsigned Line, unsigned Col, llvm::MDLocalScope *Scope,
                        SILDebugScope *DS);

  /// Create debug metadata for a global variable.
  void emitGlobalVariableDeclaration(llvm::GlobalValue *Storage, StringRef Name,
                                     StringRef LinkageName,
                                     DebugTypeInfo DebugType,
                                     Optional<SILLocation> Loc);

  /// Emit debug metadata for type metadata (for generic types). So meta.
  void emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata,
                        StringRef Name);

  /// Return the native, absolute path to the main file.
  StringRef getMainFilename() const { return MainFilename; }

  /// Return the DIBuilder.
  llvm::DIBuilder &getBuilder() { return DBuilder; }

  /// Removes the function from the Functions map again.
  void eraseFunction(llvm::Function *Fn);

private:
  StringRef BumpAllocatedString(const char *Data, size_t Length);
  StringRef BumpAllocatedString(std::string S);
  StringRef BumpAllocatedString(StringRef S);

  void createImportedModule(StringRef Name, StringRef MangledPrefix,
                            llvm::MDModule *Module, unsigned Line);

  llvm::MDType *createType(DebugTypeInfo DbgTy, StringRef MangledName,
                           llvm::MDScope *Scope, llvm::MDFile *File);
  llvm::MDType *getOrCreateType(DebugTypeInfo DbgTy);
  llvm::MDScope *getOrCreateScope(SILDebugScope *DS);
  llvm::MDScope *getOrCreateContext(DeclContext *DC);
  llvm::MDNode *createInlinedAt(SILDebugScope *Scope);

  StringRef getCurrentDirname();
  llvm::MDFile *getOrCreateFile(const char *Filename);
  llvm::MDType *getOrCreateDesugaredType(Type Ty, DebugTypeInfo DTI);
  StringRef getName(const FuncDecl &FD);
  StringRef getName(SILLocation L);
  StringRef getMangledName(TypeAliasDecl *Decl);
  StringRef getMangledName(DebugTypeInfo DTI);
  llvm::MDTypeRefArray createParameterTypes(CanSILFunctionType FnTy,
                                            DeclContext *DeclCtx);
  llvm::MDTypeRefArray createParameterTypes(SILType SILTy,
                                            DeclContext *DeclCtx);
  void createParameterType(llvm::SmallVectorImpl<llvm::Metadata *> &Parameters,
                           SILType CanTy, DeclContext *DeclCtx);
  llvm::DebugNodeArray getTupleElements(TupleType *TupleTy,
                                        llvm::MDScope *Scope,
                                        llvm::MDFile *File, unsigned Flags,
                                        DeclContext *DeclContext,
                                        unsigned &SizeInBits);
  llvm::MDFile *getFile(llvm::MDScope *Scope);
  llvm::MDModule *getOrCreateModule(llvm::MDScope *Parent, std::string Name,
                                    llvm::MDFile *File);
  llvm::MDScope *getModule(StringRef MangledName);
  llvm::DebugNodeArray getStructMembers(NominalTypeDecl *D, Type BaseTy,
                                        llvm::MDScope *Scope,
                                        llvm::MDFile *File,
                                        unsigned Flags, unsigned &SizeInBits);
  llvm::MDCompositeType *
  createStructType(DebugTypeInfo DbgTy, NominalTypeDecl *Decl, Type BaseTy,
                   llvm::MDScope *Scope, llvm::MDFile *File, unsigned Line,
                   unsigned SizeInBits, unsigned AlignInBits, unsigned Flags,
                   llvm::MDType *DerivedFrom, unsigned RuntimeLang,
                   StringRef UniqueID);
  llvm::MDDerivedType *createMemberType(DebugTypeInfo DTI, StringRef Name,
                                        unsigned &OffsetInBits,
                                        llvm::MDScope *Scope,
                                        llvm::MDFile *File, unsigned Flags);
  llvm::DebugNodeArray getEnumElements(DebugTypeInfo DbgTy, EnumDecl *D,
                                       llvm::MDScope *Scope, llvm::MDFile *File,
                                       unsigned Flags);
  llvm::MDCompositeType *createEnumType(DebugTypeInfo DbgTy, EnumDecl *Decl,
                                        StringRef MangledName,
                                        llvm::MDScope *Scope,
                                        llvm::MDFile *File, unsigned Line,
                                        unsigned Flags);
  llvm::MDType *createPointerSizedStruct(llvm::MDScope *Scope,
                                        StringRef Name, llvm::MDFile *File,
                                        unsigned Line, unsigned Flags,
                                        StringRef MangledName);
  llvm::MDType *createPointerSizedStruct(llvm::MDScope *Scope,
                                        StringRef Name, llvm::MDType *PointeeTy,
                                        llvm::MDFile *File, unsigned Line,
                                        unsigned Flags, StringRef MangledName);
  uint64_t getSizeOfBasicType(DebugTypeInfo DbgTy);
  TypeAliasDecl *getMetadataType();
};

/// \brief An RAII object that autorestores the debug location.
class AutoRestoreLocation {
  IRGenDebugInfo *DI;
public:
  AutoRestoreLocation(IRGenDebugInfo *DI) : DI(DI) {
    if (DI)
      DI->pushLoc();
  }

  /// \brief Autorestore everything back to normal.
  ~AutoRestoreLocation() {
    if (DI)
      DI->popLoc();
  }
};

/// \brief An RAII object that temporarily switches to
/// an artificial debug location that has a valid scope, but no line
/// information. This is useful when emitting compiler-generated
/// instructions (e.g., ARC-inserted calls to release()) that have no
/// source location associated with them. The DWARF specification
/// allows the compiler to use the special line number 0 to indicate
/// code that can not be attributed to any source location.
class ArtificialLocation : public AutoRestoreLocation {
public:
  /// \brief Set the current location to line 0, but within scope DS.
  ArtificialLocation(SILDebugScope *DS, IRGenDebugInfo *DI, IRBuilder &Builder)
    : AutoRestoreLocation(DI) {
    if (DI) {
      auto DL = llvm::DebugLoc::get(0, 0, DI->getOrCreateScope(DS));
      Builder.SetCurrentDebugLocation(DL);
    }
  }
};

/// \brief An RAII object that temporarily switches to an
/// empty location. This is how the function prologue is represented.
class PrologueLocation : public AutoRestoreLocation {
public:
  /// \brief Set the current location to an empty location.
  PrologueLocation(IRGenDebugInfo *DI, IRBuilder &Builder)
    : AutoRestoreLocation(DI) {
    if (DI)
      DI->clearLoc(Builder);
  }
};

} // irgen
} // swift

#endif
