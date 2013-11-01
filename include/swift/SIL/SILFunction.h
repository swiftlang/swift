//===--- SILFunction.h - Defines the SILFunction class ----------*- C++ -*-===//
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
// This file defines the SILFunction class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILFUNCTION_H
#define SWIFT_SIL_SILFUNCTION_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/AST/KernelOrShaderKind.h"

namespace swift {

class ASTContext;
class SILInstruction;
class SILModule;
  
/// Linkage attribute for a SIL function.
enum class SILLinkage : unsigned char {
  External,
  Thunk,
  Internal,
  Deserialized // Deserialized from a module.
};
  
enum IsTransparent_t { IsNotTransparent, IsTransparent };

/// SILFunction - A function body that has been lowered to SIL. This consists of
/// zero or more SIL SILBasicBlock objects that contain the SILInstruction
/// objects making up the function.
class SILFunction
  : public llvm::ilist_node<SILFunction>, public SILAllocated<SILFunction> {
public:
  typedef llvm::iplist<SILBasicBlock> BlockListType;

private:
  friend class SILBasicBlock;
  friend class SILModule;

  /// ModuleAndLinkage - The SIL module that the function belongs to, and
  /// the function's linkage.
  llvm::PointerIntPair<SILModule*, 2, SILLinkage> ModuleAndLinkage;
  
  /// The mangled name of the SIL function, which will be propagated to the
  /// binary.
  std::string Name;

  /// The lowered type of the function.
  SILType LoweredType;
  
  /// The collection of all BasicBlocks in the SILFunction. Empty for external
  /// function references.
  BlockListType BlockList;

  /// The SIL location of the function, which provides a link back to the AST.
  /// The function only gets a location after it's been emitted.
  Optional<SILLocation> Location;

  /// The declcontext of this function.
  DeclContext *DeclCtx;

  /// The source location and scope of the function.
  SILDebugScope *DebugScope;

  /// Whether this is a compute kernel/graphics shader.
  KernelOrShaderKind KernelOrShader;

  /// The function's transparent attribute.
  unsigned Transparent : 1; // FIXME: pack this somewhere
  
  /// This is the number of function_ref instructions using this SILFunction.
  friend class FunctionRefInst;
  unsigned RefCount = 0;

public:

  SILFunction(SILModule &Module, SILLinkage Linkage,
              StringRef MangledName, SILType LoweredType,
              Optional<SILLocation> Loc = Nothing,
              KernelOrShaderKind KOS = KernelOrShaderKind::Default,
              IsTransparent_t isTrans = IsNotTransparent);

  ~SILFunction();

  SILModule &getModule() const { return *ModuleAndLinkage.getPointer(); }

  SILType getLoweredType() const { return LoweredType; }
  SILFunctionType *getFunctionTypeInfo() const {
    return LoweredType.getFunctionTypeInfo(getModule());
  }
    
  /// Return the number of function_ref instructions that refer to this
  /// function.
  unsigned getRefCount() const { return RefCount; }
  
  /// Returns the calling convention used by this entry point.
  AbstractCC getAbstractCC() const {
    return getLoweredType().getAbstractCC();
  }

  StringRef getName() const { return Name; }
  void setName(StringRef N) {
    Name = N;
  }
  
  std::string &getMutableName() { return Name; }

  /// True if this is a declaration of a function defined in another module.
  bool isExternalDeclaration() const { return BlockList.empty(); }
  
  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return ModuleAndLinkage.getInt(); }
  void setLinkage(SILLinkage L) { ModuleAndLinkage.setInt(L); }

  /// Get the DeclContext of this function. (Debug info only).
  DeclContext *getDeclContext() const { return DeclCtx; }
  void setDeclContext(Decl *D);
  void setDeclContext(Expr *E);

  /// Initialize the source location of the function.
  void setLocation(SILLocation L) { Location = L; }

  /// Check if the function has a location.
  /// FIXME: All functions should have locations, so this method should not be
  /// necessary.
  bool hasLocation() const {
    return Location.hasValue();
  }

  /// Get the source location of the function.
  SILLocation getLocation() const {
    assert(Location.hasValue());
    return Location.getValue();
  }

  /// Initialize the debug scope of the function.
  void setDebugScope(SILDebugScope *DS) { DebugScope = DS; }

  /// Get the source location of the function.
  SILDebugScope *getDebugScope() const { return DebugScope; }
  
  /// Axle-related accessors.
  KernelOrShaderKind getKernelOrShaderKind() const {
    return KernelOrShader;
  }
  bool isKernel() const {
    return KernelOrShader == KernelOrShaderKind::Kernel;
  }
  bool isVertex() const {
    return KernelOrShader == KernelOrShaderKind::Vertex;
  }
  bool isFragment() const {
    return KernelOrShader == KernelOrShaderKind::Fragment;
  }
  
  /// Get this function's transparent attribute.
  IsTransparent_t isTransparent() const { return IsTransparent_t(Transparent); }
  void setTransparent(IsTransparent_t isT) { Transparent = isT; }

  //===--------------------------------------------------------------------===//
  // Block List Access
  //===--------------------------------------------------------------------===//

  BlockListType &getBlocks() { return BlockList; }
  const BlockListType &getBlocks() const { return BlockList; }

  typedef BlockListType::iterator iterator;
  typedef BlockListType::const_iterator const_iterator;

  bool empty() const { return BlockList.empty(); }
  iterator begin() { return BlockList.begin(); }
  iterator end() { return BlockList.end(); }
  const_iterator begin() const { return BlockList.begin(); }
  const_iterator end() const { return BlockList.end(); }
  unsigned size() const { return BlockList.size(); }

  SILBasicBlock &front() { return *begin(); }
  const SILBasicBlock &front() const { return *begin(); }

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the SILFunction follows
  /// invariants.
  void verify() const;
  
  /// Pretty-print the SILFunction.
  void dump(bool Verbose) const;
  void dump() const { dump(false); }

  /// Pretty-print the SILFunction with the designated stream as a 'sil'
  /// definition.
  ///
  /// \param Verbose In verbose mode, print the SIL locations.
  void print(raw_ostream &OS, bool Verbose = false) const;
  
  /// Pretty-print the SILFunction's name using SIL syntax,
  /// '@function_mangled_name'.
  void printName(raw_ostream &OS) const;
  
  ASTContext &getASTContext() const;
};
  
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const SILFunction &F) {
  F.print(OS);
  return OS;
}

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILFunction
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILFunction> :
public ilist_default_traits<::swift::SILFunction> {
  typedef ::swift::SILFunction SILFunction;

private:
  mutable ilist_half_node<SILFunction> Sentinel;

public:
  SILFunction *createSentinel() const {
    return static_cast<SILFunction*>(&Sentinel);
  }
  void destroySentinel(SILFunction *) const {}

  SILFunction *provideInitialHead() const { return createSentinel(); }
  SILFunction *ensureHead(SILFunction*) const { return createSentinel(); }
  static void noteHead(SILFunction*, SILFunction*) {}
  static void deleteNode(SILFunction *V) {}
  
private:
  void createNode(const SILFunction &);
};

} // end llvm namespace

#endif
