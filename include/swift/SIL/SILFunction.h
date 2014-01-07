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
#include "swift/SIL/SILLinkage.h"
#include "llvm/ADT/StringMap.h"

/// The symbol name used for the program entry point function.
/// FIXME: Hardcoding this is lame.
#define SWIFT_ENTRY_POINT_FUNCTION "top_level_code"

namespace swift {

class ASTContext;
class SILInstruction;
class SILModule;
  
enum IsBare_t { IsNotBare, IsBare };
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
  
  /// The mangled name of the SIL function, which will be propagated
  /// to the binary.  A pointer into the module's lookup table.
  StringRef Name;

  /// The lowered type of the function.
  CanSILFunctionType LoweredType;
  
  /// The context archetypes of the function.
  GenericParamList *ContextGenericParams;

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

  /// The function's bare attribute. Bare means that the function is SIL-only.
  unsigned Bare : 1;

  /// The function's transparent attribute.
  unsigned Transparent : 1; // FIXME: pack this somewhere
  
  /// This is the number of function_ref instructions using this SILFunction.
  friend class FunctionRefInst;
  friend class SILVTable;
  friend class SILWitnessTable;
  unsigned RefCount = 0;

  SILFunction(SILModule &module, SILLinkage linkage,
              StringRef mangledName, CanSILFunctionType loweredType,
              Optional<SILLocation> loc,
              IsBare_t isBareSILFunction,
              IsTransparent_t isTrans,
              SILFunction *insertBefore,
              SILDebugScope *debugScope,
              DeclContext *DC);

public:
  static SILFunction *create(SILModule &M, SILLinkage linkage, StringRef name,
                             CanSILFunctionType loweredType,
                             Optional<SILLocation> loc = Nothing,
                             IsBare_t isBareSILFunction = IsNotBare,
                             IsTransparent_t isTrans = IsNotTransparent,
                             SILFunction *InsertBefore = nullptr,
                             SILDebugScope *DebugScope = nullptr,
                             DeclContext *DC = nullptr);
  ~SILFunction();

  SILModule &getModule() const { return *ModuleAndLinkage.getPointer(); }

  SILType getLoweredType() const {
    return SILType::getPrimitiveObjectType(LoweredType);
  }
  CanSILFunctionType getLoweredFunctionType() const {
    return LoweredType;
  }
    
  /// Return the number of function_ref instructions that refer to this
  /// function.
  unsigned getRefCount() const { return RefCount; }
  
  /// Returns the calling convention used by this entry point.
  AbstractCC getAbstractCC() const {
    return getLoweredFunctionType()->getAbstractCC();
  }

  StringRef getName() const { return Name; }
  
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
  
  /// Get this function's bare attribute.
  IsBare_t isBare() const { return IsBare_t(Bare); }
  void setBare(IsBare_t isB) { Bare = isB; }

  /// Get this function's transparent attribute.
  IsTransparent_t isTransparent() const { return IsTransparent_t(Transparent); }
  void setTransparent(IsTransparent_t isT) { Transparent = isT; }
    
  /// Retrieve the generic parameter list containing the contextual archetypes
  /// of the function.
  ///
  /// FIXME: We should remove this in favor of lazy archetype instantiation
  /// using the 'getArchetype' and 'mapTypeIntoContext' interfaces.
  GenericParamList *getContextGenericParams() const {
    return ContextGenericParams;
  }
  void setContextGenericParams(GenericParamList *params) {
    ContextGenericParams = params;
  }
    
  /// Map the given type, which is based on an interface SILFunctionType and may
  /// therefore be dependent, to a type based on the context archetypes of this
  /// SILFunction.
  Type mapTypeIntoContext(Type type) const;
    
  /// Map the given type, which is based on an interface SILFunctionType and may
  /// therefore be dependent, to a type based on the context archetypes of this
  /// SILFunction.
  SILType mapTypeIntoContext(SILType type) const {
    return SILType::getPrimitiveType(
             mapTypeIntoContext(type.getSwiftRValueType())->getCanonicalType(),
             type.getCategory());
  }

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
  void dump() const;

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
