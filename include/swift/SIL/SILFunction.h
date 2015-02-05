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
#define SWIFT_ENTRY_POINT_FUNCTION "main"

namespace swift {

class ASTContext;
class SILInstruction;
class SILModule;

enum IsBare_t { IsNotBare, IsBare };
enum IsTransparent_t { IsNotTransparent, IsTransparent };
enum Inline_t { InlineDefault, NoInline, AlwaysInline };
enum IsThunk_t { IsNotThunk, IsThunk };

/// SILFunction - A function body that has been lowered to SIL. This consists of
/// zero or more SIL SILBasicBlock objects that contain the SILInstruction
/// objects making up the function.
class SILFunction
  : public llvm::ilist_node<SILFunction>, public SILAllocated<SILFunction> {
public:
  typedef llvm::iplist<SILBasicBlock> BlockListType;

  /// The visibility of this method's class (if any).
  enum ClassVisibility_t {
    
    /// This is a method in the vtable of a public class.
    PublicClass,
    
    /// This is a method in the vtable of an internal class.
    InternalClass,
    
    /// All other cases (e.g. this function is not a method).
    NotRelevant
  };
    
private:
  friend class SILBasicBlock;
  friend class SILModule;
    
  /// Module - The SIL module that the function belongs to.
  SILModule &Module;

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

  /// The function's fragile attribute.
  ///
  /// Fragile means that the function can be inlined into another module.
  /// Currently this flag is set for public transparent functions and for all
  /// functions in the stdlib.
  unsigned Fragile : 1;

  /// Specifies if this function is a thunk.
  ///
  /// The inliner uses this information to avoid inlining (non-trivial)
  /// functions into the thunk.
  unsigned Thunk : 1;

  /// The visiblity of the parent class, if this is a method which is contained
  /// in the vtable of that class.
  unsigned ClassVisibility : 2;
    
  /// The function's global_init attribute.
  unsigned GlobalInitFlag : 1;

  /// The function's noinline attribute.
  unsigned InlineStrategy : 2;

  /// The linkage of the function.
  unsigned Linkage : NumSILLinkageBits;

  /// This is the number of uses of this SILFunction inside the SIL.
  /// It does not include references from debug scopes.
  unsigned RefCount = 0;

  /// The function's semantics attribute.
  std::string SemanticsAttr;

  /// The function's effects attribute.
  EffectsKind EK;
    
  /// True if this function is inlined at least once. This means that the
  /// debug info keeps a pointer to this function.
  bool Inlined = false;

  /// True if this function is a zombie function. This means that the function
  /// is dead and not referenced from anywhere inside the SIL. But it is kept
  /// for other purposes:
  /// *) It is inlined and the debug info keeps a reference to the function.
  /// *) It is a dead method of a class which has higher visibility than the
  ///    method itself. In this case we need to create a vtable stub for it.
  bool Zombie = false;

  SILFunction(SILModule &module, SILLinkage linkage,
              StringRef mangledName, CanSILFunctionType loweredType,
              GenericParamList *contextGenericParams,
              Optional<SILLocation> loc,
              IsBare_t isBareSILFunction,
              IsTransparent_t isTrans,
              IsFragile_t isFragile,
              IsThunk_t isThunk,
              ClassVisibility_t classVisibility,
              Inline_t inlineStrategy, EffectsKind E,
              SILFunction *insertBefore,
              SILDebugScope *debugScope,
              DeclContext *DC);

public:
  static SILFunction *create(SILModule &M, SILLinkage linkage, StringRef name,
                             CanSILFunctionType loweredType,
                             GenericParamList *contextGenericParams,
                             Optional<SILLocation> loc,
                             IsBare_t isBareSILFunction,
                             IsTransparent_t isTrans,
                             IsFragile_t isFragile,
                             IsThunk_t isThunk = IsNotThunk,
                             ClassVisibility_t classVisibility = NotRelevant,
                             Inline_t inlineStrategy = InlineDefault,
                             EffectsKind EK = EffectsKind::Unspecified,
                             SILFunction *InsertBefore = nullptr,
                             SILDebugScope *DebugScope = nullptr,
                             DeclContext *DC = nullptr);
  ~SILFunction();

  SILModule &getModule() const { return Module; }

  SILType getLoweredType() const {
    return SILType::getPrimitiveObjectType(LoweredType);
  }
  CanSILFunctionType getLoweredFunctionType() const {
    return LoweredType;
  }

  /// Unsafely rewrite the lowered type of this function.
  ///
  /// This routine does not touch the entry block arguments
  /// or return instructions; you need to do that yourself
  /// if you care.
  ///
  /// This is a hack and should be removed!
  void rewriteLoweredTypeUnsafe(CanSILFunctionType newType) {
    assert(canBeDeleted());
    LoweredType = newType;
  }

  bool canBeDeleted() const {
    return !getRefCount() && !isZombie();
  }

  /// Return the number of entities referring to this function (other
  /// than the SILModule).
  unsigned getRefCount() const { return RefCount; }

  /// Increment the reference count.
  void incrementRefCount() {
    RefCount++;
    assert(RefCount != 0 && "Overflow of reference count!");
  }

  /// Decrement the reference count.
  void decrementRefCount() {
    assert(RefCount != 0 && "Expected non-zero reference count on decrement!");
    RefCount--;
  }

  /// Drops all uses belonging to instructions in this function. The only valid
  /// operation performable on this object after this is called is called the
  /// destructor or deallocation.
  void dropAllReferences() {
    for (SILBasicBlock &BB : *this)
      BB.dropAllReferences();
  }

  /// Notify that this function was inlined. This implies that it is still
  /// needed for debug info generation, even if it is removed afterwards.
  void setInlined() {
    assert(!isZombie() && "Can't inline a zombie function");
    Inlined = true;
  }

  /// Returns true if this function was inlined.
  bool isInlined() const { return Inlined; }

  /// Mark this function as removed from the module's function list, but kept
  /// as "zombie" for debug info or vtable stub generation.
  void setZombie() {
    assert((isInlined() || isExternallyUsedSymbol())  &&
          "Function should be deleted instead of getting a zombie");
    Zombie = true;
  }
  
  /// Returns true if this function is dead, but kept in the module's zombie list.
  bool isZombie() const { return Zombie; }

  /// Returns the calling convention used by this entry point.
  AbstractCC getAbstractCC() const {
    return getLoweredFunctionType()->getAbstractCC();
  }

  /// Returns true if this function has a calling convention that has a self
  /// argument.
  bool hasSelfArgument() const {
    return getLoweredFunctionType()->hasSelfArgument();
  }

  StringRef getName() const { return Name; }

  /// True if this is a declaration of a function defined in another module.
  bool isExternalDeclaration() const { return BlockList.empty(); }
  bool isDefinition() const { return !isExternalDeclaration(); }

  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return SILLinkage(Linkage); }
  void setLinkage(SILLinkage linkage) { Linkage = unsigned(linkage); }

  /// Get's the effective linkage which is used to derive the llvm linkage.
  /// Usually this is the same as getLinkage(), except in one case: if this
  /// function is a method in a class which has higher visiblity than the
  /// method itself, the function can be referenced from vtables of derived
  /// classes in other compilation units.
  SILLinkage getEffectiveSymbolLinkage() const {
    SILLinkage L = getLinkage();
    switch (getClassVisibility()) {
      case NotRelevant:
        break;
      case InternalClass:
        if (L == SILLinkage::Private)
          return SILLinkage::Hidden;
        break;
      case PublicClass:
        if (L == SILLinkage::Private || L == SILLinkage::Hidden)
          return SILLinkage::Public;
        break;
    }
    return L;
  }
    
  /// Helper method which returns true if this function has "external" linkage.
  bool isAvailableExternally() const {
    return swift::isAvailableExternally(getLinkage());
  }

  /// Helper method which returns true if the linkage of the SILFunction
  /// indicates that the objects definition might be required outside the
  /// current SILModule.
  bool isPossiblyUsedExternally() const;

  /// In addition to isPossiblyUsedExternally() it returns also true if this
  /// is a (private or internal) vtable method which can be referenced by
  /// vtables of derived classes outside the compilation unit.
  bool isExternallyUsedSymbol() const;

  /// Get the DeclContext of this function. (Debug info only).
  DeclContext *getDeclContext() const { return DeclCtx; }
  void setDeclContext(Decl *D);
  void setDeclContext(Expr *E);

  /// \returns True if the function is marked with the @semantics attribute
  /// and has special semantics that the optimizer can use to optimize the
  /// function.
  bool hasDefinedSemantics() const {
    return SemanticsAttr.length() > 0;
  }

  /// \returns the semantics tag that describes this function.
  StringRef getSemanticsString() const {
    assert(hasDefinedSemantics() &&
           "Accessing a function with no semantics tag");
    return SemanticsAttr;
  }

  /// \returns True if the function has the semantics flag \p Value;
  bool hasSemanticsString(StringRef Value) const {
    return SemanticsAttr == Value;
  }

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

  /// Get this function's fragile attribute.
  IsFragile_t isFragile() const { return IsFragile_t(Fragile); }
  void setFragile(IsFragile_t isFrag) { Fragile = isFrag; }

  IsThunk_t isThunk() const { return IsThunk_t(Thunk); }
  void setThunk(IsThunk_t isThunk) { Thunk = isThunk; }

  /// Get the class visibility (relevant for class methods).
  ClassVisibility_t getClassVisibility() const {
    return ClassVisibility_t(ClassVisibility);
  }
    
  /// Get this function's noinline attribute.
  Inline_t getInlineStrategy() const { return Inline_t(InlineStrategy); }
  void setInlineStrategy(Inline_t inStr) { InlineStrategy = inStr; }

  /// \return the function side effects information.
  EffectsKind getEffectsKind() const { return EK; }

  /// \return True if the function is annotated with the @effects attribute.
  bool hasEffectsKind() const { return EK != EffectsKind::Unspecified; }

  /// \brief Set the function side effect information.
  void setEffectsKind(EffectsKind E) { EK = E; }

  /// Get this function's global_init attribute.
  ///
  /// The implied semantics are:
  /// - side-effects can occur any time before the first invocation.
  /// - all calls to the same global_init function have the same side-effects.
  /// - any operation that may observe the initializer's side-effects must be
  ///   preceded by a call to the initializer.
  ///
  /// This is currently true if the function is an addressor that was lazily
  /// generated from a global variable access. Note that the initialization
  /// function itself does not need this attribute. It is private and only
  /// called within the addressor.
  bool isGlobalInit() const { return GlobalInitFlag; }
  void setGlobalInit(bool isGI) { GlobalInitFlag = isGI; }

  StringRef getSemanticsAttr() const { return SemanticsAttr; }
  void setSemanticsAttr(StringRef attr) { SemanticsAttr = attr; }

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
  SILType mapTypeIntoContext(SILType type) const;

  /// Converts the given function if it is a definition to a deaclaration.
  void convertToDeclaration();

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

  SILBasicBlock *createBasicBlock();

  /// Splice the body of \p F into this function at end.
  void spliceBody(SILFunction *F) {
    getBlocks().splice(begin(), F->getBlocks());
  }

  /// Return the unique basic block containing a return inst if it
  /// exists. Otherwise, returns end.
  iterator findReturnBB() {
    return std::find_if(begin(), end(),
      [](const SILBasicBlock &BB) -> bool {
        const TermInst *TI = BB.getTerminator();
        return isa<ReturnInst>(TI);
    });
  }

  /// Return the unique basic block containing a return inst if it
  /// exists. Otherwise, returns end.
  const_iterator findReturnBB() const {
    return std::find_if(begin(), end(),
      [](const SILBasicBlock &BB) -> bool {
        const TermInst *TI = BB.getTerminator();
        return isa<ReturnInst>(TI);
    });
  }

  //===--------------------------------------------------------------------===//
  // Argument Helper Methods
  //===--------------------------------------------------------------------===//

  SILArgument *getArgument(unsigned i) {
    assert(!empty() && "Can not get argument of a function without a body");
    return begin()->getBBArg(i);
  }

  const SILArgument *getArgument(unsigned i) const {
    assert(!empty() && "Can not get argument of a function without a body");
    return begin()->getBBArg(i);
  }

  ArrayRef<SILArgument *> getArguments() const { return begin()->getBBArgs(); }

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

  /// This function is meant for use from the debugger.  You can just say 'call
  /// F->viewCFG()' and a ghostview window should pop up from the program,
  /// displaying the CFG of the current function with the code for each basic
  /// block inside.  This depends on there being a 'dot' and 'gv' program in
  /// your path.
  void viewCFG() const;

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
  static void deleteNode(SILFunction *V) { V->~SILFunction(); }

private:
  void createNode(const SILFunction &);
};

} // end llvm namespace

#endif
