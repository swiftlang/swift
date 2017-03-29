//===--- SILFunction.h - Defines the SILFunction class ----------*- C++ -*-===//
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
// This file defines the SILFunction class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILFUNCTION_H
#define SWIFT_SIL_SILFUNCTION_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILPrintContext.h"
#include "llvm/ADT/StringMap.h"

/// The symbol name used for the program entry point function.
/// FIXME: Hardcoding this is lame.
#define SWIFT_ENTRY_POINT_FUNCTION "main"

namespace swift {

class ASTContext;
class SILInstruction;
class SILModule;
namespace Lowering {
class TypeLowering;
} // namespace Lowering

enum IsBare_t { IsNotBare, IsBare };
enum IsTransparent_t { IsNotTransparent, IsTransparent };
enum Inline_t { InlineDefault, NoInline, AlwaysInline };
enum IsThunk_t { IsNotThunk, IsThunk, IsReabstractionThunk };

class SILSpecializeAttr final {
  friend SILFunction;
public:
  enum class SpecializationKind {
    Full,
    Partial
  };

  static SILSpecializeAttr *create(SILModule &M,
                                   ArrayRef<Requirement> requirements,
                                   bool exported, SpecializationKind kind);

  ArrayRef<Requirement> getRequirements() const;

  bool isExported() const {
    return exported;
  }

  bool isFullSpecialization() const {
    return kind == SpecializationKind::Full;
  }

  bool isPartialSpecialization() const {
    return kind == SpecializationKind::Partial;
  }

  SpecializationKind getSpecializationKind() const {
    return kind;
  }

  SILFunction *getFunction() const {
    return F;
  }

  void print(llvm::raw_ostream &OS) const;

private:
  unsigned numRequirements;
  SpecializationKind kind;
  bool exported;
  SILFunction *F;

  SILSpecializeAttr(ArrayRef<Requirement> requirements, bool exported,
                    SpecializationKind kind);

  Requirement *getRequirementsData() {
    return reinterpret_cast<Requirement *>(this+1);
  }
};

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
  GenericEnvironment *GenericEnv;

  /// The forwarding substitutions, lazily computed.
  Optional<SubstitutionList> ForwardingSubs;

  /// The collection of all BasicBlocks in the SILFunction. Empty for external
  /// function references.
  BlockListType BlockList;

  /// The owning declaration of this function's clang node, if applicable.
  ValueDecl *ClangNodeOwner = nullptr;

  /// The source location and scope of the function.
  const SILDebugScope *DebugScope;

  /// The function's bare attribute. Bare means that the function is SIL-only
  /// and does not require debug info.
  unsigned Bare : 1;

  /// The function's transparent attribute.
  unsigned Transparent : 1;

  /// The function's serialized attribute.
  unsigned Serialized : 2;

  /// Specifies if this function is a thunk or a reabstraction thunk.
  ///
  /// The inliner uses this information to avoid inlining (non-trivial)
  /// functions into the thunk.
  unsigned Thunk : 2;

  /// The visibility of the parent class, if this is a method which is contained
  /// in the vtable of that class.
  unsigned ClassVisibility : 2;
    
  /// The function's global_init attribute.
  unsigned GlobalInitFlag : 1;

  /// The function's noinline attribute.
  unsigned InlineStrategy : 2;

  /// The linkage of the function.
  unsigned Linkage : NumSILLinkageBits;

  /// This flag indicates if a function can be eliminated by dead function
  /// elimination. If it is unset, DFE will preserve the function and make
  /// it public.
  unsigned KeepAsPublic : 1;

  /// This is the number of uses of this SILFunction inside the SIL.
  /// It does not include references from debug scopes.
  unsigned RefCount = 0;

  /// The function's set of semantics attributes.
  ///
  /// TODO: Why is this using a std::string? Why don't we use uniqued
  /// StringRefs?
  llvm::SmallVector<std::string, 1> SemanticsAttrSet;

  /// The function's remaining set of specialize attributes.
  std::vector<SILSpecializeAttr*> SpecializeAttrSet;

  /// The function's effects attribute.
  EffectsKind EffectsKindAttr;

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

  /// True if SILOwnership is enabled for this function.
  ///
  /// This enables the verifier to easily prove that before the Ownership Model
  /// Eliminator runs on a function, we only see a non-semantic-arc world and
  /// after the pass runs, we only see a semantic-arc world.
  bool HasQualifiedOwnership = true;

  SILFunction(SILModule &module, SILLinkage linkage, StringRef mangledName,
              CanSILFunctionType loweredType, GenericEnvironment *genericEnv,
              Optional<SILLocation> loc, IsBare_t isBareSILFunction,
              IsTransparent_t isTrans, IsSerialized_t isSerialized,
              IsThunk_t isThunk, ClassVisibility_t classVisibility,
              Inline_t inlineStrategy, EffectsKind E,
              SILFunction *insertBefore,
              const SILDebugScope *debugScope);

  static SILFunction *
  create(SILModule &M, SILLinkage linkage, StringRef name,
         CanSILFunctionType loweredType, GenericEnvironment *genericEnv,
         Optional<SILLocation> loc, IsBare_t isBareSILFunction,
         IsTransparent_t isTrans, IsSerialized_t isSerialized,
         IsThunk_t isThunk = IsNotThunk,
         ClassVisibility_t classVisibility = NotRelevant,
         Inline_t inlineStrategy = InlineDefault,
         EffectsKind EffectsKindAttr = EffectsKind::Unspecified,
         SILFunction *InsertBefore = nullptr,
         const SILDebugScope *DebugScope = nullptr);

public:
  ~SILFunction();

  SILModule &getModule() const { return Module; }

  SILType getLoweredType() const {
    return SILType::getPrimitiveObjectType(LoweredType);
  }
  CanSILFunctionType getLoweredFunctionType() const {
    return LoweredType;
  }
  SILFunctionConventions getConventions() const {
    return SILFunctionConventions(LoweredType, getModule());
  }

  bool isNoReturnFunction() const;

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
    return !getRefCount() && !isZombie() && !isKeepAsPublic();
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

  /// Returns true if this function has qualified ownership instructions in it.
  bool hasQualifiedOwnership() const { return HasQualifiedOwnership; }

  /// Returns true if this function has unqualified ownership instructions in
  /// it.
  bool hasUnqualifiedOwnership() const { return !HasQualifiedOwnership; }

  /// Sets the HasQualifiedOwnership flag to false. This signals to SIL that no
  /// ownership instructions should be in this function any more.
  void setUnqualifiedOwnership() {
    HasQualifiedOwnership = false;
  }

  /// Returns the calling convention used by this entry point.
  SILFunctionTypeRepresentation getRepresentation() const {
    return getLoweredFunctionType()->getRepresentation();
  }

  /// Returns true if this function has a calling convention that has a self
  /// argument.
  bool hasSelfParam() const {
    return getLoweredFunctionType()->hasSelfParam();
  }

  /// Returns true if the function has parameters that are consumed by the
  // callee.
  bool hasOwnedParameters() const {
    for (auto &ParamInfo : getLoweredFunctionType()->getParameters()) {
      if (ParamInfo.isConsumed())
        return true;
    }
    return false;
  }

  // Returns true if the function has indirect out parameters.
  bool hasIndirectFormalResults() const {
    return getLoweredFunctionType()->hasIndirectFormalResults();
  }

  /// Returns true if this function either has a self metadata argument or
  /// object that Self metadata may be derived from.
  ///
  /// Note that this is not the same as hasSelfParam().
  ///
  /// For closures that capture DynamicSelfType, hasSelfMetadataParam()
  /// is true and hasSelfParam() is false. For methods on value types,
  /// hasSelfParam() is true and hasSelfMetadataParam() is false.
  bool hasSelfMetadataParam() const;

  /// Return the mangled name of this SILFunction.
  StringRef getName() const { return Name; }

  /// A convenience function which checks if the function has a specific
  /// \p name. It is equivalent to getName() == Name, but as it is not
  /// inlined it can be called from the debugger.
  bool hasName(const char *Name) const;

  /// True if this is a declaration of a function defined in another module.
  bool isExternalDeclaration() const { return BlockList.empty(); }

  /// Returns true if this is a definition of a function defined in this module.
  bool isDefinition() const { return !isExternalDeclaration(); }

  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return SILLinkage(Linkage); }

  /// Set the function's linkage attribute.
  void setLinkage(SILLinkage linkage) { Linkage = unsigned(linkage); }

  /// Returns true if this function can be inlined into a fragile function
  /// body.
  bool hasValidLinkageForFragileInline() const {
    return (isSerialized() == IsSerialized ||
            isSerialized() == IsSerializable);
  }

  /// Returns true if this function can be referenced from a fragile function
  /// body.
  bool hasValidLinkageForFragileRef() const;

  /// Get's the effective linkage which is used to derive the llvm linkage.
  /// Usually this is the same as getLinkage(), except in one case: if this
  /// function is a method in a class which has higher visibility than the
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
        if (L == SILLinkage::PrivateExternal || L == SILLinkage::HiddenExternal)
          return SILLinkage::PublicExternal;
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
  DeclContext *getDeclContext() const {
    return getLocation().getAsDeclContext();
  }

  /// \returns True if the function is marked with the @_semantics attribute
  /// and has special semantics that the optimizer can use to optimize the
  /// function.
  bool hasSemanticsAttrs() const { return SemanticsAttrSet.size() > 0; }

  /// \returns True if the function has a semantic attribute that starts with a
  /// specific string.
  ///
  /// TODO: This needs a better name.
  bool hasSemanticsAttrThatStartsWith(StringRef S) {
    return count_if(getSemanticsAttrs(), [&S](const std::string &Attr) -> bool {
      return StringRef(Attr).startswith(S);
    });
  }

  /// \returns the semantics tag that describes this function.
  ArrayRef<std::string> getSemanticsAttrs() const { return SemanticsAttrSet; }

  /// \returns True if the function has the semantics flag \p Value;
  bool hasSemanticsAttr(StringRef Value) const {
    return count(SemanticsAttrSet, Value);
  }

  /// Add the given semantics attribute to the attr list set.
  void addSemanticsAttr(StringRef Ref) {
    if (hasSemanticsAttr(Ref))
      return;
    SemanticsAttrSet.push_back(Ref);
    std::sort(SemanticsAttrSet.begin(), SemanticsAttrSet.end());
  }

  /// Remove the semantics
  void removeSemanticsAttr(StringRef Ref) {
    auto Iter =
        std::remove(SemanticsAttrSet.begin(), SemanticsAttrSet.end(), Ref);
    SemanticsAttrSet.erase(Iter);
  }

  /// \returns the range of specialize attributes.
  ArrayRef<SILSpecializeAttr*> getSpecializeAttrs() const {
    return SpecializeAttrSet;
  }

  /// Removes all specialize attributes from this function.
  void clearSpecializeAttrs() { SpecializeAttrSet.clear(); }

  void addSpecializeAttr(SILSpecializeAttr *Attr);

  /// \returns True if the function is optimizable (i.e. not marked as no-opt),
  ///          or is raw SIL (so that the mandatory passes still run).
  bool shouldOptimize() const;

  /// Check if the function has a location.
  /// FIXME: All functions should have locations, so this method should not be
  /// necessary.
  bool hasLocation() const {
    return DebugScope && !DebugScope->Loc.isNull();
  }

  /// Get the source location of the function.
  SILLocation getLocation() const {
    assert(DebugScope && "no scope/location");
    return getDebugScope()->Loc;
  }

  /// Initialize the debug scope of the function.
  void setDebugScope(const SILDebugScope *DS) { DebugScope = DS; }

  /// Get the source location of the function.
  const SILDebugScope *getDebugScope() const { return DebugScope; }

  /// Get this function's bare attribute.
  IsBare_t isBare() const { return IsBare_t(Bare); }
  void setBare(IsBare_t isB) { Bare = isB; }

  /// Get this function's transparent attribute.
  IsTransparent_t isTransparent() const { return IsTransparent_t(Transparent); }
  void setTransparent(IsTransparent_t isT) { Transparent = isT; }

  /// Get this function's serialized attribute.
  IsSerialized_t isSerialized() const { return IsSerialized_t(Serialized); }
  void setSerialized(IsSerialized_t isSerialized) { Serialized = isSerialized; }

  /// Get this function's thunk attribute.
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
  EffectsKind getEffectsKind() const { return EffectsKindAttr; }

  /// \return True if the function is annotated with the @effects attribute.
  bool hasEffectsKind() const {
    return EffectsKindAttr != EffectsKind::Unspecified;
  }

  /// \brief Set the function side effect information.
  void setEffectsKind(EffectsKind E) {
    EffectsKindAttr = E;
  }

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

  bool isKeepAsPublic() const { return KeepAsPublic; }
  void setKeepAsPublic(bool keep) { KeepAsPublic = keep; }

  /// Return whether this function has a foreign implementation which can
  /// be emitted on demand.
  bool hasForeignBody() const;

  /// Return whether this function corresponds to a Clang node.
  bool hasClangNode() const {
    return ClangNodeOwner != nullptr;
  }

  /// Set the owning declaration of the Clang node associated with this
  /// function.  We have to store an owner (a Swift declaration) instead of
  /// directly referencing the original declaration due to current
  /// limitations in the serializer.
  void setClangNodeOwner(ValueDecl *owner) {
    assert(owner->hasClangNode());
    ClangNodeOwner = owner;
  }

  /// Return the owning declaration of the Clang node associated with this
  /// function.  This should only be used for serialization.
  ValueDecl *getClangNodeOwner() const {
    return ClangNodeOwner;
  }

  /// Return the Clang node associated with this function if it has one.
  ClangNode getClangNode() const {
    return (ClangNodeOwner ? ClangNodeOwner->getClangNode() : ClangNode());
  }
  const clang::Decl *getClangDecl() const {
    return (ClangNodeOwner ? ClangNodeOwner->getClangDecl() : nullptr);
  }

  /// Retrieve the generic environment containing the mapping from interface
  /// types to context archetypes for this function. Only present if the
  /// function has a body.
  GenericEnvironment *getGenericEnvironment() const {
    return GenericEnv;
  }
  void setGenericEnvironment(GenericEnvironment *env) {
    GenericEnv = env;
  }

  /// Returns the type lowering for the \p Type given the generic signature of
  /// the current function.
  const Lowering::TypeLowering &getTypeLowering(SILType Type) const;

  /// Map the given type, which is based on an interface SILFunctionType and may
  /// therefore be dependent, to a type based on the context archetypes of this
  /// SILFunction.
  Type mapTypeIntoContext(Type type) const;

  /// Map the given type, which is based on an interface SILFunctionType and may
  /// therefore be dependent, to a type based on the context archetypes of this
  /// SILFunction.
  SILType mapTypeIntoContext(SILType type) const;

  /// Map the given type, which is based on a contextual SILFunctionType and may
  /// therefore contain context archetypes, to an interface type.
  Type mapTypeOutOfContext(Type type) const;

  /// Converts the given function definition to a declaration.
  void convertToDeclaration();

  /// Return the identity substitutions necessary to forward this call if it is
  /// generic.
  SubstitutionList getForwardingSubstitutions();

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

  SILBasicBlock *getEntryBlock() { return &front(); }
  const SILBasicBlock *getEntryBlock() const { return &front(); }

  SILBasicBlock *createBasicBlock();
  SILBasicBlock *createBasicBlock(SILBasicBlock *After);

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

  /// Return the unique basic block containing a throw inst if it
  /// exists. Otherwise, returns end.
  iterator findThrowBB() {
    return std::find_if(begin(), end(),
                        [](const SILBasicBlock &BB) -> bool {
                          const TermInst *TI = BB.getTerminator();
                          return isa<ThrowInst>(TI);
                        });
  }
  
  /// Return the unique basic block containing a throw inst if it
  /// exists. Otherwise, returns end.
  const_iterator findThrowBB() const {
    return std::find_if(begin(), end(),
                        [](const SILBasicBlock &BB) -> bool {
                          const TermInst *TI = BB.getTerminator();
                          return isa<ThrowInst>(TI);
                        });
  }
    
  //===--------------------------------------------------------------------===//
  // Argument Helper Methods
  //===--------------------------------------------------------------------===//

  SILArgument *getArgument(unsigned i) {
    assert(!empty() && "Cannot get argument of a function without a body");
    return begin()->getArgument(i);
  }

  const SILArgument *getArgument(unsigned i) const {
    assert(!empty() && "Cannot get argument of a function without a body");
    return begin()->getArgument(i);
  }

  ArrayRef<SILArgument *> getArguments() const {
    assert(!empty() && "Cannot get arguments of a function without a body");
    return begin()->getArguments();
  }

  ArrayRef<SILArgument *> getIndirectResults() const {
    assert(!empty() && "Cannot get arguments of a function without a body");
    return begin()->getArguments().slice(
        0, getConventions().getNumIndirectSILResults());
  }

  ArrayRef<SILArgument *> getArgumentsWithoutIndirectResults() const {
    assert(!empty() && "Cannot get arguments of a function without a body");
    return begin()->getArguments().slice(
        getConventions().getNumIndirectSILResults());
  }

  const SILArgument *getSelfArgument() const {
    assert(hasSelfParam() && "This method can only be called if the "
                             "SILFunction has a self parameter");
    return getArguments().back();
  }

  const SILArgument *getSelfMetadataArgument() const {
    assert(hasSelfMetadataParam() && "This method can only be called if the "
           "SILFunction has a self-metadata parameter");
    return getArguments().back();
  }

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the SILFunction follows
  /// invariants.
  void verify(bool SingleFunction = true) const;

  /// Pretty-print the SILFunction.
  void dump(bool Verbose) const;
  void dump() const;
  
  /// Pretty-print the SILFunction.
  /// Useful for dumping the function when running in a debugger.
  /// Warning: no error handling is done. Fails with an assert if the file
  /// cannot be opened.
  void dump(const char *FileName) const;

  /// Pretty-print the SILFunction to the tream \p OS.
  ///
  /// \param Verbose Dump SIL location information in verbose mode.
  void print(raw_ostream &OS, bool Verbose = false) const {
    SILPrintContext PrintCtx(OS, Verbose);
    print(PrintCtx);
  }

  /// Pretty-print the SILFunction with the context \p PrintCtx.
  void print(SILPrintContext &PrintCtx) const;

  /// Pretty-print the SILFunction's name using SIL syntax,
  /// '@function_mangled_name'.
  void printName(raw_ostream &OS) const;

  /// Assigns consecutive numbers to all SILValues in the function.
  void numberValues(llvm::DenseMap<const ValueBase*,
                    unsigned> &ValueToNumberMap) const;

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

public:
  static void deleteNode(SILFunction *V) { V->~SILFunction(); }

private:
  void createNode(const SILFunction &);
};

} // end llvm namespace

#endif
