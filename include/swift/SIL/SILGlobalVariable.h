//===--- SILGlobalVariable.h - Defines SILGlobalVariable class --*- C++ -*-===//
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
// This file defines the SILGlobalVariable class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILGLOBALVARIABLE_H
#define SWIFT_SIL_SILGLOBALVARIABLE_H

#include "swift/SIL/SILFunction.h"

namespace swift {
  
/// A global variable that has been referenced in SIL.
class SILGlobalVariable
  : public llvm::ilist_node<SILGlobalVariable>,
    public SILAllocated<SILGlobalVariable>
{
private:
  friend class SILBasicBlock;
  friend class SILModule;

  /// ModuleAndLinkage - The SIL module that the function belongs to, and
  /// the function's linkage.
  llvm::PointerIntPair<SILModule*, 2, SILLinkage> ModuleAndLinkage;
  
  /// The mangled name of the variable, which will be propagated to the
  /// binary.
  std::string Name;

  /// The lowered type of the variable.
  SILType LoweredType;
  
  /// The SIL location of the variable, which provides a link back to the AST.
  /// The variable only gets a location after it's been emitted.
  Optional<SILLocation> Location;

  /// True if the storage for this global variable lies in this module. False
  /// if this is a reference to storage from a different module.
  bool IsDefinition;
  
public:
  SILGlobalVariable(SILModule &Module, SILLinkage Linkage,
                    StringRef MangledName, SILType LoweredType,
                    bool IsDefinition,
                    Optional<SILLocation> Loc = Nothing);

  ~SILGlobalVariable();

  SILModule &getModule() const { return *ModuleAndLinkage.getPointer(); }

  SILType getLoweredType() const { return LoweredType; }
  CanSILFunctionType getLoweredFunctionType() const {
    return LoweredType.castTo<SILFunctionType>();
  }
    
  StringRef getName() const { return Name; }
  void setName(StringRef N) {
    Name = N;
  }
  
  std::string &getMutableName() { return Name; }

  /// True if this is a declaration of a variable defined in another module.
  bool isExternalDeclaration() const { return !IsDefinition; }
  /// True if this is a definition of the variable.
  bool isDefinition() const { return IsDefinition; }
  void setDefinition(bool b) { IsDefinition = b; }
  
  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return ModuleAndLinkage.getInt(); }
  void setLinkage(SILLinkage L) { ModuleAndLinkage.setInt(L); }

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

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the variable follows
  /// invariants.
  void verify() const;
  
  /// Pretty-print the variable.
  void dump(bool Verbose) const;
  void dump() const { dump(false); }

  /// Pretty-print the variable to the designated stream as a 'sil_global'
  /// definition.
  ///
  /// \param Verbose In verbose mode, print the SIL locations.
  void print(raw_ostream &OS, bool Verbose = false) const;
  
  /// Pretty-print the variable name using SIL syntax,
  /// '@var_mangled_name'.
  void printName(raw_ostream &OS) const;
  
  ASTContext &getASTContext() const;
};
  
inline llvm::raw_ostream &operator<<(llvm::raw_ostream &OS,
                                     const SILGlobalVariable &F) {
  F.print(OS);
  return OS;
}

} // end swift namespace

//===----------------------------------------------------------------------===//
// ilist_traits for SILFunction
//===----------------------------------------------------------------------===//

namespace llvm {
  
template <>
struct ilist_traits<::swift::SILGlobalVariable> :
public ilist_default_traits<::swift::SILGlobalVariable> {
  typedef ::swift::SILGlobalVariable SILGlobalVariable;

private:
  mutable ilist_half_node<SILGlobalVariable> Sentinel;

public:
  SILGlobalVariable *createSentinel() const {
    return static_cast<SILGlobalVariable*>(&Sentinel);
  }
  void destroySentinel(SILGlobalVariable *) const {}

  SILGlobalVariable *provideInitialHead() const { return createSentinel(); }
  SILGlobalVariable *ensureHead(SILGlobalVariable*) const {
    return createSentinel();
  }
  static void noteHead(SILGlobalVariable*, SILGlobalVariable*) {}
  static void deleteNode(SILGlobalVariable *V) {}
  
private:
  void createNode(const SILGlobalVariable &);
};

} // end llvm namespace

#endif
