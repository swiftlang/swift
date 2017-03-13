//===--- SILGlobalVariable.h - Defines SILGlobalVariable class --*- C++ -*-===//
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
// This file defines the SILGlobalVariable class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILGLOBALVARIABLE_H
#define SWIFT_SIL_SILGLOBALVARIABLE_H

#include <string>
#include "swift/SIL/SILLinkage.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILType.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace swift {

class ASTContext;
class SILFunction;
class SILInstruction;
class SILModule;
class VarDecl;
  
/// A global variable that has been referenced in SIL.
class SILGlobalVariable
  : public llvm::ilist_node<SILGlobalVariable>,
    public SILAllocated<SILGlobalVariable>
{
private:
  friend class SILBasicBlock;
  friend class SILModule;

  /// The SIL module that the global variable belongs to.
  SILModule &Module;
  
  /// The mangled name of the variable, which will be propagated to the
  /// binary.  A pointer into the module's lookup table.
  StringRef Name;

  /// The lowered type of the variable.
  SILType LoweredType;
  
  /// The SIL location of the variable, which provides a link back to the AST.
  /// The variable only gets a location after it's been emitted.
  Optional<SILLocation> Location;

  /// The linkage of the global variable.
  unsigned Linkage : NumSILLinkageBits;

  /// The global variable's fragile attribute.
  /// Fragile means that the variable can be "inlined" into another module.
  /// Currently this flag is set for all global variables in the stdlib.
  unsigned Fragile : 1;
  
  /// Whether this is a 'let' property, which can only be initialized
  /// once (either in its declaration, or once later), making it immutable.
  unsigned IsLet : 1;

  /// The VarDecl associated with this SILGlobalVariable. For debugger purpose.
  VarDecl *VDecl;

  /// Whether or not this is a declaration.
  bool IsDeclaration;

  /// The static initializer.
  SILFunction *InitializerF;

  SILGlobalVariable(SILModule &M, SILLinkage linkage, bool IsFragile,
                    StringRef mangledName, SILType loweredType,
                    Optional<SILLocation> loc, VarDecl *decl);
  
public:
  static SILGlobalVariable *create(SILModule &Module, SILLinkage Linkage,
                                   bool IsFragile,
                                   StringRef MangledName, SILType LoweredType,
                                   Optional<SILLocation> Loc = None,
                                   VarDecl *Decl = nullptr);

  ~SILGlobalVariable();

  SILModule &getModule() const { return Module; }

  SILType getLoweredType() const { return LoweredType; }
  CanSILFunctionType getLoweredFunctionType() const {
    return LoweredType.castTo<SILFunctionType>();
  }
    
  StringRef getName() const { return Name; }
  
  void setDeclaration(bool isD) { IsDeclaration = isD; }

  /// True if this is a definition of the variable.
  bool isDefinition() const { return !IsDeclaration; }

  /// Get this function's linkage attribute.
  SILLinkage getLinkage() const { return SILLinkage(Linkage); }
  void setLinkage(SILLinkage linkage) { Linkage = unsigned(linkage); }

  /// Get this global variable's fragile attribute.
  bool isFragile() const { return Fragile != 0; }
  void setFragile(bool isFrag) { Fragile = isFrag ? 1 : 0; }
  
  /// Is this an immutable 'let' property?
  bool isLet() const { return IsLet; }
  void setLet(bool isLet) { IsLet = isLet; }

  VarDecl *getDecl() const { return VDecl; }

  SILFunction *getInitializer() const { return InitializerF; }
  void setInitializer(SILFunction *InitF);

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

  // Helper functions to analyze the static initializer.
  static bool canBeStaticInitializer(SILFunction *F);
  /// Check if a given SILFunction can be a static initializer. If yes, return
  /// the SILGlobalVariable that it writes to.
  static SILGlobalVariable *getVariableOfStaticInitializer(SILFunction *F);
  /// Return the value that is written into the global variable.
  SILInstruction *getValueOfStaticInitializer();

  /// Return whether this variable corresponds to a Clang node.
  bool hasClangNode() const;

  /// Return the Clang node associated with this variable if it has one.
  ClangNode getClangNode() const;

  const clang::Decl *getClangDecl() const;

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the variable follows
  /// invariants.
  void verify() const;
  
  /// Pretty-print the variable.
  void dump(bool Verbose) const;
  void dump() const LLVM_ATTRIBUTE_USED { dump(false); }

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
// ilist_traits for SILGLobalVariable
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::SILGlobalVariable> :
public ilist_default_traits<::swift::SILGlobalVariable> {
  typedef ::swift::SILGlobalVariable SILGlobalVariable;

public:
  static void deleteNode(SILGlobalVariable *V) {}
  
private:
  void createNode(const SILGlobalVariable &);
};

} // end llvm namespace

#endif
