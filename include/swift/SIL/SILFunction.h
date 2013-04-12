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
#include "swift/SIL/SILModule.h"

namespace swift {

class ASTContext;
class FuncExpr;
class SILInstruction;
class TranslationUnit;
  
namespace Lowering {
  class SILGenModule;
}

/// SILFunction - A function body that has been lowered to SIL. This consists of
/// zero or more SIL SILBasicBlock objects that contain the SILInstruction
/// objects making up the function.
class SILFunction : public SILAllocated<SILFunction> {
public:
  typedef llvm::iplist<SILBasicBlock> BlockListType;

private:
  friend class SILBasicBlock;
  friend class SILModule;
  friend class Lowering::SILGenModule;

  /// Module - This is the SIL module that the function belongs to.
  SILModule &Module;

  /// MangledName - This is the mangled name of the SIL function, which will
  /// be propagated into LLVM IR.
  std::string MangledName;

  /// The lowered type of the function. This will differ from the type of the
  /// original function if the function consumes or returns address-only values.
  SILType LoweredType;
  
  /// The collection of all BasicBlocks in the SILFunction.
  BlockListType BlockList;

  // Intentionally marked private so that we need to use
  // 'SILModule::constructSIL()' to generate a SILFunction.
  SILFunction(SILModule &Module, SILType LoweredType)
    : Module(Module), LoweredType(LoweredType) {}
  
public:
  ~SILFunction();

  SILModule &getModule() const { return Module; }
  ASTContext &getContext() const { return Module.Context; }
  SILType getLoweredType() const { return LoweredType; }

  const std::string &getMangledName() const { return MangledName; }
  void setMangledName(const std::string &N) { MangledName = N; }


  //===--------------------------------------------------------------------===//
  // Block List Access
  //===--------------------------------------------------------------------===//

  BlockListType &getBlocks() { return BlockList; }
  const BlockListType &getBlocks() const { return BlockList; }

  typedef BlockListType::iterator iterator;
  typedef BlockListType::const_iterator const_iterator;

  bool empty() { return BlockList.empty(); }
  iterator begin() { return BlockList.begin(); }
  iterator end() { return BlockList.end(); }
  const_iterator begin() const { return BlockList.begin(); }
  const_iterator end() const { return BlockList.end(); }
  unsigned size() { return BlockList.size(); }

  SILBasicBlock &front() { return *begin(); }
  const SILBasicBlock &front() const { return *begin(); }

  //===--------------------------------------------------------------------===//
  // Miscellaneous
  //===--------------------------------------------------------------------===//

  /// verify - Run the IR verifier to make sure that the SILFunction follows
  /// invariants.
  void verify() const;
  
  /// Pretty-print the SILFunction.
  void dump() const;

  /// Pretty-print the SILFunction with the designated stream.
  void print(raw_ostream &OS) const;
  
  //===--------------------------------------------------------------------===//
  // Forwarding to the SILModule's allocator and type information
  //===--------------------------------------------------------------------===//
  void *allocate(unsigned Size, unsigned Align) const {
    return Module.allocate(Size, Align);
  }
  
  SILTypeList *getSILTypeList(llvm::ArrayRef<SILType> Types) const {
    return Module.getSILTypeList(Types);
  }

};

} // end swift namespace

#endif
