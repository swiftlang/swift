//===--------- GenAxleMeta.h - Axle IR Metadata Generation ----------------===//
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
//  This file implements metadata generation for Axle.
//
//  Metadata consists of named nodes which list entry point functions of their
//  corresponding type.  The following named nodes are supported:
//  - air.kernel
//  - air.vertex
//  - air.fragment
//
//  Entry point metadata consists of a tuple containing the following:
//  - llvm::Function*
//  - Output metadata
//  - Argument metadata
//
//  Output and argument metadata lists the Axle variable attributes for
//  each value, and the index of that value in either the outputs or arguments.
//
//  For example, if the output is (pos [position] : Vec4f), then the metadata is
//  !{ i32 0, "air.position" }
//
//===----------------------------------------------------------------------===//

#include "GenAxleMeta.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILFunction.h"

using namespace axle;
using namespace swift;

GenAxleMeta::GenAxleMeta(llvm::Module& mod) : Module(mod) {
}

/// Creates LLVM IR metadata for a function.
void GenAxleMeta::createFunctionMetadata(SILFunction *SILFn,
                                         llvm::Function *IRFn) {
  llvm::LLVMContext &Context = Module.getContext();

  // First find the top level metadata to add this function to.
  const char* ShaderKindName;
  switch (SILFn->getKernelOrShaderKind()) {
    case KernelOrShaderKind::Default:
      return;
    case KernelOrShaderKind::Kernel:
      ShaderKindName = "air.kernel";
      break;
    case KernelOrShaderKind::Vertex:
      ShaderKindName = "air.vertex";
      break;
    case KernelOrShaderKind::Fragment:
      ShaderKindName = "air.fragment";
      break;
  }
  llvm::NamedMDNode *ShaderKindNode =
    Module.getOrInsertNamedMetadata(ShaderKindName);

  // Entry point function metadata nodes are:
  //   Function*
  //   metadata -> !{ outputs... }
  //   metadata -> !{ arguments... }
  llvm::SmallVector<llvm::Value*, 4> functionMDArgs;

  // Add the Function*
  functionMDArgs.push_back(IRFn);

  // Add output metadata
  // TODO: Fill this in once we have variable attributes.
  llvm::SmallVector<llvm::Value*, 4> outputMDArgs;
  functionMDArgs.push_back(llvm::MDNode::get(Context, outputMDArgs));

  // Add argument metadata
  // TODO: Fill this in once we have variable attributes.
  llvm::SmallVector<llvm::Value*, 4> argumentMDArgs;
  functionMDArgs.push_back(llvm::MDNode::get(Context, argumentMDArgs));

  ShaderKindNode->addOperand(llvm::MDNode::get(Context, functionMDArgs));
}
