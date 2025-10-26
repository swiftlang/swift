//===----- GenCoro.h - Code generation related to coroutines --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

namespace swift {
namespace irgen {

class IRGenModule;

llvm::Constant *getCoroAllocFn(IRGenModule &IGM);
llvm::Constant *getCoroAllocFrameFn(IRGenModule &IGM);
llvm::Constant *getCoroDeallocFn(IRGenModule &IGM);
llvm::Constant *getCoroDeallocFrameFn(IRGenModule &IGM);

llvm::Value *
emitYieldOnce2CoroutineAllocator(IRGenFunction &IGF,
                                 std::optional<CoroAllocatorKind> kind);

} // end namespace irgen
} // end namespace swift
