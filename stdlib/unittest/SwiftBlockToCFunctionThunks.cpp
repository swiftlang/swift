//===--- ObjCBlockToFunctionAdapter.cpp -----------------------------------===//
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
///
/// \file This file implements C thunks that call ObjC blocks.  These thunks
/// can be used to implement block-based wrappers for APIs that accept a function
/// pointer and a context pointer.
///
//===----------------------------------------------------------------------===//

extern "C" void *swift_stdlib_getTypeMetadata(void *, void *T) {
  return T;
}

extern "C" void *swift_stdlib_executeSwiftClosure(
    void *Context, const void *ParamMetadata, const void *ResultMetadata);

template <typename Result, typename Param>
using FunctionPointerType = Result (*)(Param);

static void *executeBlock_VoidPtr_VoidPtr(void *Context) {
  const void **MetadataPointers = reinterpret_cast<const void **>(Context);
  return swift_stdlib_executeSwiftClosure(Context, MetadataPointers[0],
                                          MetadataPointers[1]);
}

/// Returns a thunk for a 'void *(*)(void *context)' C function.
extern "C" FunctionPointerType<void *, void *>
swift_stdlib_getExecuteBlockFunctionPtr_VoidPtr_VoidPtr() {
  return &executeBlock_VoidPtr_VoidPtr;
}

