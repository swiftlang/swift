//===--- AsyncCall.h - Conveniences for doing async calls ----------*- C++ -*-//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Convenience functions for implementing Swift asynchronous functions
// in C++ code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_ASYNCCALL_H
#define SWIFT_CONCURRENCY_ASYNCCALL_H

#include "swift/Runtime/Concurrency.h"
#include "swift/ABI/Task.h"
#include <tuple>

namespace swift {
namespace {

/// Template-metaprogrammed basic layout for the given sequence of types.
template <size_t StartingOffset, class... FieldTys>
struct BasicLayout;
template <size_t StartingOffset>
struct BasicLayout<StartingOffset> {
  static constexpr size_t size = StartingOffset;
};
template <size_t StartingOffset, class HeadTy, class... TailTys>
struct BasicLayout<StartingOffset, HeadTy, TailTys...> {
  // Round up to a multiple of the alignment.
  static constexpr size_t fieldOffset =
    (StartingOffset + alignof(HeadTy) - 1) & ~(alignof(HeadTy) - 1);
  static constexpr size_t fieldEnd = fieldOffset + sizeof(HeadTy);
  using TailLayout = BasicLayout<fieldEnd, TailTys...>;
  static constexpr size_t size = TailLayout::size;
};

template <class Layout, size_t Index>
struct BasicLayoutOffset {
  static constexpr size_t value =
    BasicLayoutOffset<typename Layout::TailLayout, Index - 1>::value;
};
template <class Layout>
struct BasicLayoutOffset<Layout, 0> {
  static constexpr size_t value = Layout::fieldOffset;
};

/// Template-metaprogrammed layout for an async frame with the given
/// signature.  Works as long as you don't have a mix of indirect and
/// direct results; indirect results should be coded as initial
/// indirect arguments in the function signature.
///
/// Note that there's always a slot for an error result.
template <class Signature>
struct AsyncFrameLayout;

template <class... ArgTys, bool HasErrorResult>
struct AsyncFrameLayout<AsyncSignature<void(ArgTys...), HasErrorResult>> {
  using BasicLayout = BasicLayout<0, SwiftError **, ArgTys...>;
  static constexpr size_t firstArgIndex = 1;
};
template <class ResultTy, class... ArgTys, bool HasErrorResult>
struct AsyncFrameLayout<AsyncSignature<ResultTy(ArgTys...), HasErrorResult>> {
  using BasicLayout = BasicLayout<0, SwiftError **, ResultTy, ArgTys...>;
  static constexpr size_t firstArgIndex = 2;
};

/// A helper class which, when used as a base class under common
/// C++ ABIs, adds no extra size to a struct when the template
/// argument is 0.
template <size_t Size>
class AsyncFrameStorageHelper: public AsyncContext {
  // This needs to be aligned at least this much or else Itanium
  // will try to put it in the tail-padding of the AsyncContext.
  alignas(void*)
  char buffer[Size];
public:
  using AsyncContext::AsyncContext;
  char *data() { return buffer; }
};
template <>
class AsyncFrameStorageHelper<0>: public AsyncContext {
public:
  using AsyncContext::AsyncContext;
  char *data() { return reinterpret_cast<char*>(this); }
};

template <class CalleeSignature,
          class FrameLayout = AsyncFrameLayout<CalleeSignature>>
struct AsyncFrameStorage;
template <class ResultTy, class... ArgTys,
          bool HasErrorResult, class FrameLayout>
struct AsyncFrameStorage<AsyncSignature<ResultTy(ArgTys...),
                                        HasErrorResult>,
                         FrameLayout>
    : AsyncFrameStorageHelper<FrameLayout::BasicLayout::size> {

  AsyncFrameStorage(AsyncContextFlags flags,
                    TaskContinuationFunction *resumeFunction,
                    AsyncContext *resumeToContext,
                    ArgTys... args)
      : AsyncFrameStorageHelper<FrameLayout::BasicLayout::size>(
          flags, resumeFunction, resumeToContext) {
    initializeHelper<FrameLayout::firstArgIndex>(this->data(), args...);
  }

private:
  template <size_t NextArgIndex>
  void initializeHelper(char *buffer) {}

  template <size_t NextArgIndex, class NextArgTy, class... TailArgTys>
  void initializeHelper(char *buffer, NextArgTy nextArg,
                        TailArgTys... tailArgs) {
    auto offset = BasicLayoutOffset<typename FrameLayout::BasicLayout,
                                    NextArgIndex>::value;
    new (buffer + offset) NextArgTy(nextArg);
    initializeHelper<NextArgIndex+1>(buffer, tailArgs...);
  }
};


/// The context header for calling a function that takes the
/// given arguments.
template <class CallerContextType, class CalleeSignature>
struct AsyncCalleeContext : AsyncFrameStorage<CalleeSignature> {
  using CallerContext = CallerContextType;

  template <class... Args>
  AsyncCalleeContext(TaskContinuationFunction *resumeFunction,
                     CallerContext *resumeToContext,
                     Args... args)
    : AsyncFrameStorage<CalleeSignature>(AsyncContextKind::Ordinary,
                                         resumeFunction,
                                         resumeToContext, args...) {}

  CallerContext *getParent() const {
    return static_cast<CallerContext*>(this->Parent);
  }
};

/// Push a context to call a function.
template <class CalleeSignature, class CallerContext, class... Args>
static AsyncCalleeContext<CallerContext, CalleeSignature> *
pushAsyncContext(CallerContext *callerContext, size_t calleeContextSize,
                 TaskContinuationFunction *resumeFunction,
                 Args... args) {
  using CalleeContext =
    AsyncCalleeContext<CallerContext, CalleeSignature>;

  void *rawCalleeContext = swift_task_alloc(calleeContextSize);
  // We no longer store arguments in the context so we can just cast to an async
  // context.
  return reinterpret_cast<CalleeContext *>(new (rawCalleeContext) AsyncContext(
      AsyncContextKind::Ordinary, resumeFunction, callerContext));
}

/// Make an asynchronous call.
template <class CalleeSignature, class CallerContext, class... Args>
SWIFT_CC(swiftasync)
static void callAsync(CallerContext *callerContext,
                      TaskContinuationFunction *resumeFunction,
                const typename CalleeSignature::FunctionPointer *function,
                      Args... args) {
  auto calleeContextSize = function->ExpectedContextSize;
  auto calleeContext = pushAsyncContext(callerContext,
                                        calleeContextSize, resumeFunction,
                                        args...);
  return function->Function(calleeContext);
}

/// Given that that we've just entered the caller's continuation function
/// upon return from a function previously called with callAsync, pop the
/// callee's context and return the caller's context.
template <class CalleeContext>
static typename CalleeContext::CallerContext *
popAsyncContext(CalleeContext *calleeContext) {
  auto callerContext = calleeContext->getParent();
  swift_task_dealloc(calleeContext);
  return callerContext;
}

} // end anonymous namespace
} // end namespace swift

#endif
