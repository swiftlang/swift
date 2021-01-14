//===--- Executor.h - ABI structures for executors --------------*- C++ -*-===//
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
// Swift ABI describing executors.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ABI_EXECUTOR_H
#define SWIFT_ABI_EXECUTOR_H

#include <inttypes.h>
#include "swift/ABI/HeapObject.h"

namespace swift {
class AsyncContext;
class AsyncTask;
class DefaultActor;
class Job;

/// An ExecutorRef isn't necessarily just a pointer to an executor
/// object; it may have other bits set.
class ExecutorRef {
  static constexpr uintptr_t IsDefaultActor = 1;
  static constexpr uintptr_t PointerMask = 7;

  uintptr_t Value;

  constexpr ExecutorRef(uintptr_t value) : Value(value) {}

public:
  /// A generic execution environment.  When running in a generic
  /// environment, it's presumed to be okay to switch synchronously
  /// to an actor.  As an executor request, this represents a request
  /// to drop whatever the current actor is.
  constexpr static ExecutorRef generic() {
    return ExecutorRef(0);
  }

  /// Given a pointer to a default actor, return an executor reference
  /// for it.
  static ExecutorRef forDefaultActor(DefaultActor *actor) {
    assert(actor);
    return ExecutorRef(reinterpret_cast<uintptr_t>(actor) | IsDefaultActor);
  }

  /// Is this the generic executor reference?
  bool isGeneric() const {
    return Value == 0;
  }

  /// Is this a default-actor executor reference?
  bool isDefaultActor() const {
    return Value & IsDefaultActor;
  }
  DefaultActor *getDefaultActor() const {
    assert(isDefaultActor());
    return reinterpret_cast<DefaultActor*>(Value & ~PointerMask);
  }

  uintptr_t getRawValue() const {
    return Value;
  }

  /// Do we have to do any work to start running as the requested
  /// executor?
  bool mustSwitchToRun(ExecutorRef newExecutor) const {
    return *this != newExecutor;
  }

  bool operator==(ExecutorRef other) const {
    return Value == other.Value;
  }
  bool operator!=(ExecutorRef other) const {
    return Value != other.Value;
  }
};

using JobInvokeFunction =
  SWIFT_CC(swiftasync)
  void (Job *, ExecutorRef);

using TaskContinuationFunction =
  SWIFT_CC(swiftasync)
  void (AsyncTask *, ExecutorRef, AsyncContext *);

template <class AsyncSignature>
class AsyncFunctionPointer;
template <class AsyncSignature>
struct AsyncFunctionTypeImpl;

/// The abstract signature for an asynchronous function.
template <class Sig, bool HasErrorResult>
struct AsyncSignature;

template <class DirectResultTy, class... ArgTys, bool HasErrorResult>
struct AsyncSignature<DirectResultTy(ArgTys...), HasErrorResult> {
  bool hasDirectResult = !std::is_same<DirectResultTy, void>::value;
  using DirectResultType = DirectResultTy;

  bool hasErrorResult = HasErrorResult;

  using FunctionPointer = AsyncFunctionPointer<AsyncSignature>;
  using FunctionType = typename AsyncFunctionTypeImpl<AsyncSignature>::type;
};

/// A signature for a thin async function that takes no arguments
/// and returns no results.
using ThinNullaryAsyncSignature =
  AsyncSignature<void(), false>;

/// A signature for a thick async function that takes no formal
/// arguments and returns no results.
using ThickNullaryAsyncSignature =
  AsyncSignature<void(HeapObject*), false>;

/// A class which can be used to statically query whether a type
/// is a specialization of AsyncSignature.
template <class T>
struct IsAsyncSignature {
  static const bool value = false;
};
template <class DirectResultTy, class... ArgTys, bool HasErrorResult>
struct IsAsyncSignature<AsyncSignature<DirectResultTy(ArgTys...),
                                       HasErrorResult>> {
  static const bool value = true;
};

template <class Signature>
struct AsyncFunctionTypeImpl {
  static_assert(IsAsyncSignature<Signature>::value,
                "template argument is not an AsyncSignature");

  // TODO: expand and include the arguments in the parameters.
  using type = TaskContinuationFunction;
};

template <class Fn>
using AsyncFunctionType = typename AsyncFunctionTypeImpl<Fn>::type;

/// A "function pointer" for an async function.
///
/// Eventually, this will always be signed with the data key
/// using a type-specific discriminator.
template <class AsyncSignature>
class AsyncFunctionPointer {
public:
  /// The function to run.
  RelativeDirectPointer<AsyncFunctionType<AsyncSignature>,
                        /*nullable*/ false,
                        int32_t> Function;

  /// The expected size of the context.
  uint32_t ExpectedContextSize;
};

}

#endif
