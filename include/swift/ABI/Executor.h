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
#include "swift/ABI/Actor.h"
#include "swift/ABI/HeapObject.h"
#include "swift/Runtime/Casting.h"

namespace swift {
class AsyncContext;
class AsyncTask;
class DefaultActor;
class Job;

/// FIXME: only exists for the quick-and-dirty MainActor implementation.
SWIFT_EXPORT_FROM(swift_Concurrency)
Metadata* MainActorMetadata;

/// An unmanaged reference to an executor.
///
/// The representation is two words: identity and implementation.
/// The identity word is a reference to the executor object; for
/// default actors, this is the actor object.  The implementation
/// word describes how the executor works; it carries a witness table
/// as well as a small number of bits indicating various special
/// implementation properties.  As an exception to both of these
/// rules, a null identity represents a generic executor and
/// implies a null implementation word.
class ExecutorRef {
  HeapObject *Identity; // Not necessarily Swift reference-countable
  uintptr_t Implementation;

  constexpr ExecutorRef(HeapObject *identity, uintptr_t implementation)
    : Identity(identity), Implementation(implementation) {}

public:
  /// A generic execution environment.  When running in a generic
  /// environment, it's presumed to be okay to switch synchronously
  /// to an actor.  As an executor request, this represents a request
  /// to drop whatever the current actor is.
  constexpr static ExecutorRef generic() {
    return ExecutorRef(nullptr, 0);
  }

  /// FIXME: only exists for the quick-and-dirty MainActor implementation.
  /// NOTE: I didn't go with Executor::forMainActor(DefaultActor*) because
  /// __swift_run_job_main_executor can't take more than one argument.
  static ExecutorRef mainExecutor() {
    auto identity = getMainActorIdentity();
    return ExecutorRef(identity, 0);
  }
  static HeapObject *getMainActorIdentity() {
    return reinterpret_cast<HeapObject*>(
                                   ExecutorRefFlags::MainActorIdentity);
  }

  /// Given a pointer to a default actor, return an executor reference
  /// for it.
  static ExecutorRef forDefaultActor(DefaultActor *actor) {
    assert(actor);
    return ExecutorRef(actor, unsigned(ExecutorRefFlags::DefaultActor));
  }

  HeapObject *getIdentity() const {
    return Identity;
  }

  /// Is this the generic executor reference?
  bool isGeneric() const {
    return Identity == 0;
  }

  /// FIXME: only exists for the quick-and-dirty MainActor implementation.
  bool isMainExecutor() const {
    if (Identity == getMainActorIdentity())
      return true;

    if (Identity == nullptr || MainActorMetadata == nullptr)
      return false;

    Metadata const* metadata = swift_getObjectType(Identity);
    return metadata == MainActorMetadata;
  }

  /// Is this a default-actor executor reference?
  bool isDefaultActor() const {
    return Implementation & unsigned(ExecutorRefFlags::DefaultActor);
  }
  DefaultActor *getDefaultActor() const {
    assert(isDefaultActor());
    return reinterpret_cast<DefaultActor*>(Identity);
  }

  /// Do we have to do any work to start running as the requested
  /// executor?
  bool mustSwitchToRun(ExecutorRef newExecutor) const {
    return Identity != newExecutor.Identity;
  }

  bool operator==(ExecutorRef other) const {
    return Identity == other.Identity
    /// FIXME: only exists for the quick-and-dirty MainActor implementation.
          || (isMainExecutor() && other.isMainExecutor());
  }
  bool operator!=(ExecutorRef other) const {
    return !(*this == other);
  }
};

using JobInvokeFunction =
  SWIFT_CC(swiftasync)
  void (Job *);

using TaskContinuationFunction =
  SWIFT_CC(swiftasync)
  void (SWIFT_ASYNC_CONTEXT AsyncContext *);

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
