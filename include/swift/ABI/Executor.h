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
class SerialExecutorWitnessTable;

/// An unmanaged reference to an executor.
///
/// This type corresponds to the type Optional<Builtin.Executor> in
/// Swift.  The representation of nil in Optional<Builtin.Executor>
/// aligns with what this type calls the generic executor, so the
/// notional subtype of this type which is never generic corresponds
/// to the type Builtin.Executor.
///
/// An executor reference is divided into two pieces:
///
/// - The identity, which is just a (potentially ObjC) object
///   reference; when this is null, the reference is generic.
///   Equality of executor references is based solely on equality
///   of identity.
///
/// - The implementation, which is an optional reference to a
///   witness table for the SerialExecutor protocol.  When this
///   is null, but the identity is non-null, the reference is to
///   a default actor.  The low bits of the implementation pointer
///   are reserved for the use of marking interesting properties
///   about the executor's implementation.  The runtime masks these
///   bits off before accessing the witness table, so setting them
///   in the future should back-deploy as long as the witness table
///   reference is still present.
class ExecutorRef {
  HeapObject *Identity; // Not necessarily Swift reference-countable
  uintptr_t Implementation;

  // We future-proof the ABI here by masking the low bits off the
  // implementation pointer before using it as a witness table.
  enum: uintptr_t {
    WitnessTableMask = ~uintptr_t(alignof(void*) - 1)
  };

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

  /// Given a pointer to a default actor, return an executor reference
  /// for it.
  static ExecutorRef forDefaultActor(DefaultActor *actor) {
    assert(actor);
    return ExecutorRef(actor, 0);
  }

  /// Given a pointer to a serial executor and its SerialExecutor
  /// conformance, return an executor reference for it.
  static ExecutorRef forOrdinary(HeapObject *identity,
                           const SerialExecutorWitnessTable *witnessTable) {
    assert(identity);
    assert(witnessTable);
    return ExecutorRef(identity, reinterpret_cast<uintptr_t>(witnessTable));
  }

  HeapObject *getIdentity() const {
    return Identity;
  }

  /// Is this the generic executor reference?
  bool isGeneric() const {
    return Identity == 0;
  }

  /// Is this a default-actor executor reference?
  bool isDefaultActor() const {
    return !isGeneric() && Implementation == 0;
  }
  DefaultActor *getDefaultActor() const {
    assert(isDefaultActor());
    return reinterpret_cast<DefaultActor*>(Identity);
  }

  const SerialExecutorWitnessTable *getSerialExecutorWitnessTable() const {
    assert(!isGeneric() && !isDefaultActor());
    auto table = Implementation & WitnessTableMask;
    return reinterpret_cast<const SerialExecutorWitnessTable*>(table);
  }

  /// Do we have to do any work to start running as the requested
  /// executor?
  bool mustSwitchToRun(ExecutorRef newExecutor) const {
    return Identity != newExecutor.Identity;
  }

  /// Is this executor the main executor?
  bool isMainExecutor() const;

  bool operator==(ExecutorRef other) const {
    return Identity == other.Identity;
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

using ThrowingTaskFutureWaitContinuationFunction =
  SWIFT_CC(swiftasync)
  void (SWIFT_ASYNC_CONTEXT AsyncContext *, SWIFT_CONTEXT void *);


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
