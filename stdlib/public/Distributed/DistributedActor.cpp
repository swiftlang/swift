///===--- DistributedActor.cpp - Distributed actor implementation ----------===///
///
/// This source file is part of the Swift.org open source project
///
/// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
/// Licensed under Apache License v2.0 with Runtime Library Exception
///
/// See https:///swift.org/LICENSE.txt for license information
/// See https:///swift.org/CONTRIBUTORS.txt for the list of Swift project authors
///
///===----------------------------------------------------------------------===///
///
/// The implementation of Swift distributed actors.
///
///===----------------------------------------------------------------------===///

#include "swift/ABI/Actor.h"
#include "swift/ABI/Metadata.h"
#include "swift/ABI/Task.h"
#include "swift/Basic/Casting.h"
#include "swift/Runtime/AccessibleFunction.h"
#include "swift/Runtime/Concurrency.h"

using namespace swift;

static const AccessibleFunctionRecord *
findDistributedAccessor(const char *targetNameStart, size_t targetNameLength) {
  if (auto *func = runtime::swift_findAccessibleFunction(targetNameStart,
                                                         targetNameLength)) {
    assert(func->Flags.isDistributed());
    return func;
  }
  return nullptr;
}


SWIFT_CC(swift)
SWIFT_EXPORT_FROM(swiftDistributed)
void *swift_distributed_getGenericEnvironment(const char *targetNameStart,
                                              size_t targetNameLength) {
  auto *accessor = findDistributedAccessor(targetNameStart, targetNameLength);
  return accessor ? accessor->GenericEnvironment.get() : nullptr;
}

/// func _executeDistributedTarget<D: DistributedTargetInvocationDecoder>(
///    on: AnyObject,
///    _ targetName: UnsafePointer<UInt8>,
///    _ targetNameLength: UInt,
///    argumentDecoder: inout D,
///    argumentTypes: UnsafeBufferPointer<Any.Type>,
///    resultBuffer: Builtin.RawPointer,
///    substitutions: UnsafeRawPointer?,
///    witnessTables: UnsafeRawPointer?,
///    numWitnessTables: UInt
/// ) async throws
using TargetExecutorSignature =
    AsyncSignature<void(/*on=*/DefaultActor *,
                        /*targetName=*/const char *, /*targetNameSize=*/size_t,
                        /*argumentDecoder=*/HeapObject *,
                        /*argumentTypes=*/const Metadata *const *,
                        /*resultBuffer=*/void *,
                        /*substitutions=*/void *,
                        /*witnessTables=*/void **,
                        /*numWitnessTables=*/size_t,
                        /*decoderType=*/Metadata *,
                        /*decoderWitnessTable=*/void **),
                   /*throws=*/true>;

SWIFT_CC(swiftasync)
SWIFT_EXPORT_FROM(swiftDistributed)
TargetExecutorSignature::FunctionType swift_distributed_execute_target;

/// Accessor takes:
///   - an async context
///   - an argument decoder as an instance of type conforming to `InvocationDecoder`
///   - a list of all argument types (with substitutions applied)
///   - a result buffer as a raw pointer
///   - a list of substitutions
///   - a list of witness tables
///   - a number of witness tables in the buffer
///   - a reference to an actor to execute method on.
///   - a type of the argument decoder
///   - a witness table associated with argument decoder value
using DistributedAccessorSignature =
    AsyncSignature<void(/*argumentDecoder=*/HeapObject *,
                        /*argumentTypes=*/const Metadata *const *,
                        /*resultBuffer=*/void *,
                        /*substitutions=*/void *,
                        /*witnessTables=*/void **,
                        /*numWitnessTables=*/size_t,
                        /*actor=*/HeapObject *,
                        /*decoderType=*/Metadata *,
                        /*decoderWitnessTable=*/void **),
                   /*throws=*/true>;

SWIFT_CC(swiftasync)
static DistributedAccessorSignature::ContinuationType
    swift_distributed_execute_target_resume;

SWIFT_CC(swiftasync)
static void swift_distributed_execute_target_resume(
    SWIFT_ASYNC_CONTEXT AsyncContext *context,
    SWIFT_CONTEXT SwiftError *error) {
  auto parentCtx = context->Parent;
  auto resumeInParent =
      function_cast<TargetExecutorSignature::ContinuationType *>(
          parentCtx->ResumeParent);
  swift_task_dealloc(context);
  // See `swift_distributed_execute_target` - `parentCtx` in this case
  // is `callContext` which should be completely transparent on resume.
  return resumeInParent(parentCtx, error);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
SwiftError* swift_distributed_makeDistributedTargetAccessorNotFoundError();

SWIFT_CC(swiftasync)
void swift_distributed_execute_target(
    SWIFT_ASYNC_CONTEXT AsyncContext *callerContext, DefaultActor *actor,
    const char *targetNameStart, size_t targetNameLength,
    HeapObject *argumentDecoder,
    const Metadata *const *argumentTypes,
    void *resultBuffer,
    void *substitutions,
    void **witnessTables,
    size_t numWitnessTables,
    Metadata *decoderType,
    void **decoderWitnessTable
    ) {
  auto *accessor = findDistributedAccessor(targetNameStart, targetNameLength);
  if (!accessor) {
    SwiftError *error =
        swift_distributed_makeDistributedTargetAccessorNotFoundError();
    auto resumeInParent =
        function_cast<TargetExecutorSignature::ContinuationType *>(
            callerContext->ResumeParent);
    resumeInParent(callerContext, error);
    return;
  }

  auto *asyncFnPtr = reinterpret_cast<
      const AsyncFunctionPointer<DistributedAccessorSignature> *>(
      accessor->Function.get());
  assert(asyncFnPtr && "no function pointer for distributed_execute_target");

  DistributedAccessorSignature::FunctionType *accessorEntry =
      asyncFnPtr->Function.get();

  AsyncContext *calleeContext = reinterpret_cast<AsyncContext *>(
      swift_task_alloc(asyncFnPtr->ExpectedContextSize));

  calleeContext->Parent = callerContext;
  calleeContext->ResumeParent = function_cast<TaskContinuationFunction *>(
      &swift_distributed_execute_target_resume);

  accessorEntry(calleeContext, argumentDecoder, argumentTypes, resultBuffer,
                substitutions, witnessTables, numWitnessTables, actor,
                decoderType, decoderWitnessTable);
}
