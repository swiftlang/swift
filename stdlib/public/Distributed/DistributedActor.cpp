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
#if !SWIFT_DISTRIBUTED_EMBEDDED
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#endif

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

/// Find a accessor record for given identifier (remote call target identifier).
///
/// Returns the AccessibleFunctionRecord as an opaque pointer; the record is process-lifetime stable.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
const void *swift_distributed_getAccessibleRecord(const char *targetNameStart,
                                                  size_t targetNameLength) {
  return findDistributedAccessor(targetNameStart, targetNameLength);
}

#if !SWIFT_DISTRIBUTED_EMBEDDED

// Exported by the core runtime (MetadataLookup.cpp) without a header; the
// Swift side of this library declares them with @_silgen_name.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
unsigned swift_func_getParameterCount(const char *typeNameStart,
                                      size_t typeNameLength);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
const Metadata *_Nullable swift_func_getReturnTypeInfo(
    const char *typeNameStart, size_t typeNameLength,
    GenericEnvironmentDescriptor *genericEnv,
    const void *const *genericArguments);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_SPI
unsigned
swift_func_getParameterTypeInfo(const char *typeNameStart,
                                size_t typeNameLength,
                                GenericEnvironmentDescriptor *genericEnv,
                                const void *const *genericArguments,
                                Metadata const **types, unsigned typesLength);

namespace {

/// The part of a distributed target's signature that depends on the target
/// alone. For a generic target the types depend on the call's substitutions,
/// so `ParameterTypes` and `ReturnType` are null.
struct DistributedTargetSignature {
  /// Negative if the mangled name could not be resolved.
  intptr_t ParameterCount;
  const Metadata *const *ParameterTypes;
  const Metadata *ReturnType;
  const GenericEnvironmentDescriptor *GenericEnvironment;
};

/// The accessor record's address, hashed here since `llvm::hash_value` is not
/// available outside the core runtime.
struct DistributedTargetSignatureKey {
  const void *Record;

  friend llvm::hash_code hash_value(const DistributedTargetSignatureKey &key) {
    auto value = reinterpret_cast<uint64_t>(key.Record);
    return llvm::hash_code(
        static_cast<size_t>((value * 0x9E3779B97F4A7C15ULL) >> 32));
  }
};

struct DistributedTargetSignatureCacheEntry {
private:
  const void *Record;
  DistributedTargetSignature Signature;

public:
  DistributedTargetSignatureCacheEntry(
      DistributedTargetSignatureKey key,
      const DistributedTargetSignature &signature)
      : Record(key.Record), Signature(signature) {}

  const DistributedTargetSignature &getSignature() const { return Signature; }

  bool matchesKey(DistributedTargetSignatureKey key) const {
    return Record == key.Record;
  }

  friend llvm::hash_code
  hash_value(const DistributedTargetSignatureCacheEntry &entry) {
    return hash_value(DistributedTargetSignatureKey{entry.Record});
  }
};

/// Never invalidated: records, type metadata and generic environments live as
/// long as the process, and only names that resolved to a record get an entry.
static Lazy<ConcurrentReadableHashMap<DistributedTargetSignatureCacheEntry>>
    DistributedTargetSignatures;

static const DistributedTargetSignature UnresolvedDistributedTargetSignature = {
    -1, nullptr, nullptr, nullptr};

} // end anonymous namespace

/// Resolve the signature of the target behind an accessor record (as found by
/// `swift_distributed_getAccessibleRecord` for the same name) on first use and
/// return the cached signature from then on. A negative parameter count means
/// the name could not be resolved; the caller then reports the error as before.
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
DistributedTargetSignature
swift_distributed_getTargetSignature(const void *accessorRecord,
                                     const char *targetNameStart,
                                     size_t targetNameLength) {
  auto &signatures = DistributedTargetSignatures.get();
  DistributedTargetSignatureKey key{accessorRecord};
  {
    auto snapshot = signatures.snapshot();
    if (auto *entry = snapshot.find(key))
      return entry->getSignature();
  }

  auto *record =
      reinterpret_cast<const AccessibleFunctionRecord *>(accessorRecord);
  DistributedTargetSignature signature = {-1, nullptr, nullptr,
                                          record->GenericEnvironment.get()};

  unsigned parameterCount =
      swift_func_getParameterCount(targetNameStart, targetNameLength);
  if (static_cast<int>(parameterCount) < 0)
    return UnresolvedDistributedTargetSignature;
  signature.ParameterCount = parameterCount;

  const Metadata **parameterTypes = nullptr;
  if (!signature.GenericEnvironment) {
    // At least one slot, so that the accessor never gets a null pointer.
    parameterTypes = reinterpret_cast<const Metadata **>(
        malloc(std::max(parameterCount, 1u) * sizeof(const Metadata *)));
    unsigned decoded = swift_func_getParameterTypeInfo(
        targetNameStart, targetNameLength, /*genericEnv=*/nullptr,
        /*genericArguments=*/nullptr, parameterTypes, parameterCount);
    const Metadata *returnType =
        decoded == parameterCount
            ? swift_func_getReturnTypeInfo(targetNameStart, targetNameLength,
                                           /*genericEnv=*/nullptr,
                                           /*genericArguments=*/nullptr)
            : nullptr;
    if (!returnType) {
      free(parameterTypes);
      return UnresolvedDistributedTargetSignature;
    }
    signature.ParameterTypes = parameterTypes;
    signature.ReturnType = returnType;
  }

  signatures.getOrInsert(
      key, [&](DistributedTargetSignatureCacheEntry *entry, bool created) {
        if (created) {
          ::new (entry) DistributedTargetSignatureCacheEntry{key, signature};
        } else {
          // Another thread resolved the same target first; its entry wins.
          free(parameterTypes);
          signature = entry->getSignature();
        }
        return true;
      });
  return signature;
}

#endif // !SWIFT_DISTRIBUTED_EMBEDDED

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

/// Same as `TargetExecutorSignature` but with additional leading
/// `isolatedActor`/`isolatedActorWTable`, and with the accessor record found
/// by `swift_distributed_getAccessibleRecord` in place of the target name.
///
/// Introduced in SwiftStdlib 6.5.
using TargetExecutorWithIsolationSignature =
    AsyncSignature<void(/*on=*/DefaultActor *,
                        /*isolatedActor=*/HeapObject *,
                        /*isolatedActorWTable=*/void **,
                        /*accessorRecord=*/const void *,
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
TargetExecutorWithIsolationSignature::FunctionType
    swift_distributed_execute_target_with_isolation;

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

/// Same as `DistributedAccessorSignature`, but with additional
/// `isolatedActor`/`isolatedActorWTable` parameters right after `actor` --
/// the same actor, erased to `(any Actor)?`, nil when remote. Used by
/// accessors whose distributed thunk is `nonisolated(nonsending)`; the
/// accessor is `isolated` to this pair and hops to it after decoding the
/// arguments, right before calling the distributed thunk.
/// Both pointers null means "no isolation", matching the legacy
/// `DistributedAccessorSignature` behavior.
using DistributedAccessorWithIsolationSignature =
    AsyncSignature<void(/*argumentDecoder=*/HeapObject *,
                        /*argumentTypes=*/const Metadata *const *,
                        /*resultBuffer=*/void *,
                        /*substitutions=*/void *,
                        /*witnessTables=*/void **,
                        /*numWitnessTables=*/size_t,
                        /*actor=*/HeapObject *,
                        /*isolatedActor=*/HeapObject *,
                        /*isolatedActorWTable=*/void **,
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

SWIFT_CC(swiftasync)
static void swift_distributed_execute_target_with_isolation_resume(
    SWIFT_ASYNC_CONTEXT AsyncContext *context,
    SWIFT_CONTEXT SwiftError *error) {
  auto parentCtx = context->Parent;
  auto resumeInParent =
      function_cast<TargetExecutorWithIsolationSignature::ContinuationType *>(
          parentCtx->ResumeParent);
  swift_task_dealloc(context);
  return resumeInParent(parentCtx, error);
}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
SwiftError* swift_distributed_makeDistributedTargetAccessorNotFoundError();

/// Dispatch to the accessor.
///
/// If the accessor has a leading isolated `(any Actor)?` parameter pass it the `(isolatedActor, isolatedActorWTable)`.
/// The accessor decodes the arguments on the caller's executor, then hops to the requested isolation
/// and executes the target method there without any further executor switch.
///
/// Always inlined into the `swiftasync` entry points: the call into the
/// accessor has to be a tail call, which it only is once this is inlined.
SWIFT_ALWAYS_INLINE
static void dispatchToDistributedAccessor(
    AsyncContext *callerContext,
    DefaultActor *actor,
    HeapObject *isolatedActor,
    void **isolatedActorWTable,
    const AccessibleFunctionRecord *accessor,
    HeapObject *argumentDecoder,
    const Metadata *const *argumentTypes,
    void *resultBuffer,
    void *substitutions,
    void **witnessTables,
    size_t numWitnessTables,
    Metadata *decoderType,
    void **decoderWitnessTable,
    TaskContinuationFunction *resume) {
  if (!accessor) {
    SwiftError *error =
        swift_distributed_makeDistributedTargetAccessorNotFoundError();
    auto resumeInParent =
        function_cast<TargetExecutorSignature::ContinuationType *>(
            callerContext->ResumeParent);
    resumeInParent(callerContext, error);
    return;
  }

  AsyncContext *calleeContext;
  if (accessor->Flags.hasLeadingImplicitActorIsolationParameter()) {
    auto *asyncFnPtr = reinterpret_cast<const AsyncFunctionPointer<
        DistributedAccessorWithIsolationSignature> *>(accessor->Function.get());
    assert(asyncFnPtr && "no function pointer for distributed_execute_target");

    DistributedAccessorWithIsolationSignature::FunctionType *accessorEntry =
        asyncFnPtr->Function.get();

    calleeContext = reinterpret_cast<AsyncContext *>(
        swift_task_alloc(asyncFnPtr->ExpectedContextSize));
    calleeContext->Parent = callerContext;
    calleeContext->ResumeParent =
        function_cast<TaskContinuationFunction *>(resume);

    accessorEntry(calleeContext, argumentDecoder, argumentTypes, resultBuffer,
                  substitutions, witnessTables, numWitnessTables, actor,
                  isolatedActor, isolatedActorWTable, decoderType,
                  decoderWitnessTable);
    return;
  }

  auto *asyncFnPtr = reinterpret_cast<
      const AsyncFunctionPointer<DistributedAccessorSignature> *>(
      accessor->Function.get());
  assert(asyncFnPtr && "no function pointer for distributed_execute_target");

  DistributedAccessorSignature::FunctionType *accessorEntry =
      asyncFnPtr->Function.get();

  calleeContext = reinterpret_cast<AsyncContext *>(
      swift_task_alloc(asyncFnPtr->ExpectedContextSize));
  calleeContext->Parent = callerContext;
  calleeContext->ResumeParent =
      function_cast<TaskContinuationFunction *>(resume);

  accessorEntry(calleeContext, argumentDecoder, argumentTypes, resultBuffer,
                substitutions, witnessTables, numWitnessTables, actor,
                decoderType, decoderWitnessTable);
}

SWIFT_CC(swiftasync)
void swift_distributed_execute_target(
    SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    DefaultActor *actor,
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
  dispatchToDistributedAccessor(
      callerContext, actor,
      /*isolatedActor=*/nullptr, /*isolatedActorWTable=*/nullptr,
      findDistributedAccessor(targetNameStart, targetNameLength),
      argumentDecoder, argumentTypes, resultBuffer, substitutions,
      witnessTables, numWitnessTables, decoderType, decoderWitnessTable,
      function_cast<TaskContinuationFunction *>(
          &swift_distributed_execute_target_resume));
}

SWIFT_CC(swiftasync)
void swift_distributed_execute_target_with_isolation(
    SWIFT_ASYNC_CONTEXT AsyncContext *callerContext,
    DefaultActor *actor,
    HeapObject *isolatedActor,
    void **isolatedActorWTable,
    const void *accessorRecord,
    HeapObject *argumentDecoder,
    const Metadata *const *argumentTypes,
    void *resultBuffer,
    void *substitutions,
    void **witnessTables,
    size_t numWitnessTables,
    Metadata *decoderType,
    void **decoderWitnessTable
    ) {
  dispatchToDistributedAccessor(
      callerContext, actor,
      isolatedActor, isolatedActorWTable,
      reinterpret_cast<const AccessibleFunctionRecord *>(accessorRecord),
      argumentDecoder, argumentTypes, resultBuffer, substitutions,
      witnessTables, numWitnessTables, decoderType, decoderWitnessTable,
      function_cast<TaskContinuationFunction *>(
          &swift_distributed_execute_target_with_isolation_resume));
}
