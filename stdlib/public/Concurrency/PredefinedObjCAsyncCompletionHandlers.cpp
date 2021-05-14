//===--- PredefinedObjCAsyncCompletionHandlers.cpp - ObjC async handlers --===//
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
// Predefined ObjC async completion handlers.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"

#if SWIFT_OBJC_INTEROP

#include "swift/ABI/Task.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrency.h"
#include <dlfcn.h>

namespace swift {

/// The layout of the block that's passed to our completion handlers.
struct ContinuationBlockLayout {
  void *isa;
  volatile int32_t flags;
  int32_t reserved;
  void *invoke;
  struct Block_descriptor_1 *descriptor;
  AsyncTask *continuation;
};

/// Helper to get a typed result pointer from a continuation block.
template <class T>
T *getResultPtr(ContinuationBlockLayout *block) {
  auto *context = reinterpret_cast<ContinuationAsyncContext *>(
      block->continuation->ResumeContext);
  return reinterpret_cast<T *>(context->NormalResult);
}

// Bridging types and functions.

struct SwiftString {
#if __POINTER_WIDTH__ == 64
  uint64_t countAndFlags;
  void *object;
#else
  int32_t count;
  void *object;
  uint8_t objectDiscriminator;
  uint8_t discriminator;
  uint16_t flags;
#endif
};

typedef SWIFT_CC(swift) SwiftString (*BridgeNSStringToStringFunction)(void *);
typedef SWIFT_CC(swift) SwiftString *(*TakeStringWithInitFunction)(
    SwiftString *dest, SwiftString *src);
typedef SWIFT_CC(swift) SwiftError *(*ConvertNSErrorToErrorFunction)(void *);

static SwiftString BridgeNSStringToString(void *nsstring) {
  auto bridge = SWIFT_LAZY_CONSTANT(reinterpret_cast<
                                    BridgeNSStringToStringFunction>(dlsym(
      RTLD_DEFAULT,
      MANGLE_AS_STRING(MANGLE_SYM(
          SS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ)))));
  return bridge(nsstring);
}

static SwiftString *TakeStringWithInit(SwiftString *dest, SwiftString *src) {
  auto take = SWIFT_LAZY_CONSTANT(reinterpret_cast<TakeStringWithInitFunction>(
      dlsym(RTLD_DEFAULT, MANGLE_AS_STRING(MANGLE_SYM(SSSgWOb)))));
  return take(dest, src);
}

static SwiftError *ConvertNSErrorToError(void *nserror) {
  auto convert =
      SWIFT_LAZY_CONSTANT(reinterpret_cast<ConvertNSErrorToErrorFunction>(
          dlsym(RTLD_DEFAULT,
                MANGLE_AS_STRING(MANGLE_SYM(
                    10Foundation22_convertNSErrorToErrorys0E0_pSo0C0CSgF)))));
  return convert(nserror);
}

// Predefined handlers.

// (Int, Int, Int, Int, Int) -> Void
// Toy example, to be deleted.
SWIFT_EXPORT_FROM(swift_Concurrency)
void PredefinedFiveIntsHandler(ContinuationBlockLayout *block, long a, long b,
                               long c, long d, long e) {
  struct Result {
    long a, b, c, d, e;
  };
  auto *resultPtr = getResultPtr<Result>(block);

  resultPtr->a = a;
  resultPtr->b = b;
  resultPtr->c = c;
  resultPtr->d = d;
  resultPtr->e = e;

  swift_continuation_resume(block->continuation);
}

// (NSString?) -> Void
SWIFT_EXPORT_FROM(swift_Concurrency)
void PredefinedNSStringHandler(ContinuationBlockLayout *block, void *nsstring) {
  SwiftString result = BridgeNSStringToString(nsstring);
  TakeStringWithInit(getResultPtr<SwiftString>(block), &result);

  swift_continuation_resume(block->continuation);
}

// (NSString?, NSError?) -> Void
SWIFT_EXPORT_FROM(swift_Concurrency)
void PredefinedNSStringErrorHandler(ContinuationBlockLayout *block,
                                    void *nsstring, void *nserror) {
  if (nserror) {
    SwiftError *error = ConvertNSErrorToError(nserror);
    swift_continuation_throwingResumeWithError(block->continuation, error);
    return;
  }

  SwiftString result = BridgeNSStringToString(nsstring);
  TakeStringWithInit(getResultPtr<SwiftString>(block), &result);

  swift_continuation_throwingResume(block->continuation);
}

} // namespace swift

#endif // SWIFT_OBJC_INTEROP
