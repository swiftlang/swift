//===--- LLVMSupport.cpp - Swift Language ABI Metadata Support ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Hashing.h"

// ADT uses report_bad_alloc_error to report an error when it can't allocate
// elements for a data structure. The swift runtime uses ADT without linking
// against libSupport, so here we provide a stub to make sure we don't fail
// to link.
#if defined(swiftCore_EXPORTS)
namespace llvm {

#if defined(_WIN32)
extern void report_bad_alloc_error(const char *Reason, bool GenCrashDiag);
void _report_bad_alloc_error(const char *Reason, bool GenCrashDiag) {}
#if defined(_WIN64)
#pragma comment(linker, "/alternatename:?report_bad_alloc_error@llvm@@YAXPEBD_N@Z=?_report_bad_alloc_error@llvm@@YAXPEBD_N@Z")
#else
#pragma comment(linker, "/alternatename:?report_bad_alloc_error@llvm@@YAXPBD_N@Z=?_report_bad_alloc_error@llvm@@YAXPBD_N@Z")
#endif
#else
void __attribute__((__weak__, __visibility__("hidden")))
report_bad_alloc_error(const char *Reason, bool GenCrashDiag) {}
#endif

// The same for SmallVector: provide the grow_pod implementation (the only
// SmallVector function which is not inlined) as we don't link LLVM.
// TODO: This is a hack. This implementaiton is copied from LLVM and has to stay
// in sync with it.

/// grow_pod - This is an implementation of the grow() method which only works
/// on POD-like datatypes and is out of line to reduce code duplication.
void
#if !defined(_WIN32)
__attribute__((__weak__, __visibility__("hidden")))
#endif
llvm::SmallVectorBase::grow_pod(void *FirstEl, size_t MinCapacity,
                                size_t TSize) {
  // Ensure we can fit the new capacity in 32 bits.
  if (MinCapacity > UINT32_MAX)
    report_bad_alloc_error("SmallVector capacity overflow during allocation");

  size_t NewCapacity = 2 * capacity() + 1; // Always grow.
  NewCapacity =
    std::min(std::max(NewCapacity, MinCapacity), size_t(UINT32_MAX));

  void *NewElts;
  if (BeginX == FirstEl) {
    NewElts = safe_malloc(NewCapacity * TSize);

    // Copy the elements over.  No need to run dtors on PODs.
    memcpy(NewElts, this->BeginX, size() * TSize);
  } else {
    // If this wasn't grown from the inline copy, grow the allocated space.
    NewElts = safe_realloc(this->BeginX, NewCapacity * TSize);
  }

  this->BeginX = NewElts;
  this->Capacity = NewCapacity;
}

namespace hashing { namespace detail {
  // An extern variable expected by LLVM's hashing templates. We don't link any
  // LLVM libs into the runtime, so define it as a weak symbol.
  //
  // Systems that compile this code into a dynamic library will do so with
  // hidden visibility, making this all internal to the dynamic library.
  // Systems that statically link the Swift runtime into applications (e.g. on
  // Linux) need this to handle the case when the app already uses LLVM.
  uint64_t LLVM_ATTRIBUTE_WEAK fixed_seed_override = 0;
} // namespace detail
} // namespace hashing

#if defined(_WIN32)
// Same thing for hash_code as it is required by DenseMap
extern hash_code hash_value(StringRef S);
hash_code _hash_value(StringRef S) {
  return hash_combine_range(S.begin(), S.end());
}
#pragma comment(linker, "/alternatename:?hash_value@llvm@@YA?AVhash_code@1@VStringRef@1@@Z=?_hash_value@llvm@@YA?AVhash_code@1@VStringRef@1@@Z")
#else
void __attribute__((__weak__, __visibility__("hidden")))
hash_code hash_value(StringRef S) {
  return hash_combine_range(S.begin(), S.end());
}
#endif

} // end namespace llvm
#endif // defined(swiftCore_EXPORTS)
