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
#include "llvm/Support/MemAlloc.h"

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
template <class Size_T>
void SmallVectorBase<Size_T>::grow_pod(void *FirstEl, size_t MinCapacity,
                                       size_t TSize) {
  // Ensure we can fit the new capacity.
  // This is only going to be applicable when the capacity is 32 bit.
  if (MinCapacity > SizeTypeMax())
    report_bad_alloc_error("SmallVector capacity overflow during allocation");

  // Ensure we can meet the guarantee of space for at least one more element.
  // The above check alone will not catch the case where grow is called with a
  // default MinCapacity of 0, but the current capacity cannot be increased.
  // This is only going to be applicable when the capacity is 32 bit.
  if (capacity() == SizeTypeMax())
    report_bad_alloc_error("SmallVector capacity unable to grow");

  // In theory 2*capacity can overflow if the capacity is 64 bit, but the
  // original capacity would never be large enough for this to be a problem.
  size_t NewCapacity = 2 * capacity() + 1; // Always grow.
  NewCapacity = std::min(std::max(NewCapacity, MinCapacity), SizeTypeMax());

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

template
#if !defined(_WIN32)
__attribute__((__weak__, __visibility__("hidden")))
#endif
void SmallVectorBase<uint32_t>::grow_pod(void *FirstEl, size_t MinCapacity,
                                         size_t TSize);

template
#if !defined(_WIN32)
__attribute__((__weak__, __visibility__("hidden")))
#endif
void SmallVectorBase<uint64_t>::grow_pod(void *FirstEl, size_t MinCapacity,
                                         size_t TSize);

// The same for allocate_buffer and deallocate_buffer, which are used by
// DenseMap and unique_function. This is a hack. This implementation is copied
// from llvm/lib/Support/MemAlloc.cpp and has to stay in sync with it.
LLVM_ATTRIBUTE_RETURNS_NONNULL LLVM_ATTRIBUTE_RETURNS_NOALIAS void *
llvm::allocate_buffer(size_t Size, size_t Alignment) {
  return ::operator new(Size
#ifdef __cpp_aligned_new
                        ,
                        std::align_val_t(Alignment)
#endif
  );
}

void llvm::deallocate_buffer(void *Ptr, size_t Size, size_t Alignment) {
  ::operator delete(Ptr
#ifdef __cpp_sized_deallocation
                    ,
                    Size
#endif
#ifdef __cpp_aligned_new
                    ,
                    std::align_val_t(Alignment)
#endif
  );
}

} // end namespace llvm
#endif // defined(swiftCore_EXPORTS)
