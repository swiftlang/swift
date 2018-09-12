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

// ADT uses report_bad_alloc_error to report an error when it can't allocate
// elements for a data structure. The swift runtime uses ADT without linking
// against libSupport, so here we provide a stub to make sure we don't fail
// to link.
#if defined(swiftCore_EXPORTS)
namespace llvm {
void __attribute__((__weak__, __visibility__("hidden")))
report_bad_alloc_error(const char *Reason, bool GenCrashDiag) {}
} // end namespace llvm
#endif

