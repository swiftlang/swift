//===--- Fingerprint.cpp - A stable identity for compiler data --*- C++ -*-===//
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

#include "swift/Basic/Fingerprint.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

llvm::raw_ostream &llvm::operator<<(llvm::raw_ostream &OS,
                                    const Fingerprint &FP) {
  return OS << FP.getRawValue();
}

void swift::simple_display(llvm::raw_ostream &out, const Fingerprint &fp) {
  out << fp.getRawValue();
}

namespace {
  template <class T> struct SmallStringBound;
  template <size_t N> struct SmallStringBound<llvm::SmallString<N>> {
    static constexpr size_t value = N;
  };
};

// Assert that the \c DIGEST_LENGTH value we export from the \c Fingerprint
// has the right byte length. It's unlikely this value will change in LLVM,
// but it's always good to have compile-time justification for a
// magic constant - especially one that gets used for serialization.
using MD5Digest_t =
    decltype (&llvm::MD5::MD5Result::digest)(llvm::MD5::MD5Result);
static_assert(SmallStringBound<std::result_of<MD5Digest_t>::type>::value ==
                  Fingerprint::DIGEST_LENGTH,
              "MD5 digest size does not match size expected by Fingerprint!");
