//===--- MD5Stream.h - raw_ostream that compute MD5  ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MD5STREAM_H
#define SWIFT_MD5STREAM_H

#include "llvm/Support/MD5.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

/// An output stream which calculates the MD5 hash of the streamed data.
class MD5Stream : public llvm::raw_ostream {
private:

  uint64_t Pos = 0;
  llvm::MD5 Hash;

  void write_impl(const char *Ptr, size_t Size) override {
    Hash.update(ArrayRef<uint8_t>(reinterpret_cast<const uint8_t *>(Ptr), Size));
    Pos += Size;
  }

  uint64_t current_pos() const override { return Pos; }

public:

  void final(llvm::MD5::MD5Result &Result) {
    flush();
    Hash.final(Result);
  }
};

} // namespace swift

#endif
