//===--- swift-elf-fuzzer.c - ELF image parser fuzzer ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This program fuzzes the ELF image parser in the Swift RuntimeModule.
// For this to work you need to pass --enable-sanitizer-coverage to build-script
// otherwise the fuzzer doesn't have coverage information to make progress
// (making the whole fuzzing operation really ineffective).
// It is recommended to use the tool together with another sanitizer to expose
// more bugs (asan, lsan, etc...)
//
//===----------------------------------------------------------------------===//

#include <stddef.h>
#include <stdint.h>

extern int32_t swift_fuzz_elf_image(const void *data, long size);

int LLVMFuzzerTestOneInput(const uint8_t *Data, size_t Size) {
  swift_fuzz_elf_image(Data, (long)Size);
  return 0;
}
