//===-- StringHelpers.c - Optimized String helper routines ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains optimized helper routines for various String operations.
///
//===----------------------------------------------------------------------===//

#include <cassert>
#include "../SwiftShims/SwiftStddef.h"
#include "../SwiftShims/SwiftStdint.h"

using wide_t = __swift_uintptr_t;
using narrow_t = __swift_uint8_t;

union iterator_t {
  iterator_t(const void *v) : v(v) {}
  const wide_t   *w;
  const narrow_t *b;
  const void     *v;

  const __swift_uintptr_t i;
};

constexpr __swift_size_t wide_size = sizeof(wide_t);
constexpr __swift_size_t min_wide_len = wide_size * 2 - 1;
constexpr __swift_size_t wide_align_mask = wide_size - 1;

static_assert(sizeof(narrow_t) == 1, "Narrow type expected to be of size 1");

static
__swift_size_t _swift_string_memcmp_narrow(iterator_t it1,
                                           iterator_t it2,
                                           __swift_size_t n) {
  __swift_size_t bytes_left = n;
  while (bytes_left > 0) {
    if (*it1.b != *it2.b)
      break;
    ++it1.b;
    ++it2.b;
    --bytes_left;
  }
  return n - bytes_left;
}

static
__swift_size_t _swift_string_memcmp_wide(iterator_t it1,
                                         iterator_t it2,
                                         __swift_size_t n) {
  // See below for why we expect at least this many bytes.
  assert(n >= (wide_size - 1 + wide_size) && "Too few bytes to compare");
  __swift_size_t bytes_left = n;
  // Alignment loop:
  // Doesn't check bytes_left, assumes caller supplied >= (wide_size - 1) bytes.
  while ((it1.i & wide_align_mask) != 0) {
    if (*it1.b != *it2.b)
      goto matchfail;
    ++it1.b;
    ++it2.b;
    --bytes_left;
  }
  // Wide compare loop:
  // Does at least one iteration, assumes that we have >= wide_size bytes
  // remaining after the alignment loop.
  assert((it1.i & wide_align_mask) == 0 && "Expecting first buffer to be aligned");
  assert((it2.i & wide_align_mask) == 0 && "Expecting second buffer to be aligned");
  do {
    if (*it1.w != *it2.w)
      goto matchfail;
    ++it1.w;
    ++it2.w;
    bytes_left -= wide_size;
  } while (bytes_left >= wide_size);
  // Residue loop:
  while (bytes_left > 0) {
    if (*it1.b != *it2.b)
      break;
    ++it1.b;
    ++it2.b;
    --bytes_left;
  }
  return n - bytes_left;

matchfail:
  // Residue loop for mismatched buffers:
  // We know the buffers contain bytes that don't match, so
  // we don't have to care about checking bytes_left.
  while (*it1.b == *it2.b) {
    ++it1.b;
    ++it2.b;
    --bytes_left;
    assert(bytes_left > 0 && "Expecting a mismatch prior to the end of the buffer");
  }
  return n - bytes_left;
}

// Compares n bytes in s1 and s2, respectively, and returns the offset
// to the first differing byte, or n if s1 is identical to s2.
extern "C"
__swift_size_t _swift_string_memcmp(const void *s1,
                                    const void *s2,
                                    __swift_size_t n) {
  iterator_t it1(s1), it2(s2);
  // If we want to operate on naturally aligned data we need both inputs
  // to be aligned -- failing that we want them to at least be misaligned
  // to the same degree so we can compare bytes until they're aligned.
  return n >= min_wide_len &&
	 (it1.i & wide_align_mask) == (it2.i & wide_align_mask) ?
         _swift_string_memcmp_wide(it1, it2, n) :
         _swift_string_memcmp_narrow(it1, it2, n);
}
