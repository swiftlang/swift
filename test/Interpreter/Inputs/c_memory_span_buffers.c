//===-- c_memory_span_buffers.c - Helpers for Span @extern tests --*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// Allocation and fill helpers for `c_memory_span_views.swift`, which links this
/// translation unit with `%target-clang` and `%target-build-swift` using the
/// `Extern` feature.
///
//===----------------------------------------------------------------------===//

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define C_I32_PATTERN_COUNT 8
#define C_U8_PATTERN_LEN 16

int32_t *c_alloc_i32_pattern_buffer(void) {
  int32_t *p = malloc(sizeof(int32_t) * C_I32_PATTERN_COUNT);
  if (!p) {
    return NULL;
  }
  for (int i = 0; i < C_I32_PATTERN_COUNT; i++) {
    p[i] = i * 11;
  }
  return p;
}

void c_free_i32_buffer(int32_t *p) { free(p); }

int32_t c_i32_pattern_element_count(void) { return C_I32_PATTERN_COUNT; }

uint8_t *c_alloc_byte_pattern_buffer(void) {
  uint8_t *p = malloc(C_U8_PATTERN_LEN);
  if (!p) {
    return NULL;
  }
  for (int i = 0; i < C_U8_PATTERN_LEN; i++) {
    p[i] = (uint8_t)(0xA0 + (i % 16));
  }
  return p;
}

void c_free_byte_buffer(uint8_t *p) { free(p); }

int32_t c_byte_pattern_length(void) { return C_U8_PATTERN_LEN; }

void c_fill_incrementing_u8(uint8_t *bytes, size_t byteCount) {
  if (!bytes) {
    return;
  }
  for (size_t i = 0; i < byteCount; i++) {
    bytes[i] = (uint8_t)(i & 0xFF);
  }
}
