//===--- Windows.h - Windows specifics --------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Windows specific includes and declarations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_WINDOWS_H
#define SWIFT_BACKTRACING_WINDOWS_H
#ifdef _WIN32

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// The Windows headers unfortunately don't define the various context types
// in a reasonable manner (they're all defined as CONTEXT, and they don't
// have other names).  Since we want all of them, we'll have to define our
// own variants here.
typedef struct {
  uint64_t Low;
  int64_t  High;
} WIN32_M128A;

typedef struct {
  uint16_t ControlWord;
  uint16_t StatusWord;
  uint8_t TagWord;
  uint8_t Reserved1;
  uint16_t ErrorOpcode;
  uint32_t ErrorOffset;
  uint16_t ErrorSelector;
  uint16_t Reserved2;
  uint32_t DataOffset;
  uint16_t DataSelector;
  uint16_t Reserved3;
  uint32_t MxCsr;
  uint32_t MxCsr_Mask;
  WIN32_M128A FloatRegisters[8];

  union {
    struct {
      WIN32_M128A XmmRegisters[16];
      uint8_t Reserved4[96];
    } AMD64;
    struct {
      WIN32_M128A XmmRegisters[8];
      uint8_t Reserved4[224];
    } I386;
  };
} WIN32_XSAVE_FORMAT;

typedef struct {
  uint64_t P1Home;
  uint64_t P2Home;
  uint64_t P3Home;
  uint64_t P4Home;
  uint64_t P5Home;
  uint64_t P6Home;

  uint32_t ContextFlags;
  uint32_t MxCsr;

  uint16_t SegCs;
  uint16_t SegDs;
  uint16_t SegEs;
  uint16_t SegFs;
  uint16_t SegGs;
  uint16_t SegSs;
  uint32_t EFlags;

  uint64_t Dr0;
  uint64_t Dr1;
  uint64_t Dr2;
  uint64_t Dr3;
  uint64_t Dr6;
  uint64_t Dr7;

  uint64_t Rax;
  uint64_t Rcx;
  uint64_t Rdx;
  uint64_t Rbx;
  uint64_t Rsp;
  uint64_t Rbp;
  uint64_t Rsi;
  uint64_t Rdi;
  uint64_t R8;
  uint64_t R9;
  uint64_t R10;
  uint64_t R11;
  uint64_t R12;
  uint64_t R13;
  uint64_t R14;
  uint64_t R15;

  uint64_t Rip;

  union {
    WIN32_XSAVE_FORMAT FltSave;
    struct {
      WIN32_M128A HEader[2];
      WIN32_M128A Legacy[8];
      WIN32_M128A Xmm0;
      WIN32_M128A Xmm1;
      WIN32_M128A Xmm2;
      WIN32_M128A Xmm3;
      WIN32_M128A Xmm4;
      WIN32_M128A Xmm5;
      WIN32_M128A Xmm6;
      WIN32_M128A Xmm7;
      WIN32_M128A Xmm8;
      WIN32_M128A Xmm9;
      WIN32_M128A Xmm10;
      WIN32_M128A Xmm11;
      WIN32_M128A Xmm12;
      WIN32_M128A Xmm13;
      WIN32_M128A Xmm14;
      WIN32_M128A Xmm15;
    };
  };

  WIN32_M128A VectorRegister[26];
  uint64_t VectorControl;

  uint64_t DebugControl;
  uint64_t LastBranchToRip;
  uint64_t LastBranchFromRip;
  uint64_t LastExceptionToRip;
  uint64_t LastExceptionFromRip;
} WIN32_AMD64_CONTEXT;

typedef struct {
  uint32_t ControlWord;
  uint32_t StatusWord;
  uint32_t TagWord;
  uint32_t ErrorOffset;
  uint32_t ErrorSelector;
  uint32_t DataOffset;
  uint32_t DataSelector;
  uint8_t  RegisterArea[80];
  uint32_t Spare0;
} WIN32_I386_FLOATING_SAVE_AREA;

typedef struct {
  uint32_t ContextFlags;

  uint32_t Dr0;
  uint32_t Dr1;
  uint32_t Dr2;
  uint32_t Dr3;
  uint32_t Dr6;
  uint32_t Dr7;

  WIN32_I386_FLOATING_SAVE_AREA FloatSave;

  uint32_t SegGs;
  uint32_t SegFs;
  uint32_t SegEs;
  uint32_t SegDs;

  uint32_t Edi;
  uint32_t Esi;
  uint32_t Ebx;
  uint32_t Edx;
  uint32_t Ecx;
  uint32_t Eax;

  uint32_t Ebp;
  uint32_t Eip;
  uint32_t SegCs;
  uint32_t EFlags;
  uint32_t Esp;
  uint32_t SegSs;

  uint8_t ExtendedRegisters[512];
} WIN32_I386_CONTEXT;

typedef union {
  struct {
    uint64_t Low;
    int64_t High;
  };
  double D[2];
  float S[4];
  uint16_t W[8];
  uint8_t B[16];
} WIN32_ARM64_NEON128;

typedef struct {
  uint32_t ContextFlags;
  uint32_t Cpsr;
  uint64_t X[31];
  uint64_t Sp;
  uint64_t Pc;

  WIN32_ARM64_NEON128 V[32];
  uint32_t Fpcr;
  uint32_t Fpsr;

  uint32_t Bcr[8];
  uint64_t Bvr[8];
  uint32_t Wcr[2];
  uint64_t Wvr[2];
} WIN32_ARM64_CONTEXT;

typedef struct {
  uint64_t Low;
  int64_t High;
} WIN32_NEON128;

typedef struct {
  uint32_t ContextFlags;

  uint32_t R0;
  uint32_t R1;
  uint32_t R2;
  uint32_t R3;
  uint32_t R4;
  uint32_t R5;
  uint32_t R6;
  uint32_t R7;
  uint32_t R8;
  uint32_t R9;
  uint32_t R10;
  uint32_t R11;
  uint32_t R12;

  uint32_t Sp;
  uint32_t Lr;
  uint32_t Pc;
  uint32_t Cpsr;

  uint32_t Fpscr;
  uint32_t Padding;
  union {
    WIN32_NEON128 Q[16];
    uint64_t D[32];
    uint32_t S[32];
  };

  uint32_t Bvr[8];
  uint32_t Bcr[8];
  uint32_t Wvr[1];
  uint32_t Wcr[1];

  uint32_t Padding2[2];
} WIN32_ARM_CONTEXT;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // _WIN32
#endif // SWIFT_BACKTRACING_WINDOWS_H

