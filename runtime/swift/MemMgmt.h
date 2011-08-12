/*
 * Copyright (c) 2010-2011 Apple Inc.
 * All rights reserved.
 */

#ifndef __BEDROCK_MEMMGMT__
#define __BEDROCK_MEMMGMT__

#include <sys/types.h>

#define BRALLOC_TRY 0x1
#define BRALLOC_RAW 0x2

void *
BRAllocFaster(size_t) throw();

void *
BRTryAllocFaster(size_t) throw();

void *
BRRawAllocFaster(size_t) throw();

void *
BRTryRawAllocFaster(size_t) throw();

void *
BRGenericAlloc(size_t, unsigned long flags) throw();

void
BRDeallocSlower(void *, size_t) throw();

void
BRDeallocFaster(void *, size_t) throw();

#define _BRAllocFasterSlower(sz, f, fl)	\
({	\
  __typeof__(sz) _sz = (sz);	\
 size_t _slot;	\
 void *_r;	\
 if (!__builtin_constant_p(sz)	\
     || (_sz > 0x1000)) {	\
 _r = BRGenericAlloc(_sz, fl);	\
 } else {	\
 _sz--;	\
 if (_sz == SIZE_MAX) {	\
 _slot = 0;	\
 } else if (_sz < 0x80) {	\
 _slot = (_sz >> 3);	\
 } else if (_sz < 0x100) {	\
 _slot = (_sz >> 4) + 0x8;	\
 } else if (_sz < 0x200) {	\
 _slot = (_sz >> 5) + 0x10;	\
 } else if (_sz < 0x400) {	\
 _slot = (_sz >> 6) + 0x18;	\
 } else if (_sz < 0x800) {	\
 _slot = (_sz >> 7) + 0x20;	\
 } else if (_sz < 0x1000) {	\
   _slot = (_sz >> 8) + 0x28;	\
 } else {	\
   __builtin_trap();	\
 }	\
   _r = f(_slot);	\
 }	\
 _r;	\
})

#define BRAllocSize(x) \
  ((__alignof__(x) > sizeof(x)) ? __alignof__(x) : sizeof(x))

#define BRAlloc(x) \
  _BRAllocFasterSlower(x, BRAllocFaster, 0)
#define BRTryAlloc(x) \
  _BRAllocFasterSlower(x, BRTryAllocFaster, BRALLOC_TRY)
#define BRRawAlloc(x) \
  _BRAllocFasterSlower(x, BRRawAllocFaster, BRALLOC_RAW)
#define BRTryRawAlloc(x) \
  _BRAllocFasterSlower(x, BRTryRawAllocFaster, BRALLOC_TRY|BRALLOC_RAW)


#define BRDealloc(p, sz)	\
({	\
 __typeof__(sz) _sz = (sz);	\
 size_t _slot;	\
 if (!__builtin_constant_p(sz)	\
     || (_sz > 0x1000)) {	\
 BRDeallocSlower(p, _sz);	\
 } else {	\
 _sz--;	\
 if (_sz == SIZE_MAX) {	\
 _slot = 0;	\
 } else if (_sz < 0x80) {	\
 _slot = (_sz >> 3);	\
 } else if (_sz < 0x100) {	\
 _slot = (_sz >> 4) + 0x8;	\
 } else if (_sz < 0x200) {	\
 _slot = (_sz >> 5) + 0x10;	\
 } else if (_sz < 0x400) {	\
 _slot = (_sz >> 6) + 0x18;	\
 } else if (_sz < 0x800) {	\
 _slot = (_sz >> 7) + 0x20;	\
 } else if (_sz < 0x1000) {	\
   _slot = (_sz >> 8) + 0x28;	\
 } else {	\
   __builtin_trap();	\
 }	\
   BRDeallocFaster(p, _slot);	\
 }	\
})

#endif
