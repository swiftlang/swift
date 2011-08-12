/*
 * Copyright (c) 2008-2011 Apple Inc. All rights reserved.
 */

/*
 * IMPORTANT: This header file describes INTERNAL interfaces to libbr
 * which are subject to change in future releases of Mac OS X. Any applications
 * relying on these interfaces WILL break.
 */

#ifndef __BR_SHIMS_TSD__
#define __BR_SHIMS_TSD__

#include <pthread.h>
#include <System/pthread_machdep.h>

#ifdef __arm__
#include <arm/arch.h>
#endif

#if 0
#define _PTHREAD_TSD_SLOT_PTHREAD_SELF                0
/* Keys 1- 9 for use by dyld, directly or indirectly */
#define _PTHREAD_TSD_SLOT_DYLD_1                1
#define _PTHREAD_TSD_SLOT_DYLD_2                2
#define _PTHREAD_TSD_SLOT_DYLD_3                3
#define _PTHREAD_TSD_RESERVED_SLOT_COUNT        4
/* Keys 10 - 29 are for Libc/Libsystem internal ussage */
/* used as __pthread_tsd_first + Num  */
#define __PTK_LIBC_LOCALE_KEY                10
#define __PTK_LIBC_TTYNAME_KEY                11
#define __PTK_LIBC_LOCALTIME_KEY        12
#define __PTK_LIBC_GMTIME_KEY                13
#define __PTK_LIBC_GDTOA_BIGINT_KEY        14
#define __PTK_LIBC_PARSEFLOAT_KEY        15
/* Keys 20-25 for libdispactch usage */
#define __PTK_LIBDISPATCH_KEY0                20
#define __PTK_LIBDISPATCH_KEY1                21
#define __PTK_LIBDISPATCH_KEY2                22
#define __PTK_LIBDISPATCH_KEY3                23
#define __PTK_LIBDISPATCH_KEY4                24
#define __PTK_LIBDISPATCH_KEY5                25
/* Keys 30-255 for Non Libsystem usage */
// ... last known key is 119
#endif

#ifndef	__PTK_LIBBR_KEY0
#define	__PTK_LIBBR_KEY0	120
#define	__PTK_LIBBR_KEY1	121
#define _br_alloc_key	122
#endif

static const unsigned long br_hwthread_backup_key = __PTK_LIBBR_KEY0;
static const unsigned long br_stack_key = __PTK_LIBBR_KEY1;

#ifndef _PTHREAD_TSD_OFFSET
#define _PTHREAD_TSD_OFFSET 0
#endif

static inline void
_br_thread_setspecific(unsigned long k, void *v)
{
#if defined(__i386__)
  asm(
    "movl %1, %%gs:%0"
    : "=m" (*(void **)(k * sizeof(void *) + _PTHREAD_TSD_OFFSET))
    : "ri" (v)
    : "memory"
  );
#elif defined(__x86_64__)
  asm(
    "movq %1, %%gs:%0"
    : "=m" (*(void **)(k * sizeof(void *) + _PTHREAD_TSD_OFFSET))
    : "rn" (v)
    : "memory"
  );
#elif defined(__arm__) && defined(_ARM_ARCH_6)
  void **__pthread_tsd;
  asm("mrc p15, 0, %0, c13, c0, 3" : "=r" (__pthread_tsd));
  __pthread_tsd[k + (_PTHREAD_TSD_OFFSET / sizeof(void *))] = v;
#else
  int res;
  if (_pthread_has_direct_tsd()) {
    res = _pthread_setspecific_direct(k, v);
  } else {
    res = pthread_setspecific(k, v);
  }
  //br_assert_zero(res);
#endif
}

static inline void *
_br_thread_getspecific(unsigned long k)
{
#if defined(__i386__) || defined(__x86_64__)
  void *rval;
  asm(
    "mov %%gs:%1, %0" : "=r" (rval)
    : "m" (*(void **)(k * sizeof(void *) + _PTHREAD_TSD_OFFSET))
  );
  return rval;
#elif defined(__arm__) && defined(_ARM_ARCH_6)
  void **__pthread_tsd;
  asm(
    "mrc p15, 0, %0, c13, c0, 3"
    : "=r" (__pthread_tsd)
  );
  return __pthread_tsd[k + (_PTHREAD_TSD_OFFSET / sizeof(void *))];
#else
  if (_pthread_has_direct_tsd()) {
    return _pthread_getspecific_direct(k);
  } else {
    return pthread_getspecific(k);
  }
#endif
}

static inline void
_br_thread_key_init_np(unsigned long k, void (*d)(void *))
{
  //br_assert_zero(pthread_key_init_np((int)k, d));
  pthread_key_init_np((int)k, d);
}

#endif
