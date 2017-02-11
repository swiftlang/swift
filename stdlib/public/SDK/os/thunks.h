//===----------------------------------------------------------------------===//
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

#ifndef __OS_THUNKS_H__
#define __OS_THUNKS_H__

#ifndef os_fastpath
#define os_fastpath(x) ((__typeof__(x))OS_EXPECT((long)(x), ~0l))
#endif
#ifndef os_slowpath
#define os_slowpath(x) ((__typeof__(x))OS_EXPECT((long)(x), 0l))
#endif
#ifndef os_likely
#define os_likely(x) OS_EXPECT(!!(x), 1)
#endif
#ifndef os_unlikely
#define os_unlikely(x) OS_EXPECT(!!(x), 0)
#endif

#ifndef MIN
#define MIN(a, b)  (((a)<(b))?(a):(b))
#endif

#ifndef MAX
#define MAX(a, b)  (((a)>(b))?(a):(b))
#endif

#ifndef OS_TRACE_INTERNAL_CRASH
#define OS_TRACE_INTERNAL_CRASH(_ptr, _message) _swift_os_log_reportError(0, _message)
#endif

#ifndef os_likely
#define os_likely(x) OS_EXPECT(!!(x), 1)
#endif
#ifndef os_unlikely
#define os_unlikely(x) OS_EXPECT(!!(x), 0)
#endif

#define os_add_overflow(a, b, res) __builtin_add_overflow((a), (b), (res))
#define os_sub_overflow(a, b, res) __builtin_sub_overflow((a), (b), (res))
#define os_mul_overflow(a, b, res) __builtin_mul_overflow((a), (b), (res))

__BEGIN_DECLS

__attribute__((__visibility__("hidden")))
void
_swift_os_log_reportError(uint32_t flags, const char *message);

__END_DECLS

#endif // __OS_THUNKS_H__
