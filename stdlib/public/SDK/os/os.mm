//===----------------------------------------------------------------------===//
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

#include <TargetConditionals.h>
#include <Availability.h>
#include <CoreFoundation/CoreFoundation.h>
#include <dlfcn.h>
#include <dispatch/dispatch.h>
#include <os/base.h>
#include <os/log.h>
#include <objc/runtime.h>
#include <wchar.h>

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

#define OST_FORMAT_MAX_STRING_SIZE 1024

#define OS_LOG_PRIVACY_OPTION_DEFAULT 0
#define OS_LOG_PRIVACY_OPTION_PRIVATE 1
#define OS_LOG_PRIVACY_OPTION_PUBLIC 2

enum os_trace_int_types_t {
	T_CHAR		= -2,
	T_SHORT		= -1,
	T_INT		=  0,
	T_LONG		=  1,
	T_LONGLONG	=  2,
	T_SIZE		=  3,
	T_INTMAX	=  4,
	T_PTRDIFF	=  5,
};

OS_ENUM(os_log_value_type, uint8_t,
	OS_LOG_BUFFER_VALUE_TYPE_SCALAR = 0,
	OS_LOG_BUFFER_VALUE_TYPE_COUNT = 1,
	OS_LOG_BUFFER_VALUE_TYPE_STRING = 2,
	OS_LOG_BUFFER_VALUE_TYPE_POINTER = 3,
	OS_LOG_BUFFER_VALUE_TYPE_OBJECT = 4,
);

OS_ENUM(os_log_value_subtype, uint8_t,
	OS_LOG_BUFFER_VALUE_SUBTYPE_NONE = 0,
	OS_LOG_BUFFER_VALUE_SUBTYPE_INTEGER = 1,
	OS_LOG_BUFFER_VALUE_SUBTYPE_FLOAT = 2,
);

enum os_log_int_types_t {
	OST_CHAR	  = -2,
	OST_SHORT	  = -1,
	OST_INT		  =  0,
	OST_LONG	  =  1,
	OST_LONGLONG  =  2,
	OST_SIZE	  =  3,
	OST_INTMAX	  =  4,
	OST_PTRDIFF   =  5,
};

union os_log_format_types_u {
	uint16_t	u16;
	uint32_t	u32;
	uint64_t	u64;
	char		ch;
	short		s;
	int			i;
	void		*p;
	char		*pch;
	wchar_t		wch;
	wchar_t		*pwch;
	size_t		z;
	intmax_t	im;
	ptrdiff_t	pd;
	long		l;
	long long	ll;
	double		d;
	float		f;
	long double ld;
};

typedef struct os_log_format_value_s {
	union os_log_format_types_u type;
	os_log_value_type_t ctype;
	uint16_t size;
} *os_log_format_value_t;

typedef struct os_log_buffer_value_s {
#define OS_LOG_CONTENT_FLAG_PRIVATE 0x1
#define OS_LOG_CONTENT_FLAG_PUBLIC 0x2
	uint8_t flags : 4;
	os_log_value_type_t type : 4;
	uint8_t size;
	uint8_t value[];
} *os_log_buffer_value_t;

typedef struct os_log_buffer_s {
#define OS_LOG_BUFFER_HAS_PRIVATE 0x1
#define OS_LOG_BUFFER_HAS_NON_SCALAR 0x2
#define OS_LOG_BUFFER_MAX_SIZE 1024
	uint8_t flags;
	uint8_t arg_cnt;
	uint8_t content[];
} *os_log_buffer_t;

typedef struct os_log_buffer_context_s {
	os_log_t log;
	os_log_buffer_t buffer;

	// sizes and offsets
	uint16_t content_off; // offset into buffer->content
	uint16_t content_sz; // size not including the header
	uint8_t arg_idx;
} *os_log_buffer_context_t;

static bool
_os_log_encode_arg(const void *arg, uint16_t arg_len, os_log_value_type_t ctype, uint8_t flags, os_log_buffer_context_t context)
{
	os_log_buffer_value_t content = (os_log_buffer_value_t) &context->buffer->content[context->content_off];
	size_t content_sz = sizeof(*content) + arg_len;

	content->type = ctype;
	content->flags = flags;

	switch (ctype) {
		case OS_LOG_BUFFER_VALUE_TYPE_COUNT:
		case OS_LOG_BUFFER_VALUE_TYPE_SCALAR:
			if ((context->content_off + content_sz) > context->content_sz) {
				return false;
			}

			memcpy(content->value, arg, arg_len);
			content->size = arg_len;
			context->content_off += content_sz;
			break;

		case OS_LOG_BUFFER_VALUE_TYPE_STRING:
		case OS_LOG_BUFFER_VALUE_TYPE_POINTER:
		case OS_LOG_BUFFER_VALUE_TYPE_OBJECT:
			memcpy(content->value, arg, arg_len);
			context->buffer->flags |= OS_LOG_BUFFER_HAS_NON_SCALAR;
			content->size = arg_len;
			context->content_off += content_sz;
			break;

	}

	if (content->flags & OS_LOG_CONTENT_FLAG_PRIVATE) {
		context->buffer->flags |= OS_LOG_BUFFER_HAS_PRIVATE;
	}

	context->arg_idx++;

	return true;
}

static bool
_os_log_encode(const char *format, va_list args, int saved_errno, os_log_buffer_context_t context)
{
	const char *percent = strchr(format, '%');

	while (percent != NULL) {
		++percent;
		if (percent[0] != '%') {
			struct os_log_format_value_s value;
			uint8_t	flags = 0;
			int		type = T_INT;
			bool	long_double = false;
			int		prec = 0;
			char	ch;

			for (bool done = false; !done; percent++) {
				switch (ch = percent[0]) {
						/* type of types or other */
					case 'l': // longer
						type++;
						break;

					case 'h': // shorter
						type--;
						break;

					case 'z':
						type = T_SIZE;
						break;

					case 'j':
						type = T_INTMAX;
						break;

					case 't':
						type = T_PTRDIFF;
						break;

					case '.': // precision
						if ((percent[1]) == '*') {
							prec = va_arg(args, int);
							_os_log_encode_arg(&prec, sizeof(prec), OS_LOG_BUFFER_VALUE_TYPE_COUNT, flags, context);
							percent++;
							continue;
						} else {
							// we have to read the precision and do the right thing
							const char *fmt = percent + 1;
							prec = 0;
							while (isdigit(ch = *fmt++)) {
								prec = 10 * prec + (ch - '0');
							}

							if (prec > 1024) {
								prec = 1024;
							}

							_os_log_encode_arg(&prec, sizeof(prec), OS_LOG_BUFFER_VALUE_TYPE_COUNT, flags, context);
						}
						break;

					case '-': // left-align
					case '+': // force sign
					case ' ': // prefix non-negative with space
					case '#': // alternate
					case '\'': // group by thousands
						break;

					case '{': // annotated symbols
						for (const char *curr2 = percent + 1; (ch = (*curr2)) != 0; curr2++) {
							if (ch == '}') {
								if (strncmp(percent + 1, "private", MIN(curr2 - percent - 1, 7)) == 0) {
									flags |= OS_LOG_CONTENT_FLAG_PRIVATE;
								}
								percent = curr2;
								break;
							}
						}
						break;

						/* fixed types */
					case 'd': // integer
					case 'i': // integer
					case 'o': // octal
					case 'u': // unsigned
					case 'x': // hex
					case 'X': // upper-hex
						switch (type) {
							case T_CHAR:
								value.type.ch = va_arg(args, int);
								_os_log_encode_arg(&value.type.ch, sizeof(value.type.ch), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_SHORT:
								value.type.s = va_arg(args, int);
								_os_log_encode_arg(&value.type.s, sizeof(value.type.s), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_INT:
								value.type.i = va_arg(args, int);
								_os_log_encode_arg(&value.type.i, sizeof(value.type.i), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_LONG:
								value.type.l = va_arg(args, long);
								_os_log_encode_arg(&value.type.l, sizeof(value.type.l), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_LONGLONG:
								value.type.ll = va_arg(args, long long);
								_os_log_encode_arg(&value.type.ll, sizeof(value.type.ll), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_SIZE:
								value.type.z = va_arg(args, size_t);
								_os_log_encode_arg(&value.type.z, sizeof(value.type.z), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_INTMAX:
								value.type.im = va_arg(args, intmax_t);
								_os_log_encode_arg(&value.type.im, sizeof(value.type.im), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							case T_PTRDIFF:
								value.type.pd = va_arg(args, ptrdiff_t);
								_os_log_encode_arg(&value.type.pd, sizeof(value.type.pd), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
								break;

							default:
								return false;
						}
						done = true;
						break;

					case 'P': // pointer data
						if (prec > 0) { // only encode a pointer if we have been given a length
							context->buffer->flags |= OS_LOG_BUFFER_HAS_NON_SCALAR;
							value.type.p = va_arg(args, void *);

							_os_log_encode_arg(value.type.p, prec, OS_LOG_BUFFER_VALUE_TYPE_POINTER, flags, context);
							prec = 0;
							done = true;
						}
						break;

					case 'L': // long double
						long_double = true;
						break;

					case 'a': case 'A': case 'e': case 'E': // floating types
					case 'f': case 'F': case 'g': case 'G':
						if (long_double) {
							value.type.ld = va_arg(args, long double);
							_os_log_encode_arg(&value.type.ld, sizeof(value.type.ld), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
						} else {
							value.type.d = va_arg(args, double);
							_os_log_encode_arg(&value.type.d, sizeof(value.type.d), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
						}
						done = true;
						break;

					case 'c': // char
						value.type.ch = va_arg(args, int);
						_os_log_encode_arg(&value.type.ch, sizeof(value.type.ch), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
						done = true;
						break;

					case 'C': // wide-char
						value.type.wch = va_arg(args, wint_t);
						_os_log_encode_arg(&value.type.wch, sizeof(value.type.wch), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
						done = true;
						break;

#if 0
					// String types get sent from Swift as NSString objects.
					case 's': // string
						value.type.pch = va_arg(args, char *);
						context->buffer->flags |= OS_LOG_BUFFER_HAS_NON_SCALAR;
						_os_log_encode_arg(&value.type.pch, sizeof(value.type.pch), OS_LOG_BUFFER_VALUE_TYPE_STRING, flags, context);
						prec = 0;
						done = true;
						break;
#endif

					case '@': // CFTypeRef aka NSObject *
						value.type.p = va_arg(args, void *);
						context->buffer->flags |= OS_LOG_BUFFER_HAS_NON_SCALAR;
						_os_log_encode_arg(&value.type.p, sizeof(value.type.p), OS_LOG_BUFFER_VALUE_TYPE_OBJECT, flags, context);
						done = true;
						break;

					case 'm':
						value.type.i = saved_errno;
						_os_log_encode_arg(&value.type.i, sizeof(value.type.i), OS_LOG_BUFFER_VALUE_TYPE_SCALAR, flags, context);
						done = true;
						break;

					default:
						if (isdigit(ch)) { // [0-9]
							continue;
						}
						return false;
				}

				if (done) {
					percent = strchr(percent, '%'); // Find next format
					break;
				}
			}
		} else {
			percent = strchr(percent+1, '%'); // Find next format after %%
		}
	}

	context->buffer->arg_cnt = context->arg_idx;
	context->content_sz = context->content_off;
	context->arg_idx = context->content_off = 0;

	return true;
}

#include "swift/Runtime/Config.h"

SWIFT_CC(swift) __attribute__((__visibility__("default")))
extern "C" void
_swift_os_log(void *dso, os_log_t oslog, os_log_type_t type, const char *format, va_list args)
{
	struct os_log_buffer_context_s context = { 0, 0, 0, 0, 0 };
	os_log_buffer_t buffer = (os_log_buffer_t)alloca(OS_LOG_BUFFER_MAX_SIZE);
	int save_errno = errno; // %m

	memset(buffer, 0, OS_LOG_BUFFER_MAX_SIZE);

	context.buffer = buffer;
	context.content_sz = OS_LOG_BUFFER_MAX_SIZE - sizeof(*buffer);

	if (_os_log_encode(format, args, save_errno, &context)) {
		_os_log_impl(dso, oslog, type, format, (uint8_t *)buffer, context.content_sz);
	}
}

SWIFT_CC(swift) __attribute__((__visibility__("default")))
extern "C" os_log_t
_swift_os_log_default(void) {
	return OS_LOG_DEFAULT;
}
