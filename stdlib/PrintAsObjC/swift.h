@import ObjectiveC;

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#if defined(__has_include)
# if __has_include(<uchar.h>)
#  include <uchar.h>
# elif __cplusplus < 201103L
typedef uint_least16_t char16_t;
typedef uint_least32_t char32_t;
# endif
#endif

#define SWIFT_METATYPE(X) Class
