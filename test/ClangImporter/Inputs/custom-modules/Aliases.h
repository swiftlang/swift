#pragma once

#if defined(UNICODE)
#define F FW
#define V VW
#else
#define F FA
#define V VA
#endif

#if defined(_WIN32)
#define ALIASES_ABI /**/
#else
#define ALIASES_ABI __attribute__((__visibility__("default")))
#endif

extern ALIASES_ABI const unsigned int VA;
extern ALIASES_ABI const unsigned long long VW;

ALIASES_ABI void FA(unsigned int);
ALIASES_ABI void FW(unsigned long long);

#define InvalidCall DoesNotExist

extern ALIASES_ABI float UIA;
extern ALIASES_ABI double UIW;

#if defined(UNICODE)
#define UI UIW
#else
#define UI UIA
#endif

enum {
  ALPHA = 0,
#define ALPHA ALPHA
  BETA = 1,
#define BETA BETA
};

enum {
  _CLOCK_MONOTONIC __attribute__((__swift_name__("CLOCK_MONOTONIC"))),
#define CLOCK_MONOTONIC _CLOCK_MONOTONIC
} _clock_t;

enum : int {
  overloaded,
};
#define overload overloaded
extern const int const_overloaded __attribute__((__swift_name__("overload")));

void variadic(int count, ...);
#define aliased_variadic variadic
