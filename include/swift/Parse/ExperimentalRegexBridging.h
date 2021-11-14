#ifndef EXPERIMENTAL_REGEX_BRIDGING
#define EXPERIMENTAL_REGEX_BRIDGING

// Clang will be used to import, so we'll get the non-null declarations, but
// this allows the Swift parser in C++ to be built by compilers without
// nullability support.
//
// TODO: See if we can just include Visibility.h instead.
#ifndef _Nonnull
#define _Nonnull
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef const char *(* _Nonnull ParseRegexStrawperson)(const char * _Nonnull);

void Parser_registerParseRegexStrawperson(ParseRegexStrawperson fn);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // EXPERIMENTAL_REGEX_BRIDGING


//const char* experimental_regex_strawperson(const char *in);

