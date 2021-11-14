#ifndef EXPERIMENTAL_REGEX_BRIDGING
#define EXPERIMENTAL_REGEX_BRIDGING

#ifdef __cplusplus
extern "C" {
#endif

typedef const char *(* ParseRegexStrawperson)(const char *);

void Parser_registerParseRegexStrawperson(ParseRegexStrawperson fn);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // EXPERIMENTAL_REGEX_BRIDGING


//const char* experimental_regex_strawperson(const char *in);

