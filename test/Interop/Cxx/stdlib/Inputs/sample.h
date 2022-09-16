#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SAMPLE_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SAMPLE_H

#include <string>

#define SWIFT_CXX_REF_IMMORTAL                                                 \
	__attribute__((swift_attr("import_as_ref")))                                 \
  __attribute__((swift_attr("retain:immortal")))                               \
	__attribute__((swift_attr("release:immortal")))


struct SWIFT_CXX_REF_IMMORTAL Counter {
public:
typedef int64_t Value;

void operator+=(Value delta);
void operator++() { *this += 1; }

private:
std::string name;
};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SAMPLE_H