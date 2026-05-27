#ifndef TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_IMMORTAL_FRT_HASHABLE_H
#define TEST_INTEROP_CXX_FOREIGN_REFERENCE_INPUTS_IMMORTAL_FRT_HASHABLE_H

#include <stdint.h>

struct ImmortalFRT {
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")));

struct UnsafeFRT {
} __attribute__((swift_attr("import_reference")))
__attribute__((swift_attr("retain:immortal")))
__attribute__((swift_attr("release:immortal")))
__attribute__((swift_attr("unsafe")));

inline int64_t getHashValue(const struct ImmortalFRT &) { return 0; }
inline int64_t getHashValue(const struct UnsafeFRT &) { return 0; }

#endif
