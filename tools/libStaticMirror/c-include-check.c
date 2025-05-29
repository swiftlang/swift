// This file serves as a soundness check to ensure that the header is parseable
// from C and that no C++ code has sneaked in.

#include "swift-c/StaticMirror/BinaryScan.h"
typedef swift_static_mirror_t _check_mirror_type_exists;
