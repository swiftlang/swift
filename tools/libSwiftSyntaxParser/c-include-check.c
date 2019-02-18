// This file serves as a sanity check to ensure that the header is parseable
// from C and that no C++ code has sneaked in.

#include "swift-c/SyntaxParser/SwiftSyntaxParser.h"
typedef swiftparse_syntax_node_t _check_it_exists;
