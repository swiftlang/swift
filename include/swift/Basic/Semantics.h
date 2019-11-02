#ifndef SWIFT_SEMANTICS_H
#define SWIFT_SEMANTICS_H

#include "llvm/ADT/StringRef.h"

namespace swift {
#define SEMA_ATTR(NAME, C_STR) constexpr static const StringLiteral NAME = #C_STR;
#include "Semantics.def"
}

#endif
