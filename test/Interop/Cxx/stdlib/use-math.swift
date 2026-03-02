// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

// This ensures that Swift can import C/C++ modules that contain `#include <math.h>`.

import IncludeMath

let x = getMyInt()
