// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop
// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop -Xcc -stdlib=libc++

import IncludeString
