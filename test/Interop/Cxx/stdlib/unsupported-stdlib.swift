// RUN: not %target-swift-frontend -typecheck -Xcc -stdlib=invalid -cxx-interoperability-mode=default %s 2>&1 | %FileCheck %s

// Windows does not honor `-stdlib=xyz` arguments, so the error message is different on Windows.
// XFAIL: OS=windows-msvc

// CHECK: error: invalid library name in argument '-stdlib=invalid'
