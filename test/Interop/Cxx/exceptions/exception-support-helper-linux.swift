// RUN: %empty-directory(%t)
// RUN: %target-clangxx -x c++ -std=c++17 -fexceptions -pthread -I %swift_src_root/stdlib/public/Cxx/cxxshim %S/Inputs/exception-support-helper.h -o %t/helper
// RUN: %target-run %t/helper
// RUN: %target-clangxx -x c++ -std=c++17 -fexceptions -fno-rtti -pthread -O2 -I %swift_src_root/stdlib/public/Cxx/cxxshim %S/Inputs/exception-support-helper.h -o %t/helper-optimized
// RUN: %target-run %t/helper-optimized

// REQUIRES: executable_test
// REQUIRES: OS=linux-gnu

// POSIX thread cancellation and pthread_exit use forced unwinding on Linux.
// Neither may be swallowed by the C++ exception bridge.
