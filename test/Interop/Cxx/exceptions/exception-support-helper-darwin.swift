// RUN: %empty-directory(%t)
// RUN: %target-clangxx -x objective-c++ -std=c++17 -fobjc-arc -fexceptions -I %swift_src_root/stdlib/public/Cxx/cxxshim %S/Inputs/exception-support-helper.h -framework Foundation -o %t/helper
// RUN: %target-codesign %t/helper
// RUN: %target-run %t/helper
// RUN: %target-clangxx -x objective-c++ -std=c++17 -fobjc-arc -fexceptions -fno-rtti -O2 -I %swift_src_root/stdlib/public/Cxx/cxxshim %S/Inputs/exception-support-helper.h -framework Foundation -o %t/helper-optimized
// RUN: %target-codesign %t/helper-optimized
// RUN: %target-run %t/helper-optimized

// REQUIRES: executable_test
// REQUIRES: OS=macosx

// The helper is tested directly to verify that no foreign exception reaches
// Swift, including Objective-C exceptions that use the C++ exception ABI.
