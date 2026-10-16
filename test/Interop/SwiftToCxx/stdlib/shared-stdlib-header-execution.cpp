// RUN: %empty-directory(%t)
// RUN: split-file %S/shared-stdlib-header.swift %t
// RUN: %empty-directory(%t/include/swift)
// RUN: %target-swift-frontend -parse-as-library %platform-module-dir/Swift.swiftmodule/%module-target-triple.swiftmodule -module-name Swift -parse-stdlib -typecheck -cxx-interoperability-mode=default -clang-header-expose-decls=has-expose-attr-or-stdlib -emit-clang-header-path %t/include/swift/Swift.h

// The standalone program uses the header built with the runtime and has no
// Swift source or generated user-module header.
// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/standalone.cpp -I %platform-module-dir/%target-cpu -o %t/standalone.o
// RUN: %target-build-swift %t/standalone.o -o %t/standalone -lswiftCore %target-cxx-lib
// RUN: %target-codesign %t/standalone
// RUN: %target-run %t/standalone

// RUN: %target-swift-frontend %t/Record.swift -module-name Record -parse-as-library -emit-object -o %t/Record.o -cxx-interoperability-mode=default -clang-header-expose-module Swift=swift/Swift.h -emit-clang-header-path %t/Record.h
// RUN: %target-swift-frontend %t/Text.swift -module-name Text -parse-as-library -emit-object -o %t/Text.o -cxx-interoperability-mode=default -clang-header-expose-module Swift=swift/Swift.h -emit-clang-header-path %t/Text.h

// RUN: %target-interop-build-clangxx -std=c++17 -fno-exceptions -c %t/mixed.cpp -I %t/include -I %t -DSWIFT_HEADER_FIRST -o %t/mixed-first.o
// RUN: %target-build-swift %t/mixed-first.o %t/Record.o %t/Text.o -o %t/mixed-first %target-cxx-lib
// RUN: %target-codesign %t/mixed-first
// RUN: %target-run %t/mixed-first

// RUN: %target-interop-build-clangxx -std=c++20 -O -c %t/mixed.cpp -I %t/include -I %t -o %t/mixed-last.o
// RUN: %target-build-swift %t/mixed-last.o %t/Record.o %t/Text.o -o %t/mixed-last %target-cxx-lib
// RUN: %target-codesign %t/mixed-last
// RUN: %target-run %t/mixed-last

// REQUIRES: executable_test
