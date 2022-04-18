// REQUIRES: OS=macosx
// RUN: %empty-directory(%t/inputs)
// RUN: %target-swift-emit-module-interface(%t/inputs/Foo.swiftinterface) %s -module-name Foo -F %S/../ClangImporter/Inputs/frameworks -DFoo -disable-clang-spi -library-level api

// RUN: %target-swift-frontend -typecheck %s -F %S/../ClangImporter/Inputs/frameworks -enable-library-evolution -I %t/inputs -disable-clang-spi -library-level api

// RUN: %target-swift-typecheck-module-from-interface(%t/inputs/Foo.swiftinterface) -module-name Foo -F %S/../ClangImporter/Inputs/frameworks -disable-clang-spi -library-level api


#if Foo

import SPIContainer

public func bar(_ c: SPIInterface1) {}

#else

import Foo

#endif
