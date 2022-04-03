// REQUIRES: OS=macosx
// RUN: %empty-directory(%t/inputs)
// RUN: %target-swift-frontend -typecheck %s -F %S/../ClangImporter/Inputs/frameworks -DFoo -emit-module-interface-path %t/inputs/Foo.swiftinterface -module-name Foo -enable-library-evolution -disable-clang-spi

// RUN: %target-swift-frontend -typecheck %s -F %S/../ClangImporter/Inputs/frameworks -enable-library-evolution -I %t/inputs -disable-clang-spi

// RUN: %target-swift-frontend -typecheck-module-from-interface %t/inputs/Foo.swiftinterface -F %S/../ClangImporter/Inputs/frameworks -disable-clang-spi


#if Foo

import SPIContainer

public func bar(_ c: SPIInterface1) {}

#else

import Foo

#endif
