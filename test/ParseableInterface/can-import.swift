// RUN: %empty-directory(%t)
// RUN: echo 'public func externalFunc() {}' | %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t/Library.swiftinterface -
// RUN: %target-swift-frontend -typecheck %s -I %t

#if canImport(Library)
import Library
externalFunc()
#else
#error("unable to import Library from its parseable interface")
#endif

#if canImport(LibraryThatDoesNotExist)
#error("should not return true for library that does not exist")
#endif