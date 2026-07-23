// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-clang-header-path %t/module-generated.h -typecheck %s
// RUN: %FileCheck %s -input-file %t/module-generated.h
// RUN: %check-in-clang-c %t/module-generated.h

// CHECK: #include <stdarg.h>
// CHECK: SWIFT_EXTERN void printem(va_list list) SWIFT_NOEXCEPT;
@c
public func printem(list: CVaListPointer) { }
