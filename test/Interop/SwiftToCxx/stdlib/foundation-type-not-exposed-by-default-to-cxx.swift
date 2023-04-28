// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -typecheck -module-name UseFoundation -enable-experimental-cxx-interop -emit-clang-header-path %t/UseFoundation.h
// RUN: %FileCheck %s < %t/UseFoundation.h

#if canImport(Foundation)

import Foundation

public enum UseFoundationEnum {
    case A(Data)
    case B
}

#endif

// CHECK-NOT: UseFoundationEnum
