// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseFoundation -enable-experimental-cxx-interop -typecheck -verify -emit-clang-header-path %t/UseFoundation.h
// RUN: %FileCheck %s < %t/UseFoundation.h

// REQUIRES: objc_interop

import Foundation

public enum UseFoundationEnum {
    case A(Data)
    case B
}

// CHECK: class UseFoundationEnum { } SWIFT_UNAVAILABLE_MSG("Swift enum 'UseFoundationEnum' cannot be represented in C++");
