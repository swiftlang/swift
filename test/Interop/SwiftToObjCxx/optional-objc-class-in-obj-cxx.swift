// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -module-name UseCoreFoundation -enable-experimental-cxx-interop -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/UseCoreFoundation.h
// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/UseCoreFoundation.h -o %t/o.o

// REQUIRES: objc_interop

import Foundation

public class TestFoundationType {
    private let _bundle: Bundle?

    public init(resourcesBundle bundle: Bundle? = nil) {
        _bundle = bundle
    }
}
