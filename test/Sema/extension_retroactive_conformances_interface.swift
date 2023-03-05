// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -swift-version 5 -DLIBRARY %s -module-name Library -o %t/Library.swiftmodule
// RUN: %target-swift-frontend -typecheck -swift-version 5 %s -I %t -enable-library-evolution -emit-module-interface-path - 2>&1 | %FileCheck %s

#if LIBRARY

public protocol LibraryProtocol {}

#else

import Library

// CHECK: extension Swift.String : Library.LibraryProtocol
extension String: @retroactive LibraryProtocol {}

#endif