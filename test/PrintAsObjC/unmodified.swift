// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/out.h
// RUN: touch -t 201401240005 %t/out.h
// RUN: %S/../Inputs/getmtime.py %t/out.h > %t/orig-mtime.txt

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/out.h
// RUN: %S/../Inputs/getmtime.py %t/out.h > %t/new-mtime.txt
// RUN: diff %t/orig-mtime.txt %t/new-mtime.txt


// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/out.h -DPRIVATE_EXTRA
// RUN: %S/../Inputs/getmtime.py %t/out.h > %t/private-mtime.txt
// RUN: diff %t/orig-mtime.txt %t/private-mtime.txt

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify -emit-objc-header-path %t/out.h -DPUBLIC_EXTRA
// RUN: %S/../Inputs/getmtime.py %t/out.h > %t/public-mtime.txt
// RUN: not diff %t/orig-mtime.txt %t/public-mtime.txt

// REQUIRES: objc_interop

import Foundation

public class A : NSObject {}

#if PRIVATE_EXTRA
private class B : NSObject {}
#endif

#if PUBLIC_EXTRA
public class C : NSObject {}
#endif
