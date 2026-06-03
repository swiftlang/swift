// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-clang %t/Impl.m -c -o %t/Test.o
// RUN: %target-build-swift %t/main.swift %t/doc.swift -import-objc-header %t/Test.h %t/Test.o -language-mode 5 -strict-concurrency=complete -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: concurrency

// UNSUPPORTED: remote_run || device_run

//--- Test.h
#import "Foundation/Foundation.h"

#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))

@interface Document : NSObject
- (void) relinquishToReader:(void (SWIFT_SENDABLE ^)(void (SWIFT_SENDABLE ^reacquirer)())) reader;
@end

//--- Impl.m
#import "Test.h"
@implementation Document
  - (void) relinquishToReader:(void (^ SWIFT_SENDABLE)(void (^reacquirer)())) reader {
  }
@end

//--- doc.swift
class Doc: Document {
  // No Sendable annotations
  override func relinquish(toReader: @escaping ((() -> Void)?) -> Void) {
  }

  func test1() {
    print("test1")
  }

  func test2() {
    print("test2")
  }
}

//--- main.swift
func test(doc: Doc) {
  doc.test1()
}

test(doc: Doc())
// CHECK: test1
