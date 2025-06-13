// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 6 \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop

//--- Test.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))
#define NONSENDABLE __attribute__((__swift_attr__("@_nonSendable")))
#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))

#pragma clang assume_nonnull begin

@import Foundation;

MAIN_ACTOR
@protocol P <NSObject>
@end

@interface TestFromProtocol <P>
-(void) compute: (void (^)(void)) completion;
@end

MAIN_ACTOR
@interface TestFromType <NSObject>
-(void) compute: (void (^)(void)) completion;
@end

@interface TestSubclass : TestFromType
-(void) subCompute: (void (^)(void)) completion;
@end

@interface TestFromMethod <NSObject>
-(void) MAIN_ACTOR compute: (void (^)(void)) completion;
+(void) MAIN_ACTOR computeStatic: (void (^)(void)) completion;
@end

#pragma clang assume_nonnull end

//--- main.swift

func testFromProtocol(v: TestFromProtocol) {
  let _: Int = v.compute
  // expected-error@-1 {{cannot convert value of type '@MainActor @Sendable (@escaping () -> Void) -> Void' to specified type 'Int'}}
}

func testFromType(v: TestFromType) {
  let _: Int = v.compute
  // expected-error@-1 {{annot convert value of type '@MainActor @Sendable (@escaping () -> Void) -> Void' to specified type 'Int'}}
}

func testFromSuperclass(v: TestSubclass) {
  let _: Int = v.subCompute
  // expected-error@-1 {{cannot convert value of type '@MainActor @Sendable (@escaping () -> Void) -> Void' to specified type 'Int'}}
}

func testFromMethod(v: TestFromMethod, t: TestFromMethod.Type) {
  let _: Int = v.compute
  // expected-error@-1 {{cannot convert value of type '@MainActor (@escaping () -> Void) -> Void' to specified type 'Int'}}

  let _: Int = t.computeStatic
  // expected-error@-1 {{cannot convert value of type '@MainActor @Sendable (@escaping () -> Void) -> Void' to specified type 'Int'}}
}

nonisolated func testUse(v1: TestFromProtocol, v2: TestFromType, v3: TestSubclass, v4: TestFromMethod, v5: TestFromMethod.Type) async {
  var val: Int = 0

  await v1.compute { val += 1 } // No execution warning because parameter type isn't Sendable
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v1.compute { @Sendable in val += 1 } // expected-warning {{mutation of captured var 'val' in concurrently-executing code}}
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v2.compute { val += 1 } // No execution warning because parameter type isn't Sendable
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v2.compute { @Sendable in val += 1 } // expected-warning {{mutation of captured var 'val' in concurrently-executing code}}
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v3.subCompute { val += 1 } // No execution warning because parameter type isn't Sendable
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v3.subCompute { @Sendable in val += 1 } // expected-warning {{mutation of captured var 'val' in concurrently-executing code}}
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v4.compute { val += 1 } // No execution warning because parameter type isn't Sendable
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v4.compute { @Sendable in val += 1 } // expected-warning {{mutation of captured var 'val' in concurrently-executing code}}
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v5.computeStatic { val += 1 } // No execution warning because parameter type isn't Sendable
  // expected-warning@-1 {{consider using asynchronous alternative function}}

  await v5.computeStatic { @Sendable in val += 1 } // expected-warning {{mutation of captured var 'val' in concurrently-executing code}}
  // expected-warning@-1 {{consider using asynchronous alternative function}}
}
