// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen \
// RUN:   -target %target-swift-5.1-abi-triple -swift-version 5 \
// RUN:   -import-objc-header %t/ObjCBase.h -module-name main -verify %t/main.swift \
// RUN:   | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- ObjCBase.h

@import Foundation;

__attribute__((swift_attr("@MainActor")))
@interface ObjCBase : NSObject
@end

//--- main.swift

typealias Loader = @Sendable () async -> [Int]

func consume(loader: Loader? = nil) async -> [Int] {
  await loader?() ?? []
}

func acceptSendable(
  _ body: @escaping @Sendable () async -> [Int]
) {}

final class Controller: ObjCBase {
  func makeLoader() -> Loader {
    { [] }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4main10ControllerC4testyyF
  func test() {
    let loader = makeLoader()
    acceptSendable {
      // expected-warning@+1 {{converting non-Sendable function value to '@Sendable () async -> [Int]' may introduce data races}}
      await consume(loader: loader)
    }
  }
}

// CHECK: [[COPY:%.*]] = copy_value {{%.*}}
// CHECK-NEXT: [[CONVERTED:%.*]] = convert_function [[COPY]] to $@async @callee_guaranteed () -> @owned Array<Int>
// CHECK-NEXT: {{%.*}} = partial_apply {{.*}}([[CONVERTED]])
