// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen \
// RUN:   -target %target-swift-5.1-abi-triple -swift-version 5 \
// RUN:   -import-objc-header %t/ObjCBase.h -module-name main -verify %t/main.swift \
// RUN:   | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck \
// RUN:   -dump-ast -target %target-swift-5.1-abi-triple -swift-version 5 \
// RUN:   -import-objc-header %t/ObjCBase.h -module-name main -verify %t/main.swift \
// RUN:   | %FileCheck %s --check-prefix=AST
// RUN: %target-swift-frontend -emit-silgen -swift-version 6 -verify \
// RUN:   -module-name strict %t/strict.swift -o /dev/null

// REQUIRES: objc_interop
// REQUIRES: concurrency

//--- ObjCBase.h

@import Foundation;

typedef void (^ __attribute__((swift_attr("@Sendable"))) SendableBlock)(void);

__attribute__((swift_attr("@MainActor")))
@interface ObjCBase : NSObject
- (SendableBlock _Nonnull)makeImportedBlock;
@end

//--- main.swift

typealias Loader = @Sendable () async -> [Int]

func consume(loader: Loader? = nil) async -> [Int] {
  await loader?() ?? []
}

func acceptSendable(
  _ body: @escaping @Sendable () async -> [Int]
) {}

func acceptSendableVoid(
  _ body: @escaping @Sendable () -> Void
) {}

final class Controller: ObjCBase {
  func makeLoader() -> Loader {
    { [] }
  }

  func makeOptionalLoader() -> Loader? {
    makeLoader()
  }

  // AST-LABEL: "testDirect()"
  // AST: (processed_init=function_conversion_expr implicit type="() async -> [Int]"
  // AST-NEXT: (call_expr type="Loader"
  // CHECK-LABEL: sil hidden [ossa] @$s4main10ControllerC10testDirectyyF
  func testDirect() {
    // CHECK: apply
    // CHECK-NEXT: [[LOADER:%.*]] = convert_function {{%.*}} to $@async @callee_guaranteed () -> @owned Array<Int>
    // CHECK-NEXT: [[STORED_LOADER:%.*]] = move_value {{.*}} [[LOADER]]
    let loader = makeLoader()
    acceptSendable {
      // expected-warning@+1 {{converting non-Sendable function value to '@Sendable () async -> [Int]' may introduce data races}}
      await consume(loader: loader)
    }
    // CHECK: [[CAPTURED_LOADER:%.*]] = copy_value [[STORED_LOADER]]
    // CHECK-NEXT: {{%.*}} = partial_apply {{.*}}([[CAPTURED_LOADER]])
  }

  // AST-LABEL: "testOptional()"
  // AST: (processed_init=optional_evaluation_expr implicit type="(() async -> [Int])?"
  // AST: (function_conversion_expr implicit type="() async -> [Int]"
  // AST-NEXT: (bind_optional_expr implicit type="Loader"
  // AST-NEXT: (call_expr type="Loader?"
  // CHECK-LABEL: sil hidden [ossa] @$s4main10ControllerC12testOptionalyyF
  func testOptional() {
    // CHECK: [[OPTIONAL:%.*]] = apply
    // CHECK-NEXT: switch_enum [[OPTIONAL]], case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt:
    // CHECK: [[SOME_BB]]([[SENDABLE:%.*]] : @owned $@Sendable
    // CHECK-NEXT: [[CONVERTED:%.*]] = convert_function [[SENDABLE]] to $@async @callee_guaranteed () -> @owned Array<Int>
    // CHECK-NEXT: [[SOME:%.*]] = enum $Optional<@async @callee_guaranteed () -> @owned Array<Int>>, #Optional.some!enumelt, [[CONVERTED]]
    let loader = makeOptionalLoader()
    acceptSendable {
      // expected-warning@+1 {{converting non-Sendable function value to '@Sendable () async -> [Int]' may introduce data races}}
      await consume(loader: loader)
    }
    // CHECK: [[MERGE_BB:bb[0-9]+]]([[MERGED:%.*]] : @owned $Optional<@async @callee_guaranteed () -> @owned Array<Int>>)
    // CHECK-NEXT: [[STORED_OPTIONAL:%.*]] = move_value {{.*}} [[MERGED]]
    // CHECK: [[CAPTURED_OPTIONAL:%.*]] = copy_value [[STORED_OPTIONAL]]
    // CHECK-NEXT: {{%.*}} = partial_apply {{.*}}([[CAPTURED_OPTIONAL]])
  }

  // AST-LABEL: "testImportedBlock()"
  // AST: (processed_init=function_conversion_expr implicit type="() -> Void"
  // AST-NEXT: (call_expr type="SendableBlock"
  // CHECK-LABEL: sil hidden [ossa] @$s4main10ControllerC17testImportedBlockyyF
  func testImportedBlock() {
    // CHECK: [[IMPORTED_BLOCK:%.*]] = apply
    // CHECK: [[BLOCK_THUNK:%.*]] = function_ref @$sIeyBh_Iegh_TR
    // CHECK-NEXT: [[SENDABLE_BLOCK:%.*]] = partial_apply {{.*}} [[BLOCK_THUNK]]([[IMPORTED_BLOCK]])
    // CHECK-NEXT: [[BLOCK:%.*]] = convert_function [[SENDABLE_BLOCK]] to $@callee_guaranteed () -> ()
    // CHECK-NEXT: [[STORED_BLOCK:%.*]] = move_value {{.*}} [[BLOCK]]
    let block = makeImportedBlock()
    acceptSendableVoid {
      block()
    }
    // CHECK: [[CAPTURED_BLOCK:%.*]] = copy_value [[STORED_BLOCK]]
    // CHECK-NEXT: {{%.*}} = partial_apply {{.*}}([[CAPTURED_BLOCK]])
  }

  // AST-LABEL: "testTuple()"
  // AST: (processed_init=tuple_expr type="(() async -> [Int], () async -> [Int])"
  // AST-NEXT: (function_conversion_expr implicit type="() async -> [Int]"
  // AST-NEXT: (call_expr type="Loader"
  // AST: (function_conversion_expr implicit type="() async -> [Int]"
  // AST-NEXT: (call_expr type="Loader"
  // CHECK-LABEL: sil hidden [ossa] @$s4main10ControllerC9testTupleyyF
  func testTuple() {
    // CHECK: apply
    // CHECK-NEXT: [[FIRST:%.*]] = convert_function {{%.*}} to $@async @callee_guaranteed () -> @owned Array<Int>
    // CHECK: apply
    // CHECK-NEXT: [[SECOND:%.*]] = convert_function {{%.*}} to $@async @callee_guaranteed () -> @owned Array<Int>
    // CHECK-NEXT: [[TUPLE:%.*]] = tuple ([[FIRST]], [[SECOND]])
    // CHECK-NEXT: [[STORED_TUPLE:%.*]] = move_value {{.*}} [[TUPLE]]
    let loaders = (makeLoader(), makeLoader())
    acceptSendable {
      // expected-warning@+1 {{converting non-Sendable function value to '@Sendable () async -> [Int]' may introduce data races}}
      await consume(loader: loaders.0)
    }
    // CHECK: [[CAPTURED_TUPLE:%.*]] = copy_value [[STORED_TUPLE]]
    // CHECK-NEXT: {{%.*}} = partial_apply {{.*}}([[CAPTURED_TUPLE]])
  }
}

//--- strict.swift

class Isolated {
  @preconcurrency @MainActor func method() {}

  @preconcurrency @MainActor
  func loader() -> @Sendable () -> Void {
    {}
  }
}

@MainActor
func callIsolated(_ value: Isolated) {
  value.method()
  let loader = value.loader()
  loader()
}

protocol Protocol {
  func requirement()
}

@MainActor
final class MainActorConformance: @MainActor Protocol {
  func requirement() {}
}

@globalActor
actor OtherActor {
  static let shared = OtherActor()
}

final class Box: Sendable {
  @preconcurrency @OtherActor
  func accept(_ value: some Protocol) async {}
}

@MainActor
func checkIsolatedConformance(_ box: Box) async {
  // expected-error@+1 {{main actor-isolated conformance of 'MainActorConformance' to 'Protocol' cannot be used in global actor 'OtherActor'-isolated context}}
  await box.accept(MainActorConformance())
}
