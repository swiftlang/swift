// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %s -import-objc-header %S/Inputs/imported-block-typedefs.h -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse-as-library %t/imported-block-typedefs.swiftmodule -typecheck -emit-objc-header-path %t/imported-block-typedefs-output.h -import-objc-header %S/../Inputs/empty.h -disable-objc-attr-requires-foundation-module
// RUN: %FileCheck %s < %t/imported-block-typedefs-output.h
// RUN: %check-in-clang %t/imported-block-typedefs-output.h -include %S/Inputs/imported-block-typedefs.h

// REQUIRES: objc_interop

import ObjectiveC

// CHECK-LABEL: @interface Typedefs
@objc class Typedefs {
  
  // FIXME: The imported typedefs should be printed directly as the param types,
  // but one level of sugar is currently lost when applying @noescape. The importer
  // also loses __attribute__((noescape)) for params of imported function types.
  // <https://bugs.swift.org/browse/SR-2520>
  // <https://bugs.swift.org/browse/SR-2529>
  
  // CHECK-NEXT: - (void)noescapeParam1:(SWIFT_NOESCAPE void (^ _Nonnull)(void))input;
  // CHECK-NEXT: - (void)noescapeParam2:(SWIFT_NOESCAPE void (^ _Nonnull)(PlainBlock _Nullable))input;
  // CHECK-NEXT: - (void)noescapeParam3:(SWIFT_NOESCAPE void (^ _Nonnull)(PlainBlock _Nullable))input;
  // CHECK-NEXT: - (void)noescapeParam4:(SWIFT_NOESCAPE BlockWithEscapingParam _Nullable (^ _Nonnull)(void))input;
  // CHECK-NEXT: - (void)noescapeParam5:(SWIFT_NOESCAPE BlockWithNoescapeParam _Nullable (^ _Nonnull)(void))input;
  // Ideally should be:
  // - (void)noescapeParam1:(SWIFT_NOESCAPE PlainBlock _Nonnull)input;
  // - (void)noescapeParam2:(SWIFT_NOESCAPE BlockWithEscapingParam _Nonnull)input;
  // - (void)noescapeParam3:(SWIFT_NOESCAPE BlockWithNoescapeParam _Nonnull)input;
  // - (void)noescapeParam4:(SWIFT_NOESCAPE BlockReturningBlockWithEscapingParam _Nonnull)input;
  // - (void)noescapeParam5:(SWIFT_NOESCAPE BlockReturningBlockWithNoescapeParam _Nonnull)input;
  @objc func noescapeParam1(_ input: PlainBlock) {}
  @objc func noescapeParam2(_ input: BlockWithEscapingParam) {}
  @objc func noescapeParam3(_ input: BlockWithNoescapeParam) {}
  @objc func noescapeParam4(_ input: BlockReturningBlockWithEscapingParam) {}
  @objc func noescapeParam5(_ input: BlockReturningBlockWithNoescapeParam) {}
  
  // CHECK-NEXT: - (void)escapingParam1:(PlainBlock _Nonnull)input;
  // CHECK-NEXT: - (void)escapingParam2:(BlockWithEscapingParam _Nonnull)input;
  // CHECK-NEXT: - (void)escapingParam3:(BlockWithNoescapeParam _Nonnull)input;
  // CHECK-NEXT: - (void)escapingParam4:(BlockReturningBlockWithEscapingParam _Nonnull)input;
  // CHECK-NEXT: - (void)escapingParam5:(BlockReturningBlockWithNoescapeParam _Nonnull)input;
  @objc func escapingParam1(_ input: @escaping PlainBlock) {}
  @objc func escapingParam2(_ input: @escaping BlockWithEscapingParam) {}
  @objc func escapingParam3(_ input: @escaping BlockWithNoescapeParam) {}
  @objc func escapingParam4(_ input: @escaping BlockReturningBlockWithEscapingParam) {}
  @objc func escapingParam5(_ input: @escaping BlockReturningBlockWithNoescapeParam) {}
  
  // CHECK-NEXT: - (void (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(void)))resultHasNoescapeParam1 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(PlainBlock _Nullable)))resultHasNoescapeParam2 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(SWIFT_NOESCAPE void (^ _Nonnull)(PlainBlock _Nullable)))resultHasNoescapeParam3 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockWithEscapingParam _Nullable (^ _Nonnull)(void)))resultHasNoescapeParam4 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockWithNoescapeParam _Nullable (^ _Nonnull)(void)))resultHasNoescapeParam5 SWIFT_WARN_UNUSED_RESULT;
  // Ideally should be:
  //  - (void (^ _Nonnull)(SWIFT_NOESCAPE PlainBlock _Nonnull))resultHasNoescapeParam1 SWIFT_WARN_UNUSED_RESULT;
  //  - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockWithEscapingParam _Nonnull))resultHasNoescapeParam2 SWIFT_WARN_UNUSED_RESULT;
  //  - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockWithNoescapeParam _Nonnull))resultHasNoescapeParam3 SWIFT_WARN_UNUSED_RESULT;
  //  - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockReturningBlockWithEscapingParam _Nonnull))resultHasNoescapeParam4 SWIFT_WARN_UNUSED_RESULT;
  //  - (void (^ _Nonnull)(SWIFT_NOESCAPE BlockReturningBlockWithNoescapeParam _Nonnull))resultHasNoescapeParam5 SWIFT_WARN_UNUSED_RESULT;
  @objc func resultHasNoescapeParam1() -> (PlainBlock) -> () { fatalError() }
  @objc func resultHasNoescapeParam2() -> (BlockWithEscapingParam) -> () { fatalError() }
  @objc func resultHasNoescapeParam3() -> (BlockWithNoescapeParam) -> () { fatalError() }
  @objc func resultHasNoescapeParam4() -> (BlockReturningBlockWithEscapingParam) -> () { fatalError() }
  @objc func resultHasNoescapeParam5() -> (BlockReturningBlockWithNoescapeParam) -> () { fatalError() }
  
  // CHECK-NEXT: - (void (^ _Nonnull)(PlainBlock _Nonnull))resultHasEscapingParam1 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(BlockWithEscapingParam _Nonnull))resultHasEscapingParam2 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(BlockWithNoescapeParam _Nonnull))resultHasEscapingParam3 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(BlockReturningBlockWithEscapingParam _Nonnull))resultHasEscapingParam4 SWIFT_WARN_UNUSED_RESULT;
  // CHECK-NEXT: - (void (^ _Nonnull)(BlockReturningBlockWithNoescapeParam _Nonnull))resultHasEscapingParam5 SWIFT_WARN_UNUSED_RESULT;
  @objc func resultHasEscapingParam1() -> (@escaping PlainBlock) -> () { fatalError() }
  @objc func resultHasEscapingParam2() -> (@escaping BlockWithEscapingParam) -> () { fatalError() }
  @objc func resultHasEscapingParam3() -> (@escaping BlockWithNoescapeParam) -> () { fatalError() }
  @objc func resultHasEscapingParam4() -> (@escaping BlockReturningBlockWithEscapingParam) -> () { fatalError() }
  @objc func resultHasEscapingParam5() -> (@escaping BlockReturningBlockWithNoescapeParam) -> () { fatalError() }

}
// CHECK-NEXT: @end
