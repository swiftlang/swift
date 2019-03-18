// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -enable-objc-interop -parse-as-library %s -o /dev/null -disable-objc-attr-requires-foundation-module -module-name typealias_objc -emit-module -emit-module-path %t/typealias_objc.swiftmodule
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -function-definitions=true -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module | %FileCheck %s
@objc public protocol P {
  typealias T = () -> Bool
  // CHECK: typealias T = () -> Bool
}