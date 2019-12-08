// Check that we generate the right code with the flag.
// RUN: %target-swift-emit-silgen -enable-experimental-concise-pound-file -module-name Foo %/s | %FileCheck %s

// Check that we give errors for use of #filePath if concise #file isn't enabled.
// FIXME: Drop if we stop rejecting this.
// RUN: %target-typecheck-verify-swift -module-name Foo %s

// FIXME: Once this feature becomes non-experimental, we should duplicate
// existing #file tests and delete this file.

func directUse() {
  print(#filePath) // expected-error {{use of unknown directive '#filePath'}}

// CHECK-LABEL: sil {{.*}} @$s3Foo9directUseyyF
// CHECK: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_filepath.swift"
}

func indirectUse() {
  functionWithFilePathDefaultArgument()

// CHECK-LABEL: sil {{.*}} @$s3Foo11indirectUseyyF
// CHECK: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_filepath.swift"
}

func functionWithFilePathDefaultArgument(file: String = #filePath) {}
// expected-error@-1 {{use of unknown directive '#filePath'}}
