// RUN: %target-swift-emit-silgen -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,ABSOLUTE %s
// RUN: %target-swift-emit-silgen -enable-experimental-concise-pound-file -DNEEDS_CONCISE -module-name Foo %/s | %FileCheck --check-prefixes=BOTH,CONCISE %s

// FIXME: Once this feature becomes non-experimental, we should update existing
// tests and delete this file.

func directUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo9directUseyyF
  print(#file)
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func indirectUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo11indirectUseyyF
  fatalError()
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func forceUnwrap(_ x: ()?) {
// BOTH-LABEL: sil {{.*}} @$s3Foo11forceUnwrapyyytSgF
  _ = x!
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

func forceTry(_ fn: () throws -> ()) {
// BOTH-LABEL: sil {{.*}} @$s3Foo8forceTryyyyyKXEF
  try! fn()
// ABSOLUTE: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// CONCISE: string_literal utf8 "Foo/magic_identifier_file.swift"
}

// CONCISE-LABEL: // Mappings from '#file' to '#filePath':
// CONCISE:       //   'Foo/magic_identifier_file.swift' => 'SOURCE_DIR/test/SILGen/magic_identifier_file.swift'

