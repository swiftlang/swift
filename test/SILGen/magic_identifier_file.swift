// RUN: %target-swift-emit-silgen -pound-file=path -module-name Foo %s | %FileCheck --check-prefixes=BOTH,PATH %s
// RUN: %target-swift-emit-silgen -pound-file=compact -module-name Foo %s | %FileCheck --check-prefixes=BOTH,COMPACT %s
// RUN: not %target-swift-emit-silgen -pound-file=zyzyx -module-name Foo %s 2>&1 | %FileCheck --check-prefix=ZYZYX %s

// RUN: %target-swift-emit-silgen -pound-file path -module-name Foo %s | %FileCheck --check-prefixes=BOTH,PATH %s
// RUN: %target-swift-emit-silgen -pound-file compact -module-name Foo %s | %FileCheck --check-prefixes=BOTH,COMPACT %s
// RUN: not %target-swift-emit-silgen -pound-file zyzyx -module-name Foo %s 2>&1 | %FileCheck --check-prefix=ZYZYX %s

// RUN: %target-swift-emit-silgen %s -module-name Foo | %FileCheck --check-prefixes=BOTH,PATH %s

// ZYZYX: error: invalid value 'zyzyx' in '-pound-file'

func directUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo9directUseyyF
  print(#file)
// PATH: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// COMPACT: string_literal utf8 "magic_identifier_file.swift (Foo)"
}

func indirectUse() {
// BOTH-LABEL: sil {{.*}} @$s3Foo11indirectUseyyF
  fatalError()
// PATH: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_file.swift"
// COMPACT: string_literal utf8 "magic_identifier_file.swift (Foo)"
}
