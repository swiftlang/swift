// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-for-profiling -o - | %FileCheck %s

// CHECK: distinct !DICompileUnit(language: DW_LANG_Swift
// CHECK-SAME: debugInfoForProfiling: true

// CHECK: !DILexicalBlockFile({{.*}} discriminator: 2)

public func lobster(_ n: Int) -> Bool {
  guard n > 0 else { fatalError("too cold!") }

  if n > 100 {
    print("warm but ok")
  }

  return n < 120
}
