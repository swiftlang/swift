// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

if true {
    print("\(1)")
    let foo = #^COMPLETE^#
}

// CHECK-LABEL: Begin completions
// CHECK: End completions
