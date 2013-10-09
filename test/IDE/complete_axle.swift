// Axle-specific code completions.

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_VAR_TYPE -std=axle > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR -std=axle > %t.types.txt
// RUN: FileCheck %s -check-prefix=WITH_GLOBAL_TYPES < %t.types.txt

// WITH_GLOBAL_TYPES: Begin completions
// WITH_GLOBAL_TYPES-DAG: Vec<{#type#}, {#length#}>
// WITH_GLOBAL_TYPES-DAG: Matrix<{#type#}, {#rows#}, {#columns#}>
// WITH_GLOBAL_TYPES: End completions

var x : #^TOP_LEVEL_VAR_TYPE^#

func f() {
  #^EXPR^#
}
