// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_IF_1 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_IF_2 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_IF_3 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_IF_4 | %FileCheck %s -check-prefix=UNWRAPPED_INT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_WHILE_1 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_WHILE_2 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_WHILE_3 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_WHILE_4 | %FileCheck %s -check-prefix=UNWRAPPED_INT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_IF_1 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_IF_2 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_IF_3 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_IF_4 | %FileCheck %s -check-prefix=UNWRAPPED_INT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_WHILE_1 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_WHILE_2 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_WHILE_3 | %FileCheck %s -check-prefix=UNWRAPPED_INT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNCTION_WHILE_4 | %FileCheck %s -check-prefix=UNWRAPPED_INT

// UNWRAPPED_INT-DAG: Decl[LocalVar]/Local: unwrapped[#Int#]{{; name=.+$}}

func getOptional() -> Int? {
  return .none
}

func getImplicitlyUnwrappedOptional() -> Int! {
  return .none
}

if var unwrapped = getOptional() {
  #^TOP_LEVEL_IF_1^#
}
if let unwrapped = getOptional() {
  #^TOP_LEVEL_IF_2^#
}
if var unwrapped = getImplicitlyUnwrappedOptional() {
  #^TOP_LEVEL_IF_3^#
}
if let unwrapped = getImplicitlyUnwrappedOptional() {
  #^TOP_LEVEL_IF_4^#
}

while var unwrapped = getOptional() {
  #^TOP_LEVEL_WHILE_1^#
}
while let unwrapped = getOptional() {
  #^TOP_LEVEL_WHILE_2^#
}
while var unwrapped = getImplicitlyUnwrappedOptional() {
  #^TOP_LEVEL_WHILE_3^#
}
while let unwrapped = getImplicitlyUnwrappedOptional() {
  #^TOP_LEVEL_WHILE_4^#
}

func testInsideAFunction() {
  if var unwrapped = getOptional() {
    #^IN_FUNCTION_IF_1^#
  }
  if let unwrapped = getOptional() {
    #^IN_FUNCTION_IF_2^#
  }
  if var unwrapped = getImplicitlyUnwrappedOptional() {
    #^IN_FUNCTION_IF_3^#
  }
  if let unwrapped = getImplicitlyUnwrappedOptional() {
    #^IN_FUNCTION_IF_4^#
  }

  while var unwrapped = getOptional() {
    #^IN_FUNCTION_WHILE_1^#
  }
  while let unwrapped = getOptional() {
    #^IN_FUNCTION_WHILE_2^#
  }
  while var unwrapped = getImplicitlyUnwrappedOptional() {
    #^IN_FUNCTION_WHILE_3^#
  }
  while let unwrapped = getImplicitlyUnwrappedOptional() {
    #^IN_FUNCTION_WHILE_4^#
  }
}
