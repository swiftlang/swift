// RUN: %target-swift-ide-test --code-completion --code-completion-token COMPLETE --source-filename %s | %FileCheck %s

func foo(completionHandler completion: (@Sendable (Bool) -> Void)? = nil) {}

func test() {
  foo(completionHandler: #^COMPLETE^#nil)
}

// CHECK-DAG: Literal[Nil]/None/TypeRelation[Convertible]: nil[#(@Sendable (Bool) -> Void)?#];
