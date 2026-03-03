// RUN: %target-swift-ide-test -signature-help -code-completion-token=INIT -source-filename=%s | %FileCheck %s --check-prefix=INIT

struct Person {
  init(name: String, age: Int, profession job: String) { }
}

Person(name: "John", age: #^INIT^#)
// INIT:     Begin signatures, 1 items
// INIT-DAG: Signature[Active]: init(<param name="name">name: String</param>, <param name="age" active>age: Int</param>, <param name="job">profession: String</param>)
