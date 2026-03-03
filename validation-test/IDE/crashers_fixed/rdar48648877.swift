// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%t/main.swift %t/a.swift %t/b.swift %t/c.swift %t/d.swift

// BEGIN main.swift
func foo(x: Int, y: Int) {
  x + y #^COMPLETE^#
}

// BEGIN a.swift

infix operator ***

// BEGIN b.swift

infix operator ***

// BEGIN c.swift

precedencegroup FooPrecedenceGroup {
  higherThan: MultiplicationPrecedence
}
infix operator **** : FooPrecedenceGroup

// BEGIN d.swift

precedencegroup FooPrecedenceGroup {
  higherThan: MultiplicationPrecedence
}
infix operator **** : FooPrecedenceGroup
