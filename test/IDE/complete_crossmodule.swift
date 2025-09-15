// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift
// RUN: %target-swift-ide-test -code-completion -source-filename %t/Test.swift -I %t -code-completion-token=OPAQUE_RESULT | %FileCheck --check-prefix=OPAQUE_RESULT %s

// BEGIN MyModule.swift

public protocol HasAssocWithConstraint {
  associatedtype AssocWithConstraint: HasAssocWithConstraint
  var value: AssocWithConstraint { get }
}

// BEGIN Test.swift
import MyModule

struct MyValue: HasAssocWithConstraint {
  var #^OPAQUE_RESULT^#
// OPAQUE_RESULT-DAG: Decl[InstanceVar]/Super: value: some HasAssocWithConstraint;
}
