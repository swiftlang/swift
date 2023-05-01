// RUN: %empty-directory(%t/split)
// RUN: %empty-directory(%t/build)
// RUN: %{python} %utils/split_file.py -o %t/split %s

// RUN: %target-swift-frontend -emit-module -o %t/build %t/split/pck.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %t/split/test.swift -I %t/build -code-completion-token=COMPLETE | %FileCheck %s

// BEGIN pck.swift

public protocol Foo<Bar> {
  associatedtype Bar
}

public typealias Problem = Foo<String>

public protocol EmptyProto {}
public typealias ConstrainedBar<T: EmptyProto> = Foo<T>

// BEGIN test.swift

import pck

#^COMPLETE^#

// CHECK-DAG: Decl[Protocol]/OtherModule[pck]/Flair[RareType]: Foo[#Foo#];
// CHECK-DAG: Decl[TypeAlias]/OtherModule[pck]:   Problem[#Foo<String>#];
// CHECK-DAG: Decl[TypeAlias]/OtherModule[pck]:   ConstrainedBar[#Foo<T>#];
