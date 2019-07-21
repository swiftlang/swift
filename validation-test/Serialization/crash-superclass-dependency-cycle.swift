// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule -I %S/Inputs/custom-modules %s

// FIXME: We need a way to handle the cyclic dependency here.
// rdar://problem/50835214
// RUN: not --crash %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD

public class GenericBase<T> {}
public class Sub: GenericBase<Grandchild.Nested> {
}
public class Grandchild: Sub {
  public class Nested {}
}
