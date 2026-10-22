// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule -I %S/Inputs/custom-modules %s
// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD | %FileCheck %s

// CHECK-LABEL: class GenericBase<T> {
// CHECK-LABEL: class Grandchild : Sub {
// CHECK: class Nested {
// CHECK-LABEL: class Sub : GenericBase<Grandchild.Nested> {

public class GenericBase<T> {}
public class Sub: GenericBase<Grandchild.Nested> {
}
public class Grandchild: Sub {
  public class Nested {}
}
