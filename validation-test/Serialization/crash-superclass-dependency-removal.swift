// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule -enable-objc-interop -I %S/Inputs/custom-modules %s
// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -enable-objc-interop -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -enable-objc-interop -I %t -I %S/Inputs/custom-modules -Xcc -DBAD

import SuperclassObjC

public class GenericBase<T> {}

// CHECK: class Sub : GenericBase<Sub.Nested> {
public class Sub: GenericBase<Sub.Nested> {
  public class Nested: DisappearingSuperclass {}
}
