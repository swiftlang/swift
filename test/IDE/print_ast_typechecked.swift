public class C {
  private var PrivateVar = 10
  public var PublicVar = 10
  var InternalVar = 10
  var InternalVar1 = 10, InternalVar2 = 10
  private var (PrivateTuple1, PrivateTuple2) = (10, 20)
  var (InternalTuple1, InternalTuple2) = (10, 20)
}

// RUN: %target-swift-ide-test -print-ast-typechecked -accessibility-filter-internal -source-filename %s | FileCheck %s -check-prefix=CHECK1
// CHECK1: {{^}}public class C {{{$}}
// CHECK1-NOT: private
// CHECK1: {{^}}  public var PublicVar: Int{{$}}
// CHECK1: {{^}}  var InternalVar: Int{{$}}
// CHECK1: {{^}}  var InternalVar1: Int, InternalVar2: Int{{$}}
// CHECK1-NOT: private
// CHECK1: {{^}}  var (InternalTuple1, InternalTuple2): (Int, Int){{$}}
