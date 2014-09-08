// RUN: rm -rf %t && mkdir %t

// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_notASwiftSymbol

// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_test12PublicStruct
// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_test14InternalStruct
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP13PrivateStruct
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP1_13PrivateStruct
// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP33_6EB8A1E82BFF39B1A61B6065CAD7A59613PrivateStruct | FileCheck -check-prefix=THIS-FILE %s

// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer5Inner
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer6Absent
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test6Absent5Inner
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer12PrivateInner
// RUN: not %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5OuterP1_12PrivateInner
// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5OuterP33_6EB8A1E82BFF39B1A61B6065CAD7A59612PrivateInner
// RUN: %swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_testP33_6EB8A1E82BFF39B1A61B6065CAD7A59612PrivateOuter5Inner

// RUN: %swiftc_driver -emit-module -o %t %s %S/Inputs/lookup_other.swift -module-name Lookup -Xfrontend -enable-private-discriminators
// RUN: echo 'import Lookup' > %t/test.swift

// RUN: %swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6Lookup12PublicStruct
// RUN: %swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6Lookup14InternalStruct
// RUN: %swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6LookupP33_6EB8A1E82BFF39B1A61B6065CAD7A59613PrivateStruct | FileCheck -check-prefix=THIS-FILE %s
// RUN: %swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6LookupP33_115B86AB11F1CBC1933D9E62EDF7541013PrivateStruct | FileCheck -check-prefix=OTHER-FILE %s

public struct PublicStruct {}
internal struct InternalStruct {}

private struct PrivateStruct {
  let fromMainFile: Int
}

class Outer {
  class Inner {}
  private class PrivateInner {}
}

private class PrivateOuter {
  class Inner {}
}

// THIS-FILE-NOT: fromOtherFile
// THIS-FILE: fromMainFile
// THIS-FILE-NOT: fromOtherFile

// OTHER-FILE-NOT: fromMainFile
// OTHER-FILE: fromOtherFile
// OTHER-FILE-NOT: fromMainFile
