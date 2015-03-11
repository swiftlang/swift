// RUN: rm -rf %t && mkdir %t

// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_notASwiftSymbol

// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_test12PublicStruct
// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_test14InternalStruct
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP13PrivateStruct
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP1_13PrivateStruct
// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtV14swift_ide_testP33_5CB4BCC03C4B9CB2AEEDDFF10FE7BD1E13PrivateStruct | FileCheck -check-prefix=THIS-FILE %s

// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer5Inner
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer6Absent
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test6Absent5Inner
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5Outer12PrivateInner
// RUN: not %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5OuterP1_12PrivateInner
// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_test5OuterP33_5CB4BCC03C4B9CB2AEEDDFF10FE7BD1E12PrivateInner
// RUN: %target-swift-ide-test -source-filename=%s -print-ast-typechecked -find-mangled=_TtCC14swift_ide_testP33_5CB4BCC03C4B9CB2AEEDDFF10FE7BD1E12PrivateOuter5Inner

// RUN: %target-swiftc_driver -emit-module -o %t %s %S/Inputs/lookup_other.swift -module-name Lookup
// RUN: echo 'import Lookup' > %t/test.swift

// RUN: %target-swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6Lookup12PublicStruct
// RUN: %target-swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6Lookup14InternalStruct
// RUN: %target-swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6LookupP33_FB24ABFEF851D18A6D2510DCD3FD6D6013PrivateStruct | FileCheck -check-prefix=THIS-FILE %s
// RUN: %target-swift-ide-test -source-filename=%t/test.swift -print-ast-typechecked -I %t -find-mangled=_TtV6LookupP33_F999E3591DC4FCB0EC84CD4166BF8EDB13PrivateStruct | FileCheck -check-prefix=OTHER-FILE %s

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
