import Foo
import FooSwiftModule

var glob : Int

func foo(x: Int) {}

func goo(x: Int) {
  foo(glob+x+Int(fooIntVar)+fooSwiftFunc())
}

/// Aaa.  S1.  Bbb.
struct S1 {}
var w : S1
func test2(x: S1) {}

class CC {
  init(x: Int) {
    self.init(x:0)
  }
}

var testString = "testString"
let testLetString = "testString"

func testLetParam(arg1 : Int) {
}
func testVarParam(var arg1 : Int) {
}

func testDefaultParam(arg1: Int = 0) {
}

fooSubFunc1(0)

func myFunc(arg1: String) {
}
func myFunc(arg1: String, options: Int) {
}

var derivedObj = FooClassDerived()

typealias MyInt = Int
var x: MyInt

import FooHelper.FooHelperSub

class C2 {
  lazy var lazy_bar : Int = {
    return x
  }()
}

func test1(foo: FooUnavailableMembers) {
  foo.availabilityIntroduced()
  foo.swiftUnavailable()
  foo.unavailable()
  foo.availabilityIntroducedMsg()
  foo.availabilityDeprecated()
}

public class SubscriptCursorTest {
  public subscript (i: Int) -> Int {
    return 0
  }

  public static func test() {
    let s = SubscriptCursorTest()
    let a = s[1234] + s[4321]
  }
}

// RUN: rm -rf %t.tmp
// RUN: mkdir %t.tmp
// RUN: %swiftc_driver -emit-module -target x86_64-apple-macosx10.9 -o %t.tmp/FooSwiftModule.swiftmodule %S/Inputs/FooSwiftModule.swift
// RUN: %sourcekitd-test -req=cursor -pos=9:8 %s -- -triple x86_64-apple-macosx10.9 -F %S/../Inputs/libIDE-mock-sdk %mcp_opt %s | FileCheck -check-prefix=CHECK1 %s
// CHECK1:      source.lang.swift.ref.var.global (4:5-4:9)
// CHECK1-NEXT: glob
// CHECK1-NEXT: s:v11cursor_info4globSi{{$}}
// CHECK1-NEXT: Int

// RUN: %sourcekitd-test -req=cursor -pos=9:11 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK2 %s
// CHECK2:      source.lang.swift.ref.function.operator.infix ()
// CHECK2-NEXT: +
// CHECK2-NEXT: s:ZFsoi1pFTSiSi_Si
// CHECK2-NEXT: (Int, Int) -> Int{{$}}
// CHECK2-NEXT: Swift{{$}}
// CHECK2-NEXT: SYSTEM
// CHECK2-NEXT: <Declaration>func +(lhs: <Type usr="s:Si">Int</Type>, rhs: <Type usr="s:Si">Int</Type>) -&gt; <Type usr="s:Si">Int</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=9:12 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK3 %s
// CHECK3:      source.lang.swift.ref.var.local (8:10-8:11)
// CHECK3-NEXT: x{{$}}
// CHECK3-NEXT: s:vF11cursor_info3gooFSiT_L_1xSi{{$}}
// CHECK3-NEXT: Int{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=9:18 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK4 %s
// CHECK4:      source.lang.swift.ref.var.global ({{.*}}Foo.framework/Headers/Foo.h:62:12-62:21)
// CHECK4-NEXT: fooIntVar{{$}}
// CHECK4-NEXT: c:@fooIntVar{{$}}
// CHECK4-NEXT: Int32{{$}}
// CHECK4-NEXT: Foo{{$}}
// CHECK4-NEXT: <Declaration>var fooIntVar: <Type usr="s:Vs5Int32">Int32</Type></Declaration>
// CHECK4-NEXT: <Variable file="{{[^"]+}}Foo.h" line="{{[0-9]+}}" column="{{[0-9]+}}"><Name>fooIntVar</Name><USR>c:@fooIntVar</USR><Declaration>var fooIntVar: Int32</Declaration><Abstract><Para> Aaa. fooIntVar. Bbb.</Para></Abstract></Variable>

// RUN: %sourcekitd-test -req=cursor -pos=8:7 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK5 %s
// CHECK5:      source.lang.swift.decl.function.free (8:6-8:17)
// CHECK5-NEXT: goo(_:){{$}}
// CHECK5-NEXT: s:F11cursor_info3gooFSiT_{{$}}
// CHECK5-NEXT: (Int) -> (){{$}}

// RUN: %sourcekitd-test -req=cursor -pos=9:32 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK6 %s
// CHECK6:      source.lang.swift.ref.function.free ()
// CHECK6-NEXT: fooSwiftFunc
// CHECK6-NEXT: s:F14FooSwiftModule12fooSwiftFuncFT_Si
// CHECK6-NEXT: () -> Int
// CHECK6-NEXT: FooSwiftModule
// CHECK6-NEXT: <Declaration>func fooSwiftFunc() -&gt; <Type usr="s:Si">Int</Type></Declaration>
// CHECK6-NEXT: {{^}}<Function><Name>fooSwiftFunc()</Name><USR>s:F14FooSwiftModule12fooSwiftFuncFT_Si</USR><Declaration>func fooSwiftFunc() -&gt; Int</Declaration><Abstract><Para>This is &apos;fooSwiftFunc&apos; from &apos;FooSwiftModule&apos;.</Para></Abstract></Function>{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=14:10 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK7 %s
// CHECK7:      source.lang.swift.ref.struct (13:8-13:10)
// CHECK7-NEXT: S1
// CHECK7-NEXT: s:V11cursor_info2S1
// CHECK7-NEXT: S1.Type
// CHECK7-NEXT: <Declaration>struct S1</Declaration>
// CHECK7-NEXT: <Class file="{{[^"]+}}cursor_info.swift" line="13" column="8"><Name>S1</Name><USR>s:V11cursor_info2S1</USR><Declaration>struct S1</Declaration><Abstract><Para>Aaa.  S1.  Bbb.</Para></Abstract></Class>

// RUN: %sourcekitd-test -req=cursor -pos=19:12 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK8 %s
// CHECK8:      source.lang.swift.ref.function.constructor (18:3-18:15)
// CHECK8-NEXT: init
// CHECK8-NEXT: s:FC11cursor_info2CCcFT1xSi_S0_
// CHECK8-NEXT: CC.Type -> (x: Int) -> CC

// RUN: %sourcekitd-test -req=cursor -pos=23:6 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK9 %s
// CHECK9:      source.lang.swift.decl.var.global (23:5-23:15)
// CHECK9: <Declaration>var testString: <Type usr="s:SS">String</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=24:6 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK10 %s
// CHECK10: source.lang.swift.decl.var.global (24:5-24:18)
// CHECK10: <Declaration>let testLetString: <Type usr="s:SS">String</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=26:20 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK11 %s
// CHECK11: source.lang.swift.decl.var.local (26:19-26:23)
// CHECK11: <Declaration>let arg1: <Type usr="s:Si">Int</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=28:24 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK12 %s
// CHECK12: source.lang.swift.decl.var.local (28:23-28:27)
// CHECK12: <Declaration>var arg1: <Type usr="s:Si">Int</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=31:7 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK13 %s
// CHECK13: source.lang.swift.decl.function.free (31:6-31:37)
// CHECK13: <Declaration>func testDefaultParam(arg1: <Type usr="s:Si">Int</Type> = default)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=34:4 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK14 %s
// CHECK14: source.lang.swift.ref.function.free ({{.*}}Foo.framework/Frameworks/FooSub.framework/Headers/FooSub.h:4:5-4:16)
// CHECK14: fooSubFunc1
// CHECK14: c:@F@fooSubFunc1
// CHECK14: Foo.FooSub{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=38:8 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK15 %s
// CHECK15: source.lang.swift.decl.function.free (38:6-38:40)
// CHECK15: myFunc
// CHECK15: <Declaration>func myFunc(arg1: <Type usr="s:SS">String</Type>, options: <Type usr="s:Si">Int</Type>)</Declaration>
// CHECK15: RELATED BEGIN
// CHECK15-NEXT: <RelatedName usr="s:F11cursor_info6myFuncFSST_">myFunc(_:)</RelatedName>
// CHECK15-NEXT: RELATED END

// RUN: %sourcekitd-test -req=cursor -pos=41:26 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK16 %s
// CHECK16:      source.lang.swift.ref.class ({{.*}}Foo.framework/Headers/Foo.h:157:12-157:27)
// CHECK16-NEXT: FooClassDerived
// CHECK16-NEXT: c:objc(cs)FooClassDerived

// RUN: %sourcekitd-test -req=cursor -pos=1:10 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK17 %s
// CHECK17:      source.lang.swift.ref.module ()
// CHECK17-NEXT: Foo{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=44:10 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK18 %s
// CHECK18: source.lang.swift.ref.typealias (43:11-43:16)
// CHECK18: <Declaration>typealias MyInt = <Type usr="s:Si">Int</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=46:10 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK19 %s
// CHECK19:      source.lang.swift.ref.module ()
// CHECK19-NEXT: FooHelper{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=46:25 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK20 %s
// CHECK20:      source.lang.swift.ref.module ()
// CHECK20-NEXT: FooHelperSub{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=50:12 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK21 %s
// CHECK21:      source.lang.swift.ref.var.global (44:5-44:6)
// CHECK21-NEXT:  {{^}}x{{$}}

// RUN: %sourcekitd-test -req=cursor -pos=55:15 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK22 %s
// CHECK22: <Declaration>func availabilityIntroduced()</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=56:15 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK23 %s
// CHECK23-NOT: <Declaration>func swiftUnavailable()</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=57:15 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK24 %s
// CHECK24-NOT: <Declaration>func unavailable()</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=58:15 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK25 %s
// CHECK25: <Declaration>func availabilityIntroducedMsg()</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=59:15 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK26 %s
// CHECK26-NOT: <Declaration>func availabilityDeprecated()</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=69:14 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK27 %s
// CHECK27: <Declaration>public subscript (i: <Type usr="s:Si">Int</Type>) -&gt; <Type usr="s:Si">Int</Type> { get }</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=69:19 %s -- -F %S/../Inputs/libIDE-mock-sdk -I %t.tmp %mcp_opt %s | FileCheck -check-prefix=CHECK28 %s
// CHECK28: <Declaration>public subscript (i: <Type usr="s:Si">Int</Type>) -&gt; <Type usr="s:Si">Int</Type> { get }</Declaration>

