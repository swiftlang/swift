func test(x: Foo) {
  
  x.
}

func bad() {}
func good() {}
func okay() {}

struct Foo {
  func bad() { }
  func good() { }
  func good(_ p1: Int, p2: Any..., p3: ()->(), p4: (Int, Int), p5: inout Int) { }
  func okay() {}
  var sad: Int
  var xhappy: Int
  var zmeh: Int
}

// REQUIRES: objc_interop
// RUN: %sourcekitd-test -req=complete.open -pos=2:1 -req-opts=hidelowpriority=0 %s -- %s > %t.nopopular.top
// RUN: %sourcekitd-test -req=complete.open -pos=3:5 %s -- %s > %t.nopopular.foo
// RUN: %FileCheck %s -check-prefix=NOPOP_TOP < %t.nopopular.top
// RUN: %FileCheck %s -check-prefix=NOPOP_FOO < %t.nopopular.foo

// RUN: %sourcekitd-test -req=complete.setpopularapi -req-opts=popular=%s.popular,unpopular=%s.unpopular \
// RUN:               == -req=complete.open -req-opts=hidelowpriority=0 -pos=2:1 %s -- %s > %t.popular.top
// RUN: %sourcekitd-test -req=complete.setpopularapi -req-opts=popular=%s.popular,unpopular=%s.unpopular \
// RUN:               == -req=complete.open -pos=3:5 %s -- %s > %t.popular.foo
// RUN: %FileCheck %s -check-prefix=POP_TOP < %t.popular.top
// RUN: %FileCheck %s -check-prefix=POP_FOO < %t.popular.foo

// NOPOP_TOP: key.name: "bad()
// NOPOP_TOP: key.name: "good()
// NOPOP_TOP: key.name: "okay()

// POP_TOP: key.name: "good()
// POP_TOP: key.name: "okay()
// POP_TOP: key.name: "bad()

// NOPOP_FOO: key.name: "bad()
// NOPOP_FOO: key.name: "good()
// NOPOP_FOO: key.name: "good(:p2:p3:p4:p5:)
// NOPOP_FOO: key.name: "okay()
// NOPOP_FOO: key.name: "sad
// NOPOP_FOO: key.name: "xhappy
// NOPOP_FOO: key.name: "zmeh

// POP_FOO: key.name: "good(:p2:p3:p4:p5:)
// POP_FOO: key.name: "good()
// POP_FOO: key.name: "xhappy
// POP_FOO: key.name: "okay()
// POP_FOO: key.name: "zmeh
// POP_FOO: key.name: "sad
// POP_FOO: key.name: "bad()


// RUN: %complete-test -hide-none -fuzz -group=none -popular="%s.popular" -unpopular="%s.unpopular" -tok=POPULAR_STMT_0 %s -- -I %S/Inputs > %t.popular.stmt.0
// RUN: %FileCheck %s -check-prefix=POPULAR_STMT_0 < %t.popular.stmt.0

import PopularAPI
var globalColor = 0

struct OuterNominal {
  var fromOuterNominalColor: Int = 0

  class Super {
    var fromSuperColor: Int = 0
  }

  class Derived : Super {
    var fromDerivedColor: Int = 0

    func test(argColor: Int) {
      let localColor = 1
      #^POPULAR_STMT_0,,col^#
// POPULAR_STMT_0-LABEL: Results for filterText: [
// POPULAR_STMT_0:   argColor
// POPULAR_STMT_0:   localColor
// POPULAR_STMT_0:   fromDerivedColor
// POPULAR_STMT_0:   fromSuperColor
// POPULAR_STMT_0:   good()
// POPULAR_STMT_0:   fromOuterNominalColor
// POPULAR_STMT_0:   DDModuleColor
// POPULAR_STMT_0:   CCModuleColor
// POPULAR_STMT_0:   EEModuleColor
// POPULAR_STMT_0:   globalColor
// POPULAR_STMT_0:   okay()
// POPULAR_STMT_0:   ModuleCollaborate
// POPULAR_STMT_0:   bad()
// POPULAR_STMT_0: ]
// POPULAR_STMT_0-LABEL: Results for filterText: col [
// POPULAR_STMT_0:   argColor
// POPULAR_STMT_0:   localColor
// POPULAR_STMT_0:   fromDerivedColor
// POPULAR_STMT_0:   fromSuperColor
// POPULAR_STMT_0:   fromOuterNominalColor
// POPULAR_STMT_0:   globalColor
// POPULAR_STMT_0:   DDModuleColor
// POPULAR_STMT_0:   CCModuleColor
// POPULAR_STMT_0:   EEModuleColor
// POPULAR_STMT_0:   ModuleCollaborate
// POPULAR_STMT_0:   BBModuleColor
// POPULAR_STMT_0:   AAModuleColor
// POPULAR_STMT_0: ]
    }
  }
}

struct Outer {
  struct ABTabularMonkey {}
  struct ABTextMockery {}
  struct ABTradeableEquity {}
  struct ABVocalContour {}
  struct ABBobtail {}
  struct ABFont {}
}

// RUN: %complete-test -hide-none -fuzz -group=none -popular="%s.popular" -unpopular="%s.unpopular" -tok=POPULAR_VS_PREFIX_1 %s -- -I %S/Inputs | %FileCheck %s -check-prefix=POPULAR_VS_PREFIX_1
func testPopularityVsPrefixMatch1() {
  let x: Outer.#^POPULAR_VS_PREFIX_1,,AB,ABT^#
}
// POPULAR_VS_PREFIX_1-LABEL: Results for filterText:  [
// POPULAR_VS_PREFIX_1-NEXT:    ABVocalContour
// POPULAR_VS_PREFIX_1-NEXT:    ABBobtail
// POPULAR_VS_PREFIX_1-NEXT:    ABFont
// POPULAR_VS_PREFIX_1-NEXT:    ABTabularMonkey
// POPULAR_VS_PREFIX_1-NEXT:    ABTextMockery
// POPULAR_VS_PREFIX_1-NEXT:    ABTradeableEquity
// POPULAR_VS_PREFIX_1: ]
// POPULAR_VS_PREFIX_1-LABEL: Results for filterText: AB [
// POPULAR_VS_PREFIX_1-NEXT:    ABVocalContour
// POPULAR_VS_PREFIX_1-NEXT:    ABBobtail
// POPULAR_VS_PREFIX_1-NEXT:    ABFont
// POPULAR_VS_PREFIX_1-NEXT:    ABTextMockery
// POPULAR_VS_PREFIX_1-NEXT:    ABTabularMonkey
// POPULAR_VS_PREFIX_1-NEXT:    ABTradeableEquity
// POPULAR_VS_PREFIX_1-NEXT: ]
// POPULAR_VS_PREFIX_1-LABEL: Results for filterText: ABT [
// POPULAR_VS_PREFIX_1-NEXT:    ABTextMockery
// POPULAR_VS_PREFIX_1-NEXT:    ABTabularMonkey
// POPULAR_VS_PREFIX_1-NEXT:    ABTradeableEquity
// POPULAR_VS_PREFIX_1-NEXT:    ABVocalContour
// POPULAR_VS_PREFIX_1-NEXT:    ABBobtail
// POPULAR_VS_PREFIX_1-NEXT:    ABFont
// POPULAR_VS_PREFIX_1-NEXT: ]
