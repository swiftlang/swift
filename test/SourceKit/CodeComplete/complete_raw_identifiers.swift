// RUN: %complete-test -raw -tok=GLOBAL_NAME %s | %FileCheck %s -check-prefix=GLOBAL_NAME
// RUN: %complete-test -raw -tok=MEMBER_NAME %s | %FileCheck %s -check-prefix=MEMBER_NAME
// RUN: %complete-test -raw -tok=TYPE_NAME %s | %FileCheck %s -check-prefix=TYPE_NAME
// RUN: %complete-test -raw -tok=PREC_NAME %s | %FileCheck %s -check-prefix=PREC_NAME

func `func with spaces`() {}
// GLOBAL_NAME: key.name: "`func with spaces`()",
// GLOBAL_NAME: key.description: "`func with spaces`()",
// GLOBAL_NAME: key.sourcetext: "`func with spaces`()"

typealias `Space Alias` = `Struct With Spaces`
// GLOBAL_NAME: key.name: "`Space Alias`",
// GLOBAL_NAME: key.description: "`Space Alias`",
// GLOBAL_NAME: key.sourcetext: "`Space Alias`"

struct `Struct With Spaces` {}
// GLOBAL_NAME: key.name: "`Struct With Spaces`",
// GLOBAL_NAME: key.description: "`Struct With Spaces`",
// GLOBAL_NAME: key.typename: "`Struct With Spaces`",
// GLOBAL_NAME: key.sourcetext: "`Struct With Spaces`"

func test01() {
    #^GLOBAL_NAME^#
}

struct S {
  var `name with spaces :)`: Int
  // MEMBER_NAME: key.name: "`name with spaces :)`",
  // MEMBER_NAME: key.description: "`name with spaces :)`",
  // MEMBER_NAME: key.sourcetext: "`name with spaces :)`"

  func `some.method`(`argument label!`: Int) {}
  // MEMBER_NAME: key.name: "`some.method`(`argument label!`:)",
  // MEMBER_NAME: key.description: "`some.method`(`argument label!`: Int)",
  // MEMBER_NAME: key.sourcetext: "`some.method`(`argument label!`: <#T##Int#>)"

  func f(x: `Struct With Spaces`) {}
  // MEMBER_NAME: key.name: "f(x:)",
  // MEMBER_NAME: key.description: "f(x: `Struct With Spaces`)",
  // MEMBER_NAME: key.sourcetext: "f(x: <#T##`Struct With Spaces`#>)"
}

func test02(_ x: S) {
  x.#^MEMBER_NAME^#
}

struct S2<`Generic Param`> {
  var x: #^TYPE_NAME^#
  // TYPE_NAME: key.name: "`Generic Param`",
  // TYPE_NAME: key.description: "`Generic Param`",
  // TYPE_NAME: key.typename: "Generic Param",
  // TYPE_NAME: key.sourcetext: "`Generic Param`"

  // TYPE_NAME: key.name: "Self",
  // TYPE_NAME: key.typename: "S2<Generic Param>",
}

precedencegroup `Spacedence Group` {}
precedencegroup AnotherOne {
  higherThan: #^PREC_NAME^#
  // PREC_NAME: key.name: "`Spacedence Group`",
  // PREC_NAME: key.description: "`Spacedence Group`",
  // PREC_NAME: key.sourcetext: "`Spacedence Group`"
}
