// RUN: %batch-code-completion

func `func with spaces`() {}
typealias `Space Alias` = `Struct With Spaces`
struct `Struct With Spaces` {}

func test01() {
    #^GLOBAL_NAME^#
    // GLOBAL_NAME-DAG: Decl[FreeFunction]/CurrModule:      `func with spaces`()[#Void#]; name=`func with spaces`()
    // GLOBAL_NAME-DAG: Decl[TypeAlias]/CurrModule:         `Space Alias`[#`Struct With Spaces`#]; name=`Space Alias`
    // GLOBAL_NAME-DAG: Decl[Struct]/CurrModule:            `Struct With Spaces`[#`Struct With Spaces`#]; name=`Struct With Spaces`
}

struct S {
  var `name with spaces :)`: Int
  func `some.method`(`argument label!`: Int) {}
  func f(x: `Struct With Spaces`) {}
}

func test02(_ x: S) {
  x.#^MEMBER_NAME^#
  // MEMBER_NAME-DAG: Decl[InstanceVar]/CurrNominal:      `name with spaces :)`[#Int#]; name=`name with spaces :)`
  // MEMBER_NAME-DAG: Decl[InstanceMethod]/CurrNominal:   `some.method`({#`argument label!`: Int#})[#Void#]; name=`some.method`(`argument label!`:)
  // MEMBER_NAME-DAG: Decl[InstanceMethod]/CurrNominal:   f({#x: `Struct With Spaces`#})[#Void#]; name=f(x:)
}

struct S2<`Generic Param`> {
  var x: #^TYPE_NAME^#
  // TYPE_NAME-DAG: Decl[GenericTypeParam]/Local:       `Generic Param`[#Generic Param#]; name=`Generic Param`
  // TYPE_NAME-DAG: Keyword[Self]/CurrNominal:          Self[#S2<Generic Param>#]; name=Self
}

precedencegroup `Spacedence Group` {}
precedencegroup AnotherOne {
  higherThan: #^PREC_NAME^#
  // PREC_NAME: Decl[PrecedenceGroup]/CurrModule:   `Spacedence Group`; name=`Spacedence Group`
}
