protocol MyProto {}
enum MyEnum : MyProto {
    case foo
    case bar(Int)

    static func staticReturnVoid() {}
    static func staticReturnMyEnum() -> MyEnum { return .foo }
    func intanceReturnVoid() {}
    func intanceReturnMyEnum() -> MyEnum { return .foo }
}

func testIdenticalContext() -> MyEnum {
  return MyEnum.
}

func testConvertibleContext() -> MyProto {
  return MyEnum.
}

func testBool() -> Bool {
  return 
}
func testOptionalInt() -> Int? {
  return 
}
func testVoid() -> Void {
  return 
}
func testUnknown() {

}

// RUN: %sourcekitd-test -req=complete -pos=13:17 %s -- %s > %t.identical.response
// RUN: diff --strip-trailing-cr -u %s.identical.response %t.identical.response

// RUN: %sourcekitd-test -req=complete -pos=17:17 %s -- %s > %t.convertible.response
// RUN: diff --strip-trailing-cr -u %s.convertible.response %t.convertible.response

// RUN: %empty-directory(%t/cache)
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=21:10 %s -- %s | %FileCheck %s --check-prefix=BOOLCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=24:10 %s -- %s | %FileCheck %s --check-prefix=OPTIONALCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=27:10 %s -- %s | %FileCheck %s --check-prefix=VOIDCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=27:10 %s -- %s | %FileCheck %s --check-prefix=UNKNOWNCONTEXT

// BOOLCONTEXT-LABEL: key.name: "false",
// BOOLCONTEXT-NOT:   key.name:
// BOOLCONTEXT:       key.typename: "Bool",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.identical,
// BOOLCONTEXT-LABEL: key.name: "Int",
// BOOLCONTEXT-NOT:   key.name:
// BOOLCONTEXT:       key.typename: "Int",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// BOOLCONTEXT-LABEL: key.name: "nil",
// BOOLCONTEXT-NOT:   key.name:
// BOOLCONTEXT:       key.typename: "",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// BOOLCONTEXT-LABEL: key.name: "true",
// BOOLCONTEXT-NOT:   key.name:
// BOOLCONTEXT:       key.typename: "Bool",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.identical,

// OPTIONALCONTEXT-LABEL: key.name: "false",
// OPTIONALCONTEXT-NOT:   key.name:
// OPTIONALCONTEXT:       key.typename: "Bool",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// OPTIONALCONTEXT-LABEL: key.name: "Int",
// OPTIONALCONTEXT-NOT:   key.name:
// OPTIONALCONTEXT:       key.typename: "Int",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// OPTIONALCONTEXT-LABEL: key.name: "nil",
// OPTIONALCONTEXT-NOT:   key.name:
// OPTIONALCONTEXT:       key.typename: "Int?",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.identical,
// OPTIONALCONTEXT-LABEL: key.name: "true",
// OPTIONALCONTEXT-NOT:   key.name:
// OPTIONALCONTEXT:       key.typename: "Bool",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,

// VOIDCONTEXT-LABEL: key.name: "false",
// VOIDCONTEXT-NOT:   key.name:
// VOIDCONTEXT:       key.typename: "Bool",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// VOIDCONTEXT-LABEL: key.name: "Int",
// VOIDCONTEXT-NOT:   key.name:
// VOIDCONTEXT:       key.typename: "Int",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// VOIDCONTEXT-LABEL: key.name: "nil",
// VOIDCONTEXT-NOT:   key.name:
// VOIDCONTEXT:       key.typename: "",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// VOIDCONTEXT-LABEL: key.name: "true",
// VOIDCONTEXT-NOT:   key.name:
// VOIDCONTEXT:       key.typename: "Bool",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,

// UNKNOWNCONTEXT-LABEL: key.name: "false",
// UNKNOWNCONTEXT-NOT:   key.name:
// UNKNOWNCONTEXT:       key.typename: "Bool",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: key.name: "Int",
// UNKNOWNCONTEXT-NOT:   key.name:
// UNKNOWNCONTEXT:       key.typename: "Int",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: key.name: "nil",
// UNKNOWNCONTEXT-NOT:   key.name:
// UNKNOWNCONTEXT:       key.typename: "",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: key.name: "true",
// UNKNOWNCONTEXT-NOT:   key.name:
// UNKNOWNCONTEXT:       key.typename: "Bool",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
