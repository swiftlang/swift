protocol MyProto {}
enum MyEnum : MyProto {
    case foo
    case bar(Int)

    static func staticReturnVoid() {}
    static func staticReturnMyEnum() -> MyEnum { return .foo }
    func instanceReturnVoid() {}
    func instanceReturnMyEnum() -> MyEnum { return .foo }
    @available(*, deprecated)
    func instanceReturnVoidDeprecated() {}
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
  let a = 1

}

// RUN: %sourcekitd-test -req=complete -pos=15:17 %s -- %s > %t.identical.response
// RUN: %diff -u %s.identical.response %t.identical.response

// RUN: %sourcekitd-test -req=complete -pos=19:17 %s -- %s > %t.convertible.response
// RUN: %diff -u %s.convertible.response %t.convertible.response

// RUN: %empty-directory(%t/cache)
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=23:10 %s -- %s | %FileCheck %s --check-prefix=BOOLCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=26:10 %s -- %s | %FileCheck %s --check-prefix=OPTIONALCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=29:10 %s -- %s | %FileCheck %s --check-prefix=VOIDCONTEXT
// RUN: %sourcekitd-test -req=complete.cache.ondisk -cache-path %t/cache == -req=complete -pos=33:1 %s -- %s | %FileCheck %s --check-prefix=UNKNOWNCONTEXT

// BOOLCONTEXT-LABEL: key.name: "false",
// BOOLCONTEXT:       key.typename: "Bool",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.convertible,
// BOOLCONTEXT-LABEL: }
// BOOLCONTEXT-LABEL: key.name: "Int",
// BOOLCONTEXT:       key.typename: "Int",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// BOOLCONTEXT-LABEL: }
// BOOLCONTEXT-LABEL: key.name: "nil",
// BOOLCONTEXT:       key.typename: "",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// BOOLCONTEXT-LABEL: }
// BOOLCONTEXT-LABEL: key.name: "true",
// BOOLCONTEXT:       key.typename: "Bool",
// BOOLCONTEXT:       key.typerelation: source.codecompletion.typerelation.convertible,
// BOOLCONTEXT-LABEL: }

// OPTIONALCONTEXT-LABEL: key.name: "false",
// OPTIONALCONTEXT:       key.typename: "Bool",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// OPTIONALCONTEXT-LABEL: }
// OPTIONALCONTEXT-LABEL: key.name: "Int",
// OPTIONALCONTEXT:       key.typename: "Int",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.convertible,
// OPTIONALCONTEXT-LABEL: }
// OPTIONALCONTEXT-LABEL: key.name: "nil",
// OPTIONALCONTEXT:       key.typename: "Int?",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.convertible,
// OPTIONALCONTEXT-LABEL: }
// OPTIONALCONTEXT-LABEL: key.name: "true",
// OPTIONALCONTEXT:       key.typename: "Bool",
// OPTIONALCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,

// VOIDCONTEXT-LABEL: key.name: "false",
// VOIDCONTEXT:       key.typename: "Bool",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// VOIDCONTEXT-LABEL: }
// VOIDCONTEXT-LABEL: key.name: "Int",
// VOIDCONTEXT:       key.typename: "Int",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// VOIDCONTEXT-LABEL: }
// VOIDCONTEXT-LABEL: key.name: "nil",
// VOIDCONTEXT:       key.typename: "",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// VOIDCONTEXT-LABEL: }
// VOIDCONTEXT-LABEL: key.name: "true",
// VOIDCONTEXT:       key.typename: "Bool",
// VOIDCONTEXT:       key.typerelation: source.codecompletion.typerelation.unrelated,
// VOIDCONTEXT-LABEL: }

// UNKNOWNCONTEXT-LABEL: key.name: "false",
// UNKNOWNCONTEXT:       key.typename: "Bool",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: }
// UNKNOWNCONTEXT-LABEL: key.name: "Int",
// UNKNOWNCONTEXT:       key.typename: "Int",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: }
// UNKNOWNCONTEXT-LABEL: key.name: "nil",
// UNKNOWNCONTEXT:       key.typename: "",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: }
// UNKNOWNCONTEXT-LABEL: key.name: "true",
// UNKNOWNCONTEXT:       key.typename: "Bool",
// UNKNOWNCONTEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// UNKNOWNCONTEXT-LABEL: }
