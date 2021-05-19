// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

// TYPEATTR: Begin completions
// TYPEATTR-NOT: myIntValue
// TYPEATTR-DAG: Keyword/None:                       autoclosure[#Type Attribute#]; name=autoclosure
// TYPEATTR-DAG: Keyword/None:                       convention(swift)[#Type Attribute#]; name=convention(swift)
// TYPEATTR-DAG: Keyword/None:                       convention(block)[#Type Attribute#]; name=convention(block)
// TYPEATTR-DAG: Keyword/None:                       convention(c)[#Type Attribute#]; name=convention(c)
// TYPEATTR-DAG: Keyword/None:                       convention(thin)[#Type Attribute#]; name=convention(thin)
// TYPEATTR-DAG: Keyword/None:                       escaping[#Type Attribute#]; name=escaping
// TYPEATTR-DAG: Decl[Struct]/CurrModule:            MyStruct[#MyStruct#]; name=MyStruct
// TYPEATTR-NOT: myIntValue
// TYPEATTR: End completions

struct MyStruct {}

var myIntValue: Int = 1

func foo() -> @#^FUNC_RESULT?check=TYPEATTR^# {}

func foo(x: @#^FUNC_PARAM?check=TYPEATTR^#) {}

func foo(x: (Int, @#^CLOSURE_PARAM?check=TYPEATTR^#) -> Void) {}

func foo(x: (Int) -> @#^CLOSURE_RESULT?check=TYPEATTR^#) {}
func foo(x: Array<@#^GENERIC_ARGS?check=TYPEATTR^#>) {}

func foo<T>() where T.Something == @#^WHERE_CLAUSE?check=TYPEATTR^# {}

func test() {
    let value: @#^VARDECL?check=TYPEATTR^#
}
func test() {
    let value: @Sendable @#^MULTIPLE?check=TYPEATTR^#
}
func test() {
    typealias A = @#^TYPEALIAS?check=TYPEATTR^#
}
func test(thing: Any) {
    let _ = thing as? @#^CAST?check=TYPEATTR^#
}

