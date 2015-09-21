// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_VOID_1 | FileCheck %s -check-prefix=RETURN_VOID_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_INT_1 | FileCheck %s -check-prefix=RETURN_INT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_INT_2 | FileCheck %s -check-prefix=RETURN_INT_2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TRY_RETURN_INT | FileCheck %s -check-prefix=RETURN_INT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TRY_RETURN_VOID | FileCheck %s -check-prefix=RETURN_VOID_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=RETURN_TR1 | FileCheck %s -check-prefix=RETURN_TR1

struct FooStruct {
  var instanceVar : Int
}

func testReturnVoid1() {
  return #^RETURN_VOID_1^#

// It is questionable if we should provide any results in a function returning
// Void.  But, the compiler allows us to put an expression of type Void here.
// A similar construct is also allowed in C, and might be used to cause a
// compiler error if the type of that expression changes to non-void.

// RETURN_VOID_1: Begin completions
// RETURN_VOID_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// RETURN_VOID_1: End completions
}

func testReturnInt1() {
  return #^RETURN_INT_1^#
// RETURN_INT_1: Begin completions
// RETURN_INT_1-DAG: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// RETURN_INT_1: End completions
}

func testReturnInt2(fooObject: FooStruct) {
  return fooObject.#^RETURN_INT_2^#
// RETURN_INT_2: Begin completions
// RETURN_INT_2-NEXT: Decl[InstanceVar]/CurrNominal: instanceVar[#Int#]{{; name=.+$}}
// RETURN_INT_2-NEXT: End completions
}

func testMisplacedTry() throws -> Int {
  try return #^TRY_RETURN_INT^#
}

func testMisplacedTryVoid() throws {
  try return #^TRY_RETURN_VOID^#
}

func testTR1() -> Int? {
	var i : Int
	var oi : Int?
	var fs : FooStruct
	return #^RETURN_TR1^#

// RETURN_TR1: Begin completions
// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: oi[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Identical]: testTR1()[#Int?#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local/TypeRelation[Convertible]: i[#Int#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testReturnInt1()[#Void#]{{; name=.+$}}
// RETURN_TR1-DAG: Decl[LocalVar]/Local:               fs[#FooStruct#]{{; name=.+$}}
}
