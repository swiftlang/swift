// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL1 | %FileCheck %s -check-prefix=LITERAL1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL2 | %FileCheck %s -check-prefix=LITERAL2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL3 | %FileCheck %s -check-prefix=LITERAL3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL4 | %FileCheck %s -check-prefix=LITERAL4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL5 | %FileCheck %s -check-prefix=LITERAL5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL6 | %FileCheck %s -check-prefix=LITERAL6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL7 | %FileCheck %s -check-prefix=LITERAL7

{
  1.#^LITERAL1^#
}
// LITERAL1:          Begin completions
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      bigEndian[#Int#]; name=bigEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      littleEndian[#Int#]; name=littleEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      byteSwapped[#Int#]; name=byteSwapped{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      nonzeroBitCount[#Int#]; name=nonzeroBitCount{{$}}

{
  1.1.#^LITERAL2^#
}
// LITERAL2:         Begin completions
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isNormal[#Bool#]; name=isNormal{{$}}
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isFinite[#Bool#]; name=isFinite{{$}}
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isZero[#Bool#]; name=isZero{{$}}
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isSubnormal[#Bool#]; name=isSubnormal{{$}}
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isInfinite[#Bool#]; name=isInfinite{{$}}
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isNaN[#Bool#]; name=isNaN{{$}}

{
  true.#^LITERAL3^#
}
// LITERAL3:         Begin completions
// LITERAL3-DAG:     Decl[InstanceVar]/CurrNominal:      description[#String#]; name=description{{$}}
// LITERAL3-DAG:     Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}

{
  "swift".#^LITERAL4^#
}

// LITERAL4:         Begin completions
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   withCString({#(body): (UnsafePointer<Int8>) throws -> Result##(UnsafePointer<Int8>) throws -> Result#})[' rethrows'][#Result#]; name=withCString(body: (UnsafePointer<Int8>) throws -> Result) rethrows{{$}}

// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      startIndex[#String.Index#]; name=startIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      endIndex[#String.Index#]; name=endIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   append({#(c): Character#})[#Void#]; name=append(c: Character){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   append({#contentsOf: Sequence#})[#Void#]; name=append(contentsOf: Sequence){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   insert({#contentsOf: Collection#}, {#at: String.Index#})[#Void#]; name=insert(contentsOf: Collection, at: String.Index){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   remove({#at: String.Index#})[#Character#]; name=remove(at: String.Index){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:      lowercased()[#String#]; name=lowercased(){{$}}

func giveMeAString() -> Int {
  // rdar://22637799
  return "Here's a string".#^LITERAL5^# // try .characters.count here
}

// LITERAL5-DAG:     Decl[InstanceVar]/CurrNominal:      characters[#String.CharacterView#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceVar]/CurrNominal:      endIndex[#String.Index#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: reserveCapacity({#(n): Int#})[#Void#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: append({#(c): Character#})[#Void#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: append({#contentsOf: Sequence#})[#Void#]{{; name=.+$}}

struct MyColor: _ExpressibleByColorLiteral {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) { red = colorLiteralRed }
  var red: Float
}
public typealias _ColorLiteralType = MyColor
func testColor11() {
  let y: MyColor
  y = #colorLiteral(red: 1.0, green: 0.1, blue: 0.5, alpha: 1.0).#^LITERAL6^#
}
// LITERAL6: Decl[InstanceVar]/CurrNominal:      red[#Float#]; name=red
func testColor12() {
  let y: MyColor
  y = #colorLiteral(red: 1.0, green: 0.1, blue: 0.5, alpha: 1.0) #^LITERAL7^#
}
// LITERAL7: Decl[InstanceVar]/CurrNominal:      .red[#Float#]; name=red
