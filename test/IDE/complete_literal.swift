// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL1 | FileCheck %s -check-prefix=LITERAL1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL2 | FileCheck %s -check-prefix=LITERAL2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL3 | FileCheck %s -check-prefix=LITERAL3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL4 | FileCheck %s -check-prefix=LITERAL4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL5 | FileCheck %s -check-prefix=LITERAL5

{
  1.#^LITERAL1^#
}
// LITERAL1:          Begin completions
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      bigEndian[#Int#]; name=bigEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      littleEndian[#Int#]; name=littleEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      byteSwapped[#Int#]; name=byteSwapped{{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   toIntMax()[#IntMax#]; name=toIntMax(){{$}}

{
  1.1.#^LITERAL2^#
}
// LITERAL2:         Begin completions
// LITERAL2-DAG:     Decl[InstanceVar]/CurrNominal:      isSignMinus[#Bool#]; name=isSignMinus{{$}}
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
// LITERAL3-DAG:     Decl[InstanceVar]/CurrNominal:      boolValue[#Bool#]; name=boolValue{{$}}
// LITERAL3-DAG:     Decl[InstanceVar]/CurrNominal:      description[#String#]; name=description{{$}}
// LITERAL3-DAG:     Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}

{
  "swift".#^LITERAL4^#
}

// LITERAL4:         Begin completions
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   withCString({#(f): (UnsafePointer<Int8>) throws -> Result##(UnsafePointer<Int8>) throws -> Result#})[' rethrows'][#Result#]; name=withCString(f: (UnsafePointer<Int8>) throws -> Result) rethrows{{$}}

// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      startIndex[#Index#]; name=startIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      endIndex[#Index#]; name=endIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   append({#(c): Character#})[#Void#]; name=append(c: Character){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   append({#contentsOf: S#})[#Void#]; name=append(contentsOf: S){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   insert({#contentsOf: S#}, {#at: Index#})[#Void#]; name=insert(contentsOf: S, at: Index){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   remove({#at: Index#})[#Character#]; name=remove(at: Index){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:      lowercased()[#String#]; name=lowercased(){{$}}

func giveMeAString() -> Int {
  // rdar://22637799
  return "Here's a string".#^LITERAL5^# // try .characters.count here
}

// LITERAL5-DAG:     Decl[InstanceVar]/CurrNominal:      characters[#String.CharacterView#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceVar]/CurrNominal:      endIndex[#Index#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: reserveCapacity({#(n): Int#})[#Void#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: append({#(c): Character#})[#Void#]{{; name=.+$}}
// LITERAL5-DAG:     Decl[InstanceMethod]/CurrNominal/NotRecommended/TypeRelation[Invalid]: append({#contentsOf: S#})[#Void#]{{; name=.+$}}
