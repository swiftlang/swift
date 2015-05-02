// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL1 | FileCheck %s -check-prefix=LITERAL1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL2 | FileCheck %s -check-prefix=LITERAL2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL3 | FileCheck %s -check-prefix=LITERAL3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL4 | FileCheck %s -check-prefix=LITERAL4

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
// LITERAL3-DAG:     Decl[InstanceMethod]/CurrNominal:   getMirror()[#MirrorType#]; name=getMirror(){{$}}

{
  "swift".#^LITERAL4^#
}

// LITERAL4:         Begin completions
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   withCString({#(f): UnsafePointer<Int8> -> Result##UnsafePointer<Int8> -> Result#})[#Result#]; name=withCString(f: UnsafePointer<Int8> -> Result){{$}}

// FIXME: we should show the qualified String.Index type.
// rdar://problem/20788802
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      startIndex[#Index#]; name=startIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      endIndex[#Index#]; name=endIndex{{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   append({#(c): Character#})[#Void#]; name=append(c: Character){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   extend({#(newElements): S#})[#Void#]; name=extend(newElements: S){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   splice({#(newElements): S#}, {#atIndex: Index#})[#Void#]; name=splice(newElements: S, atIndex: Index){{$}}
// LITERAL4-DAG:     Decl[InstanceMethod]/CurrNominal:   removeAtIndex({#(i): Index#})[#Character#]; name=removeAtIndex(i: Index){{$}}
// LITERAL4-DAG:     Decl[InstanceVar]/CurrNominal:      lowercaseString[#String#]; name=lowercaseString{{$}}
