// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL1 | FileCheck %s -check-prefix=LITERAL1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL2 | FileCheck %s -check-prefix=LITERAL2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL3 | FileCheck %s -check-prefix=LITERAL3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=LITERAL4 | FileCheck %s -check-prefix=LITERAL4
{
  1.#^LITERAL1^#
}
// LITERAL1:          Begin completions, 13 items
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      value[#Int{{64|32}}#]; name=value{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      bigEndian[#Int#]; name=bigEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      littleEndian[#Int#]; name=littleEndian{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      byteSwapped[#Int#]; name=byteSwapped{{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   toIntMax()[#IntMax#]; name=toIntMax(){{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      description[#String#]; name=description{{$}}
// LITERAL1-DAG:      Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   successor()[#Int#]; name=successor(){{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   predecessor()[#Int#]; name=predecessor(){{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   distanceTo({#(other): Int#})[#Distance#]; name=distanceTo(other: Int){{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   advancedBy({#(amount): Distance#})[#Int#]; name=advancedBy(amount: Distance){{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   getMirror()[#MirrorType#]; name=getMirror(){{$}}
// LITERAL1-DAG:      Decl[InstanceMethod]/CurrNominal:   encode()[#[Word]#]; name=encode(){{$}}
// LITERAL1-DAG:      End completions

{
  1.1.#^LITERAL2^#
}
// LITERAL2:          Begin completions, 15 items
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      description[#String#]; name=description{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isSignMinus[#Bool#]; name=isSignMinus{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isNormal[#Bool#]; name=isNormal{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isFinite[#Bool#]; name=isFinite{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isZero[#Bool#]; name=isZero{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isSubnormal[#Bool#]; name=isSubnormal{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isInfinite[#Bool#]; name=isInfinite{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isNaN[#Bool#]; name=isNaN{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      isSignaling[#Bool#]; name=isSignaling{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      floatingPointClass[#FloatingPointClassification#]; name=floatingPointClass{{$}}
// LITERAL2-NEXT:     Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}
// LITERAL2-NEXT:     Decl[InstanceMethod]/CurrNominal:   distanceTo({#(other): Double#})[#Double#]; name=distanceTo(other: Double){{$}}
// LITERAL2-NEXT:     Decl[InstanceMethod]/CurrNominal:   advancedBy({#(amount): Double#})[#Double#]; name=advancedBy(amount: Double){{$}}
// LITERAL2-NEXT:     Decl[InstanceMethod]/CurrNominal:   getMirror()[#MirrorType#]; name=getMirror(){{$}}
// LITERAL2-NEXT:     Decl[InstanceMethod]/CurrNominal:   encode()[#[Word]#]; name=encode(){{$}}
// LITERAL2-NEXT:     End completions
{
  true.#^LITERAL3^#
}
// LITERAL3:          Begin completions, 4 items
// LITERAL3-NEXT:     Decl[InstanceVar]/CurrNominal:      boolValue[#Bool#]; name=boolValue{{$}}
// LITERAL3-NEXT:     Decl[InstanceVar]/CurrNominal:      description[#String#]; name=description{{$}}
// LITERAL3-NEXT:     Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}
// LITERAL3-NEXT:     Decl[InstanceMethod]/CurrNominal:   getMirror()[#MirrorType#]; name=getMirror(){{$}}
// LITERAL3-NEXT:     End completions
{
  "swift".#^LITERAL4^#
}

// LITERAL4:          Begin completions, 31 items
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   withCString({#(f): UnsafePointer<Int8> -> Result##UnsafePointer<Int8> -> Result#})[#Result#]; name=withCString(f: UnsafePointer<Int8> -> Result){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   getMirror()[#MirrorType#]; name=getMirror(){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   write({#(other): String#})[#Void#]; name=write(other: String){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   writeTo({#&(target): Target#})[#Void#]; name=writeTo(&target: Target){{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      debugDescription[#String#]; name=debugDescription{{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   extend({#(other): String#})[#Void#]; name=extend(other: String){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   append({#(x): UnicodeScalar#})[#Void#]; name=append(x: UnicodeScalar){{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      startIndex[#String.Index#]; name=startIndex{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      endIndex[#String.Index#]; name=endIndex{{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   generate()[#IndexingGenerator<String>#]; name=generate(){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   reserveCapacity({#(n): Int#})[#Void#]; name=reserveCapacity(n: Int){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   append({#(c): Character#})[#Void#]; name=append(c: Character){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   extend({#(newElements): S#})[#Void#]; name=extend(newElements: S){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   join({#(elements): S#})[#String#]; name=join(elements: S){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   replaceRange({#(subRange): Range<String.Index>#}, {#with: C#})[#Void#]; name=replaceRange(subRange: Range<String.Index>, with: C){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   insert({#(newElement): Character#}, {#atIndex: String.Index#})[#Void#]; name=insert(newElement: Character, atIndex: String.Index){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   splice({#(newElements): S#}, {#atIndex: String.Index#})[#Void#]; name=splice(newElements: S, atIndex: String.Index){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   removeAtIndex({#(i): String.Index#})[#Character#]; name=removeAtIndex(i: String.Index){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   removeRange({#(subRange): Range<String.Index>#})[#Void#]; name=removeRange(subRange: Range<String.Index>){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   removeAll({#keepCapacity: Bool#})[#Void#]; name=removeAll(keepCapacity: Bool){{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      lowercaseString[#String#]; name=lowercaseString{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      uppercaseString[#String#]; name=uppercaseString{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      isEmpty[#Bool#]; name=isEmpty{{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   hasPrefix({#(prefix): String#})[#Bool#]; name=hasPrefix(prefix: String){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   hasSuffix({#(suffix): String#})[#Bool#]; name=hasSuffix(suffix: String){{$}}
// LITERAL4-NEXT:     Decl[InstanceMethod]/CurrNominal:   toInt()[#Int?#]; name=toInt(){{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      utf16[#String.UTF16View#]; name=utf16{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      utf8[#String.UTF8View#]; name=utf8{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      nulTerminatedUTF8[#ContiguousArray<CodeUnit>#]; name=nulTerminatedUTF8{{$}}
// LITERAL4-NEXT:     Decl[InstanceVar]/CurrNominal:      unicodeScalars[#String.UnicodeScalarView#]; name=unicodeScalars{{$}}
// LITERAL4-NEXT:     End completions
