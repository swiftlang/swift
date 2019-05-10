; This is not really a Swift source file: -*- Text -*-

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/manglings.txt > %t.input

RUN: swift-demangle -test-remangle < %t.input > %t.output
RUN: diff %t.input %t.output

// CHECK: Swift.(Mystruct in _7B40D7ED6632C2BEA2CA3BFFD57E3435)
RUN: swift-demangle -remangle-objc-rt '$ss8Mystruct33_7B40D7ED6632C2BEA2CA3BFFD57E3435LLV' | %FileCheck %s

// CHECK-GENERICEXT: Swift._ContiguousArrayStorage<(extension in Swift):Swift.FlattenSequence<StdlibCollectionUnittest.MinimalBidirectionalCollection<StdlibCollectionUnittest.MinimalBidirectionalCollection<Swift.Int>>>.Index>
RUN: swift-demangle -remangle-objc-rt '$ss23_ContiguousArrayStorageCys15FlattenSequenceVsE5IndexVy24StdlibCollectionUnittest020MinimalBidirectionalH0VyAIySiGG_GGD' | %FileCheck -check-prefix CHECK-GENERICEXT %s

