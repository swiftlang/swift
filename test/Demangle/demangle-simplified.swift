; This is not really a Swift source file: -*- Text -*-

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/simplified-manglings.txt > %t.input

%t.check: "A ---> B" ==> "B"
RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/simplified-manglings.txt > %t.check

RUN: swift-demangle -simplified < %t.input > %t.output
RUN: diff %t.check %t.output
