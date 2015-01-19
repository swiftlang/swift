; This is not really a Swift source file: -*- Text -*-

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/manglings.txt > %t.input

RUN: swift-demangle -test-remangle < %t.input > %t.output
RUN: diff %t.input %t.output
