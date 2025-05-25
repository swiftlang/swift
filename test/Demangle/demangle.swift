; This is not really a Swift source file: -*- Text -*-

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/manglings.txt > %t.input

%t.check: "A ---> B" ==> "B"
RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/manglings.txt > %t.check

RUN: swift-demangle -classify -ranges -no-colors < %t.input > %t.output
RUN: diff %t.check %t.output

%t.input: "A ---> B" ==> "A"
RUN: sed -ne '/--->/s/ *--->.*$//p' < %S/Inputs/manglings-with-highlighting.txt > %t.input

%t.check: "A ---> B" ==> "B"
RUN: sed -ne '/--->/s/^.*---> *//p' < %S/Inputs/manglings-with-highlighting.txt > %t.check

RUN: swift-demangle -classify < %t.input > %t.output
RUN: diff %t.check %t.output

; RUN: swift-demangle -no-colors __TtSi | %FileCheck %s -check-prefix=DOUBLE
; DOUBLE: _TtSi ---> Swift.Int
