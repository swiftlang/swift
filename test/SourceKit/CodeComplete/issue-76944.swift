// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//--- input.swift

//--- input2.swift
func body() {

extension InvalidProto {}

//--- input3.swift
func body() {

extension InvalidProto {}

struct MyStruct {

//--- dummy.swift

// RUN: %sourcekitd-test \
// RUN: -req=open %t/input.swift -req-opts=syntactic_only=1 -print-raw-response == \
// RUN: -req=typecontextinfo -pos=4:1 %t/input.swift -text-input %t/input2.swift -- %t/input.swift == \
// RUN: -req=complete -pos=6:1 %t/input.swift -text-input %t/input3.swift -repeat-request 2 -- %t/input.swift
