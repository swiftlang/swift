// BEGIN State1.swift
import Foo


// BEGIN State2.swift
import Foo
import Bar

// BEGIN State3.swift
import Bar


// BEGIN State4.swift



// BEGIN DUMMY.swift

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=3:1 -name main.swift -text-input %t/State1.swift -- main.swift -F %S/../Inputs/libIDE-mock-sdk == \
// RUN:   -req=complete -pos=3:1 -name main.swift -text-input %t/State2.swift -- main.swift -F %S/../Inputs/libIDE-mock-sdk == \
// RUN:   -req=complete -pos=3:1 -name main.swift -text-input %t/State3.swift -- main.swift -F %S/../Inputs/libIDE-mock-sdk == \
// RUN:   -req=complete -pos=3:1 -name main.swift -text-input %t/State4.swift -- main.swift -F %S/../Inputs/libIDE-mock-sdk > %t.response
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "Bar",
// RESULT-DAG: key.description: "Foo",
// RESULT-DAG: key.description: "FooHelper",
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "Foo",
// RESULT-DAG: key.description: "FooHelper",
// RESULT-DAG: key.description: "Bar",
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "Foo",
// RESULT-NOT: key.description: "FooHelper",
// RESULT-DAG: key.description: "Bar",
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "Foo",
// RESULT-NOT: key.description: "FooHelper",
// RESULT-NOT: key.description: "Bar",
// RESULT: ]
// RESULT: key.reusingastcontext: 1
