// RUN: %empty-directory(%t)
// RUN: %swift-syntax-test -input-source-filename %s -parse-gen > %t/afterRoundtrip.swift
// RUN: diff -u %s %t/afterRoundtrip.swift

_ = /abc
_ = #/abc
_ = #/abc/
_ = ##/abc/#

_ #/x
/#
_ #/
x/#

_ //#
_ /x/#
