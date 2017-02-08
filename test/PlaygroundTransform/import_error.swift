// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -playground %t/main.swift -verify

var $a = 2 // expected-error {{expected numeric value following '$'}}
