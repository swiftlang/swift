// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: %swift -parse -playground %t/main.swift -verify
var $a = 2 // expected-error {{expected numeric value following '$'}}
