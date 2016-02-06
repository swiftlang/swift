// DUPLICATE-OF: 01766-swift-typechecker-validatedecl.swift
// RUN: not --crash %target-swift-frontend %s -parse
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol e:A{protocol e:A class A}protocol A{enum A}protocol B:e
