// DUPLICATE-OF: 10659-swift-printingdiagnosticconsumer-handlediagnostic.timeout.swift
// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A:A
protocol a:A
struct c:a{
let h=D
