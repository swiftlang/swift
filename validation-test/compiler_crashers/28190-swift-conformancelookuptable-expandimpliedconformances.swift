// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// DUPLICATE-OF: 10659-swift-printingdiagnosticconsumer-handlediagnostic.timeout.swift
// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
protocol A:A
protocol a:A
struct c:a{
let h=D
