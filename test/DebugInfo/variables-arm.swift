// Run the variables.swift test for a 32 bit target.
// REQUIRES: ARM
// RUN: %swift -triple armv7-apple-ios7 %p/variables.swift -emit-llvm -g -o - | FileCheck %p/variables.swift
