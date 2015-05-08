#!/usr/bin/swift
let x = 42
x + x
// Check that we skip the hashbang at the beginning of the file.
// RUN: %target-parse-verify-swift

