#!/usr/bin/swift
println(42)
// Check that we skip the hashbang at the beginning of the file.
// RUN: %swift -parse %s -verify

