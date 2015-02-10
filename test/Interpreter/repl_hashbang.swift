#!/usr/bin/swift
class Foo {}
// Check that we diagnose and skip the hashbang in the REPL.
// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: swift_repl

// CHECK: error: hashbang line is allowed only in the main file

