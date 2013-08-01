#!/usr/bin/swift
class Foo {}
// Check that we diagnose and skip the hashbang in the REPL.
// RUN: %swift -repl < %s 2>&1 | FileCheck %s
// CHECK: error: hashbang line is allowed only in the main file

