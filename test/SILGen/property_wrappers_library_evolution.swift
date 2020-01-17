// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %S/Inputs/property_wrapper_defs.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t -enable-library-evolution
import property_wrapper_defs

// rdar://problem/55995892
// This is a crash that occurs only with -enable-library-evolution.

public enum E { case a }
struct M { @MyPublished private var e = E.a }
