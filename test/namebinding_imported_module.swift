// RUN: %swift %s -parse-as-library -verify

// This file primarily exists to be imported by namebinding_in_library.swift.

func over1(x : UInt32) {}
func over2(x : UInt32) {}
typealias over3 : UInt32
func over4(x : UInt32) {}
