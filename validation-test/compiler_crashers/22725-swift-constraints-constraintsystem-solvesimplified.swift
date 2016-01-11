// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/19343997

func b<T>(String -> (T, String)?
func |<T>(c: String -> (T, String)?
a:String > (())) -> String -> (T?, String)?
b("" | "" | "" | "")
