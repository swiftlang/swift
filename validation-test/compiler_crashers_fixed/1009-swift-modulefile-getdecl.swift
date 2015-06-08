// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func c<e>() -> (e -> e) -> e {
struct g<g : e, f: e where f.h = c {
b let g: c
