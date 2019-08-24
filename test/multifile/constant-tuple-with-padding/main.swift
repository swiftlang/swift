// RUN: %empty-directory(%t)

// RUN: %target-build-swift -O -whole-module-optimization %S/main.swift %S/Inputs/other.swift

print( g.0 )
