// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/Inputs/OtherActors.swift -target %target-swift-5.1-abi-triple

// RUN: %target-swift-frontend  -I %t %s -emit-sil -o /dev/null -verify  -parse-as-library

// REQUIRES: concurrency

@preconcurrency import OtherActors
