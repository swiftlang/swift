#!/bin/sh

/Users/rwidmann/SwiftInternal/build/Ninja-Release/swift-macosx-arm64/bin/swift-frontend -sdk $(xcrun --show-sdk-path) -compile-module-from-interface -module-name Bottom -o Bottom.swiftmodule Bottom.swiftinterface -verify 
