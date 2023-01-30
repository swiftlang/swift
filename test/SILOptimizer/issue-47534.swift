// RUN: %target-swift-frontend -O -emit-sil -primary-file %s %S/Inputs/extension-with-nested-type.swift -module-name test

// https://github.com/apple/swift/issues/47534

// Intentionally empty.
