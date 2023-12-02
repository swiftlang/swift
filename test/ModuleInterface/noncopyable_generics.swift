// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-library-evolution -emit-module -o %t/NoncopyableGenerics.swiftmodule -emit-module-interface-path %t/NoncopyableGenerics.swiftinterface -enable-experimental-feature NoncopyableGenerics %S/Inputs/NoncopyableGenerics.swift
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -I %t %s > /dev/null

// REQUIRES: asserts

import NoncopyableGenerics
