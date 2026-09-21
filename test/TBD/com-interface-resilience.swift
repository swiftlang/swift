// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-library-evolution -I %t -module-name Interfaces -validate-tbd-against-ir=all -emit-ir -o /dev/null %S/../IRGen/com-interface-resilience.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -module-name Interfaces -validate-tbd-against-ir=all -emit-ir -o /dev/null %S/../IRGen/com-interface-resilience.swift
