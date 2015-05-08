// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-1.swiftmodule -primary-file %s %S/Inputs/conformance-multi-file-other.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/multi-file-2.swiftmodule %s -primary-file %S/Inputs/conformance-multi-file-other.swift
// RUN: %target-swift-frontend -emit-module -module-name Multi -o %t/Multi.swiftmodule %t/multi-file-*.swiftmodule
// RUN: echo "import Multi; (Sub() as BaseProto).method()" | %target-swift-frontend -I %t -parse -verify -


public class Sub : Base {}
  
