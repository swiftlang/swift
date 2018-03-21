// RUN: %target-swift-frontend -c -primary-file %s %S/Inputs/context_descriptor_cross_file_reference_2.swift -verify

extension X {
  struct Y {}
}

func force_metadata() {
  print(X.Y())
}
