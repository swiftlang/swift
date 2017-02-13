// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -emit-module -o %t/SR3915.swiftmodule %s %S/Inputs/SR3915-other.swift

public enum A {}

public extension A {
  public enum B {}
}
