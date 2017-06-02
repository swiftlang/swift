// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name EnumCircularity -o %t/partial1.swiftmodule -primary-file %s %S/Inputs/enum-mutual-circularity-2.swift
// RUN: %target-swift-frontend -emit-module -module-name EnumCircularity -o %t/partial2.swiftmodule %s -primary-file %S/Inputs/enum-mutual-circularity-2.swift
// RUN: %target-swift-frontend -emit-module -module-name EnumCircularity -o %t/EnumCircularity.swiftmodule %t/partial1.swiftmodule %t/partial2.swiftmodule
// RUN: %target-swift-frontend -I %t -c -o %t/client.o %S/Inputs/enum-mutual-circularity-client.swift

public enum TweedleDee {
  indirect case dum(TweedleDum)
}
