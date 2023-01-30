// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-module -o %t/Module.swiftmodule %s %S/Inputs/multi-file-subclass-generic-instantiation-extension.swift

// https://github.com/apple/swift/issues/53896

class Superclass<T> {}
class Subclass: Superclass<Subclass.MemberTypeFromOtherFile> {}
