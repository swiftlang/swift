// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-module -o %t/Module.swiftmodule %s %S/Inputs/multi-file-subclass-generic-instantiation-extension.swift

// https://bugs.swift.org/browse/SR-11495

class Superclass<T> {}
class Subclass: Superclass<Subclass.MemberTypeFromOtherFile> {}
