// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ConcreteTypes.swift %S/Inputs/GenericTypes.swift %S/Inputs/Protocols.swift -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect > %t/typeref_decoding.txt
// RUN: diff -u %S/typeref_decoding.result.txt %t/typeref_decoding.txt
