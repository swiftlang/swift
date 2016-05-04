// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ConcreteTypes.swift %S/Inputs/GenericTypes.swift %S/Inputs/Protocols.swift %S/Inputs/Extensions.swift %S/Inputs/Closures.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension > %t/typeref_decoding.txt
// RUN: diff -u %S/typeref_decoding.result.txt %t/typeref_decoding.txt
