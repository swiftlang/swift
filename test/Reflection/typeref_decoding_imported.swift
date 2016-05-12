// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ImportedTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -o %t/libTypesToReflect.%target-dylib-extension -I %S/Inputs
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension > %t/typeref_decoding_imported.txt
// RUN: diff -u %S/typeref_decoding_imported.result.txt %t/typeref_decoding_imported.txt
