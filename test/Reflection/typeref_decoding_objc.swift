// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ObjectiveCTypes.swift -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -o %t/libTypesToReflect
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect > %t/typeref_decoding.txt
// RUN: diff -u %S/typeref_decoding_objc.result.txt %t/typeref_decoding.txt
// REQUIRES: objc_interop
