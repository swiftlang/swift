// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/ObjectiveCTypes.swift -parse-as-library -emit-module -emit-library -module-name TypesToReflect -Xfrontend -enable-reflection-metadata -Xfrontend -enable-reflection-names -o %t/libTypesToReflect.%target-dylib-extension
// RUN: %target-swift-reflection-dump -binary-filename %t/libTypesToReflect.%target-dylib-extension > %t/result.txt
// RUN: diff -u %s.%target-ptrsize.result.txt %t/result.txt
// REQUIRES: objc_interop
