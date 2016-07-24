// RUN: %target-build-swift -module-name objc_enum_errortype -emit-library %S/main.swift %S/library.swift -import-objc-header %S/objc_enum_errortype.h

// REQUIRES: objc_interop

extension AXError: Error { }
