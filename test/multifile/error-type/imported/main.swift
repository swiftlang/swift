// RUN: %target-build-swift -module-name objc_enum_errortype -emit-library %S/main.swift %S/Inputs/library.swift -import-objc-header %S/Inputs/objc_enum_errortype.h

// REQUIRES: objc_interop

extension AXError: Error { }
