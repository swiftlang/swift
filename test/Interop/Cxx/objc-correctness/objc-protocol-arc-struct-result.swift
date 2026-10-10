// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -o /dev/null -parse-as-library -cxx-interoperability-mode=default -import-objc-header %t/Inputs/ArcStruct.h %t/repro.swift

// REQUIRES: objc_interop

//--- Inputs/ArcStruct.h
struct ArcStruct {
  __strong id object;
};

@protocol ArcStructProvider
- (ArcStruct)context;
@end

//--- repro.swift
public func isProvider(_ object: AnyObject) -> Bool {
  object is any ArcStructProvider
}
