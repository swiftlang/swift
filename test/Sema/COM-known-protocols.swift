// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -com-interop-model=microsoft -module-name COM -emit-module-path %t/COM.swiftmodule -emit-ir -o %t/COM.ll %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -com-interop-model=microsoft -I %t
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-com-interop -I %t

import COM

func interface<T: COMInterface>(_: T) {}

#if $_MicrosoftCOM

func activation<T: COMActivatable>(_: T) {}

final class Aggregated: COMAggregatable {
  var controller: (any IUnknown)? { nil }
}

func controller<T: COMAggregatable>(of aggregated: T) -> (any IUnknown)? {
  aggregated.controller
}

#endif
