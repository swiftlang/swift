// The driver and frontend together write the SARIF logs the driver plans.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// In WMO one job writes one log, with its path in a supplementary file map.
// RUN: echo "{\"\": {\"sarif-diagnostics\": \"%/t/module.sarif\"}}" > %t/wmo.json
// RUN: %swiftc_driver -disallow-use-new-driver -typecheck -wmo -module-name WMO -driver-filelist-threshold=0 -serialize-diagnostics=sarif -output-file-map %t/wmo.json %t/a.swift %t/b.swift
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-driver-wmo.sarif < %t/module.sarif

// A bridging header warning is reported only in the PCH job's log.
// RUN: echo "{\"%/t/c.swift\": {\"sarif-diagnostics\": \"%/t/c.sarif\"}}" > %t/pch.json
// RUN: %swiftc_driver -disallow-use-new-driver -emit-module -module-name PCH -emit-module-path %t/PCH.swiftmodule -serialize-diagnostics=sarif -output-file-map %t/pch.json -import-objc-header %t/header.h -pch-output-dir %t/pch %t/c.swift
// RUN: cat %t/pch/header-*.sarif | %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-driver-pch-header.sarif
// RUN: %sarif-diff %S/Inputs/expected-sarif/sarif-diagnostics-driver-pch-compile.sarif < %t/c.sarif

// REQUIRES: cplusplus_driver
// REQUIRES: swift_sarif
// REQUIRES: objc_interop

//--- a.swift
func a() { let inA = 1 }

//--- b.swift
func b() { let inB = 1 }

//--- c.swift
func c() { let inC = 1 }

//--- header.h
#warning "warning from the bridging header"
