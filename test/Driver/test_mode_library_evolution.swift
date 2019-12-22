// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution %s 2>&1 | %FileCheck --check-prefix NO_O %s
// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution -Osize %s 2>&1 | %FileCheck --check-prefix O_SIZE %s
// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution -Onone %s 2>&1 | %FileCheck --check-prefix O_NONE %s
// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution -O %s 2>&1 | %FileCheck --check-prefix O %s
// RUN: %swiftc_driver -driver-print-jobs -enable-testing -enable-library-evolution -Ounchecked %s 2>&1 | %FileCheck --check-prefix O_UNCHECKED %s


// NO_O: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability; consider disabling the '-enable-library-evolution' flag
// O_SIZE: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability; consider disabling the '-enable-testing' flag
// O_NONE: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability; consider disabling the '-enable-library-evolution' flag
// O: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability; consider disabling the '-enable-testing' flag
// O_UNCHECKED: warning: specifying '-enable-library-evolution' and '-enable-testing' together will cause program instability; consider disabling the '-enable-testing' flag

@testable import Module
