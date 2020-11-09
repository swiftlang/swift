// RUN: %empty-directory(%t)
// RUN: touch %t/file-01.swift %t/file-02.swift %t/file-03.swift

// RUN: %swiftc_driver -driver-print-jobs -driver-skip-execution -j 3 -emit-module -module-name foo -emit-module-interface %t/file-01.swift %t/file-02.swift %t/file-03.swift -verify-emitted-module-interface -enable-library-evolution >%t/evolution.txt

// RUN: %swiftc_driver -driver-print-jobs -driver-skip-execution -j 3 -emit-module -module-name foo -emit-module-interface %t/file-01.swift %t/file-02.swift %t/file-03.swift -verify-emitted-module-interface >%t/no-evolution.txt

// RUN: %FileCheck %s --check-prefix=CHECK_EVOLUTION <%t/evolution.txt
// RUN: %FileCheck %s --check-prefix=CHECK_NO_EVOLUTION <%t/no-evolution.txt

// CHECK_EVOLUTION: -typecheck-module-from-interface
// CHECK_NO_EVOLUTION-NOT: -typecheck-module-from-interface
