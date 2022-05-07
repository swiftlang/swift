// RUN: %empty-directory(%t)
// RUN: touch %t/main.swift
// RUN: touch %t/second.swift

// 1. Incremental mode (single primary file)
//
// A) Run without -index-unit-output-path:
//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_without \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o
//
//    Check the unit output is based on -o
//    RUN: c-index-test core -print-unit %t/idx_without | %FileCheck -check-prefixes=INDEX_WITHOUT %s
//
//    INDEX_WITHOUT:      main_out.o-{{.*}}
//    INDEX_WITHOUT-NEXT: --------
//    INDEX_WITHOUT:      out-file: {{.*}}main_out.o

//
// B) Run with a -index-unit-output-path different from the -o value:
//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_with \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o -index-unit-output-path %t/custom_output.o
//
//    Run the same command as above again but with different -o value
//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_with \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/different.o -index-unit-output-path %t/custom_output.o
//
//    Check the unit output is based on -index-unit-output-path:
//    RUN: c-index-test core -print-unit %t/idx_with | %FileCheck -check-prefixes=INDEX_WITH %s
//
//    Check no units were produced based on the -o values
//    RUN: c-index-test core -print-unit %t/idx_with | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s
//
//    INDEX_WITH:      custom_output.o-{{.*}}
//    INDEX_WITH-NEXT: --------
//    INDEX_WITH:      out-file: {{.*}}custom_output.o
//
//    INDEX_WITH_NEGATIVE-NOT: main_out.o
//    INDEX_WITH_NEGATIVE-NOT: different.o
//
//    Run with a different -index-unit-output-path
//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_with \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/different.o -index-unit-output-path %t/custom_output_two.o
//
//    Check it resulted in a second unit being added to the store
//    RUN: c-index-test core -print-unit %t/idx_with | %FileCheck -check-prefixes=INDEX_WITH,INDEX_WITH_SECOND %s
//
//    INDEX_WITH_SECOND: custom_output_two.o-{{.*}}
//    INDEX_WITH_SECOND: --------
//    INDEX_WITH_SECOND: out-file: {{.*}}custom_output_two.o
//
// C) Do the above again but with a fresh index store and using file lists.
//
//    RUN: echo '%t/main.swift' > %t/filelist
//    RUN: echo '%t/second.swift' >> %t/filelist
//
//    RUN: echo '%t/output.o' > %t/outputlist
//
//    RUN: echo '"%t/custom_output.o' > %t/index-outputlist
//
//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_with_list \
//    RUN:     -primary-file %t/main.swift -filelist %t/filelist \
//    RUN:     -output-filelist %t/outputlist -index-unit-output-path-filelist %t/index-outputlist

//    RUN: echo '%t/different.o' > %t/outputlist

//    RUN: %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_with_list \
//    RUN:     -primary-file %t/main.swift -filelist %t/filelist \
//    RUN:     -output-filelist %t/outputlist -index-unit-output-path-filelist %t/index-outputlist
//
//    RUN: c-index-test core -print-unit %t/idx_with_list | %FileCheck -check-prefixes=INDEX_WITH %s
//    RUN: c-index-test core -print-unit %t/idx_with_list | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s


// 2. Batch mode (multiple primary files)
//
// A) Run without -index-unit-output-path:
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_batch_without \
//    RUN:     -primary-file %t/main.swift -primary-file %t/second.swift \
//    RUN:     -o %t/main_out.o -o second_out.o
//
//    Check the unit output is based on the -o values
//    RUN: c-index-test core -print-unit %t/idx_batch_without | %FileCheck -check-prefixes=INDEX_BATCH_WITHOUT %s
//
//    INDEX_BATCH_WITHOUT:      main_out.o-{{.*}}
//    INDEX_BATCH_WITHOUT-NEXT: --------
//    INDEX_BATCH_WITHOUT:      out-file: {{.*}}main_out.o
//
//    INDEX_BATCH_WITHOUT:      second_out.o-{{.*}}
//    INDEX_BATCH_WITHOUT-NEXT: --------
//    INDEX_BATCH_WITHOUT:      out-file: {{.*}}second_out.o
//
// B) Run with -index-unit-output-path values different from the -o values:
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_batch_with \
//    RUN:     -primary-file %t/main.swift -primary-file %t/second.swift \
//    RUN:     -o %t/main_out.o -o %t/second_out.o \
//    RUN:     -index-unit-output-path %t/custom_main.o -index-unit-output-path %t/custom_second.o
//
//    Run the same command as above again but with different -o value
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_batch_with \
//    RUN:     -primary-file %t/main.swift -primary-file %t/second.swift \
//    RUN:     -o %t/different_main.o -o %t/different_second.o \
//    RUN:     -index-unit-output-path %t/custom_main.o -index-unit-output-path %t/custom_second.o
//
//    Check the unit output is based on -index-unit-output-path:
//    RUN: c-index-test core -print-unit %t/idx_batch_with | %FileCheck -check-prefixes=INDEX_BATCH_WITH %s
//
//    Check the unit output is not baed on the -o values:
//    RUN: c-index-test core -print-unit %t/idx_batch_with | %FileCheck -check-prefixes=INDEX_BATCH_WITH_NEGATIVE %s
//
//    INDEX_BATCH_WITH:      custom_main.o-{{.*}}
//    INDEX_BATCH_WITH-NEXT: --------
//    INDEX_BATCH_WITH:      out-file: {{.*}}custom_main.o
//
//    INDEX_BATCH_WITH:      custom_second.o-{{.*}}
//    INDEX_BATCH_WITH-NEXT: --------
//    INDEX_BATCH_WITH:      out-file: {{.*}}custom_second.o
//
//    INDEX_BATCH_WITH_NEGATIVE-NOT: different_main.o
//    INDEX_BATCH_WITH_NEGATIVE-NOT: different_second.o
//    INDEX_BATCH_WITH_NEGATIVE-NOT: main_out.o
//    INDEX_BATCH_WITH_NEGATIVE-NOT: second_out.o
//
// C) Do the above again but with a fresh index store and using file lists.
//
//    RUN: echo '%t/main.swift' > %t/filelist
//    RUN: echo '%t/second.swift' >> %t/filelist
//
//    RUN: echo '%t/main_out.o' > %t/outputlist
//    RUN: echo '%t/second_out.o' >> %t/outputlist
//
//    RUN: echo '"%t/custom_main.o' > %t/index-outputlist
//    RUN: echo '"%t/custom_second.o' >> %t/index-outputlist
//
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_batch_with_list \
//    RUN:     -primary-filelist %t/filelist -filelist %t/filelist \
//    RUN:     -output-filelist %t/outputlist -index-unit-output-path-filelist %t/index-outputlist
//
//    RUN: echo '%t/different_main.o' > %t/outputlist
//    RUN: echo '%t/different_second.o' >> %t/outputlist
//
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_batch_with_list \
//    RUN:     -primary-filelist %t/filelist -filelist %t/filelist \
//    RUN:     -output-filelist %t/outputlist -index-unit-output-path-filelist %t/index-outputlist
//
//    RUN: c-index-test core -print-unit %t/idx_batch_with_list | %FileCheck -check-prefixes=INDEX_BATCH_WITH %s
//    RUN: c-index-test core -print-unit %t/idx_batch_with_list | %FileCheck -check-prefixes=INDEX_BATCH_WITH_NEGATIVE %s


// 3. Multi-threaded WMO
//
// A) Run without -index-unit-output-path:
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -num-threads 2 \
//    RUN:     -module-name mod_name -index-store-path %t/idx_wmo_without \
//    RUN:     %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o -o %t/second_out.o
//
//    Check the unit output is based on -o
//    RUN: c-index-test core -print-unit %t/idx_wmo_without | %FileCheck -check-prefixes=INDEX_WMO_WITHOUT %s
//
//    INDEX_WMO_WITHOUT:      main_out.o-{{.*}}
//    INDEX_WMO_WITHOUT-NEXT: --------
//    INDEX_WMO_WITHOUT:      out-file: {{.*}}main_out.o
//
//    INDEX_WMO_WITHOUT:      second_out.o-{{.*}}
//    INDEX_WMO_WITHOUT-NEXT: --------
//    INDEX_WMO_WITHOUT:      out-file: {{.*}}second_out.o
//
// B) Run with a -index-unit-output-path different from the -o value
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -num-threads 2 \
//    RUN:     -module-name mod_name -index-store-path %t/idx_wmo_with \
//    RUN:     %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o -o %t/second_out.o \
//    RUN:     -index-unit-output-path %t/main_custom.o -index-unit-output-path %t/second_custom.o
//
//    Run the same command as above again but with different -o values
//    RUN: %target-swift-frontend -typecheck -parse-stdlib -num-threads 2 \
//    RUN:     -module-name mod_name -index-store-path %t/idx_wmo_with \
//    RUN:     %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_different.o -o %t/second_different.o \
//    RUN:     -index-unit-output-path %t/main_custom.o -index-unit-output-path %t/second_custom.o
//
//    Check the unit output is based on -index-unit-output-path
//    RUN: c-index-test core -print-unit %t/idx_wmo_with | %FileCheck -check-prefixes=INDEX_WMO_WITH %s
//    Check no units were produced based on the -o values:
//    RUN: c-index-test core -print-unit %t/idx_wmo_with | %FileCheck -check-prefixes=INDEX_WMO_WITH_NEGATIVE %s
//
//    INDEX_WMO_WITH:      main_custom.o-{{.*}}
//    INDEX_WMO_WITH-NEXT: --------
//    INDEX_WMO_WITH:      out-file: {{.*}}main_custom.o
//
//    INDEX_WMO_WITH:      second_custom.o-{{.*}}
//    INDEX_WMO_WITH-NEXT: --------
//    INDEX_WMO_WITH:      out-file: {{.*}}second_custom.o
//
//    INDEX_WMO_WITH_NEGATIVE-NOT: main_out.o
//    INDEX_WMO_WITH_NEGATIVE-NOT: second_out.o
//    INDEX_WMO_WITH_NEGATIVE-NOT: main_different.o
//    INDEX_WMO_WITH_NEGATIVE-NOT: second_different.o


// 4. Diagnostics
//
// A) Check mismatched -o and -index-unit-output-path errors
//    RUN: not %target-swift-frontend -typecheck -parse-stdlib \
//    RUN:     -module-name mod_name -index-store-path %t/idx_ignored \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o -index-unit-output-path %t/custom_output.o -index-unit-output-path %t/custom_output.o 2>&1 \
//    RUN: | %FileCheck --check-prefixes=ERROR_MISMATCH %s
//
//    ERROR_MISMATCH: error: if any index unit output path files are specified, they all must be
//
// B) Check mismatched -o and -index-unit-output-path via file lists errors
//    RUN: echo '%t/main.swift' > %t/filelist
//    RUN: echo '%t/second.swift' >> %t/filelist
//
//    RUN: echo '%t/main_out.o' > %t/outputlist
//    RUN: echo '%t/second_out.o' >> %t/outputlist
//
//    RUN: echo '"%t/custom_main.o' > %t/index-outputlist
//
//    RUN: not %target-swift-frontend -typecheck -parse-stdlib -enable-batch-mode \
//    RUN:     -module-name mod_name -index-store-path %t/idx_ignored \
//    RUN:     -primary-filelist %t/filelist -filelist %t/filelist \
//    RUN:     -output-filelist %t/outputlist -index-unit-output-path-filelist %t/index-outputlist 2>&1 \
//    RUN: | %FileCheck --check-prefixes=ERROR_MISMATCH %s
//
// C) Check -index-unit-output-path without -index-store-path does not cause a warning/error
//    RUN: %target-typecheck-verify-swift -parse-stdlib \
//    RUN:     -module-name mod_name \
//    RUN:     -primary-file %t/main.swift %t/second.swift \
//    RUN:     -o %t/main_out.o -index-unit-output-path %t/custom_output.o
