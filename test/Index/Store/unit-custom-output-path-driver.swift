// RUN: %empty-directory(%t)
// RUN: touch %/t/main.swift
// RUN: touch %/t/second.swift

// 1. Incremental mode (single primary file)
//
// A) Run without an "index-unit-output-path" entry:
//    RUN: echo '{                                 ' > %/t/outputmap.json
//    RUN: echo '   "%/t/main.swift":{             ' >> %/t/outputmap.json
//    RUN: echo '      "object": "%/t/main_out.o"  ' >> %/t/outputmap.json
//    RUN: echo '   },                             ' >> %/t/outputmap.json
//    RUN: echo '   "%/t/second.swift":{           ' >> %/t/outputmap.json
//    RUN: echo '      "object": "%/t/second_out.o"' >> %/t/outputmap.json
//    RUN: echo '   }                              ' >> %/t/outputmap.json
//    RUN: echo '}                                 ' >> %/t/outputmap.json
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -disable-batch-mode -output-file-map %/t/outputmap.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_without \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on the "object" field
//    RUN: c-index-test core -print-unit %/t/idx_without | %FileCheck -check-prefixes=INDEX_WITHOUT %s
//
//    INDEX_WITHOUT:      main_out.o-{{.*}}
//    INDEX_WITHOUT-NEXT: --------
//    INDEX_WITHOUT:      out-file: {{.*}}main_out.o
//
//    INDEX_WITHOUT:      second_out.o-{{.*}}
//    INDEX_WITHOUT-NEXT: --------
//    INDEX_WITHOUT:      out-file: {{.*}}second_out.o
//
// B) Run with an "index-unit-output-path" entry different from the "output" entry:
//    RUN: echo '{                                                     ' > %/t/outputmap-custom.json
//    RUN: echo '   "%/t/main.swift":{                                 ' >> %/t/outputmap-custom.json
//    RUN: echo '      "object": "%/t/main_out.o",                     ' >> %/t/outputmap-custom.json
//    RUN: echo '      "index-unit-output-path": "%/t/main_custom.o"   ' >> %/t/outputmap-custom.json
//    RUN: echo '   },                                                 ' >> %/t/outputmap-custom.json
//    RUN: echo '   "%/t/second.swift":{                               ' >> %/t/outputmap-custom.json
//    RUN: echo '      "object": "%/t/second_out.o",                   ' >> %/t/outputmap-custom.json
//    RUN: echo '      "index-unit-output-path": "%/t/second_custom.o" ' >> %/t/outputmap-custom.json
//    RUN: echo '   }                                                  ' >> %/t/outputmap-custom.json
//    RUN: echo '}                                                     ' >> %/t/outputmap-custom.json
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -disable-batch-mode -output-file-map %/t/outputmap-custom.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Run the same command as above again but with different "output" entry
//    RUN: echo '{                                                     ' > %/t/outputmap-different.json
//    RUN: echo '   "%/t/main.swift":{                                 ' >> %/t/outputmap-different.json
//    RUN: echo '      "object": "%/t/main_different.o",               ' >> %/t/outputmap-different.json
//    RUN: echo '      "index-unit-output-path": "%/t/main_custom.o"   ' >> %/t/outputmap-different.json
//    RUN: echo '   },                                                 ' >> %/t/outputmap-different.json
//    RUN: echo '   "%/t/second.swift":{                               ' >> %/t/outputmap-different.json
//    RUN: echo '      "object": "%/t/second_different.o",             ' >> %/t/outputmap-different.json
//    RUN: echo '      "index-unit-output-path": "%/t/second_custom.o" ' >> %/t/outputmap-different.json
//    RUN: echo '   }                                                  ' >> %/t/outputmap-different.json
//    RUN: echo '}                                                     ' >> %/t/outputmap-different.json
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -disable-batch-mode -output-file-map %/t/outputmap-different.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on -index-unit-output-path:
//    RUN: c-index-test core -print-unit %/t/idx_with | %FileCheck -check-prefixes=INDEX_WITH %s
//
//    Check no units were produced based on the -o values
//    RUN: c-index-test core -print-unit %/t/idx_with | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s
//
//    INDEX_WITH:      main_custom.o-{{.*}}
//    INDEX_WITH-NEXT: --------
//    INDEX_WITH:      out-file: {{.*}}main_custom.o
//
//    INDEX_WITH:      second_custom.o-{{.*}}
//    INDEX_WITH-NEXT: --------
//    INDEX_WITH:      out-file: {{.*}}second_custom.o
//
//    INDEX_WITH_NEGATIVE-NOT: main_out.o
//    INDEX_WITH_NEGATIVE-NOT: main_different.o
//    INDEX_WITH_NEGATIVE-NOT: second_out.o
//    INDEX_WITH_NEGATIVE-NOT: second_different.o
//
//    Run with a different -index-unit-output-path
//    RUN: echo '{                                                     ' > %/t/outputmap-different2.json
//    RUN: echo '   "%/t/main.swift":{                                 ' >> %/t/outputmap-different2.json
//    RUN: echo '      "object": "%/t/main_different.o",               ' >> %/t/outputmap-different2.json
//    RUN: echo '      "index-unit-output-path": "%/t/xmain_custom.o"  ' >> %/t/outputmap-different2.json
//    RUN: echo '   },                                                 ' >> %/t/outputmap-different2.json
//    RUN: echo '   "%/t/second.swift":{                               ' >> %/t/outputmap-different2.json
//    RUN: echo '      "object": "%/t/second_different.o",             ' >> %/t/outputmap-different2.json
//    RUN: echo '      "index-unit-output-path": "%/t/xsecond_custom.o"' >> %/t/outputmap-different2.json
//    RUN: echo '   }                                                  ' >> %/t/outputmap-different2.json
//    RUN: echo '}                                                     ' >> %/t/outputmap-different2.json
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -disable-batch-mode -output-file-map %/t/outputmap-different2.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check it resulted in a second unit being added to the store
//    RUN: c-index-test core -print-unit %/t/idx_with | %FileCheck -check-prefixes=INDEX_WITH,INDEX_WITH_SECOND %s
//
//    INDEX_WITH_SECOND: xmain_custom.o-{{.*}}
//    INDEX_WITH_SECOND: --------
//    INDEX_WITH_SECOND: out-file: {{.*}}xmain_custom.o
//
//    INDEX_WITH_SECOND: xsecond_custom.o-{{.*}}
//    INDEX_WITH_SECOND: --------
//    INDEX_WITH_SECOND: out-file: {{.*}}xsecond_custom.o
//
// C) Do the above again but with a fresh index store and using file lists.
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -disable-batch-mode -driver-filelist-threshold=0 \
//    RUN:     -output-file-map %/t/outputmap-custom.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_with_list \
//    RUN:     %/t/main.swift %/t/second.swift
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -driver-filelist-threshold=0 -output-file-map %/t/outputmap-different.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_with_list \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    RUN: c-index-test core -print-unit %/t/idx_with_list | %FileCheck -check-prefixes=INDEX_WITH %s
//    RUN: c-index-test core -print-unit %/t/idx_with_list | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s


// 2. Batch mode (multiple primary files)
//
// A) Run without an "index-unit-output-path" entry:
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -enable-batch-mode -driver-batch-count 1 -output-file-map %/t/outputmap.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_batch_without \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on the "output" values
//    RUN: c-index-test core -print-unit %/t/idx_batch_without | %FileCheck -check-prefixes=INDEX_WITHOUT %s
//
// B) Run with "index-unit-output-path" values different from the "output" values:
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -enable-batch-mode -driver-batch-count 1 -output-file-map %/t/outputmap-custom.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_batch_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Run the same command as above again but with different "output" value
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -enable-batch-mode -driver-batch-count 1 -output-file-map %/t/outputmap-different.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_batch_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on "index-unit-output-path":
//    RUN: c-index-test core -print-unit %/t/idx_batch_with | %FileCheck -check-prefixes=INDEX_WITH %s
//
//    Check the unit output is not baed on the "output" values:
//    RUN: c-index-test core -print-unit %/t/idx_batch_with | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s
//
// C) Do the above again but with a fresh index store and using file lists.
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -driver-filelist-threshold=0 -enable-batch-mode -driver-batch-count 1 \
//    RUN:     -output-file-map %/t/outputmap-custom.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_batch_with_list \
//    RUN:     %/t/main.swift %/t/second.swift
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -driver-filelist-threshold=0 -enable-batch-mode -driver-batch-count 1 \
//    RUN:     -output-file-map %/t/outputmap-different.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_batch_with_list \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    RUN: c-index-test core -print-unit %/t/idx_batch_with_list | %FileCheck -check-prefixes=INDEX_WITH %s
//    RUN: c-index-test core -print-unit %/t/idx_batch_with_list | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s


// 3. Multi-threaded WMO
//
// A) Run without an "index-unit-output-path" entry:
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -whole-module-optimization -num-threads 2 -output-file-map %/t/outputmap.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_wmo_without \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on the "output" entry
//    RUN: c-index-test core -print-unit %/t/idx_wmo_without | %FileCheck -check-prefixes=INDEX_WITHOUT %s
//
// B) Run with an "index-unit-output-path" entry different from the "output" value
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -whole-module-optimization -num-threads 2 -output-file-map %/t/outputmap-custom.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_wmo_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Run the same command as above again but with different "output" values
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -whole-module-optimization -num-threads 2 -output-file-map %/t/outputmap-different.json \
//    RUN:     -module-name mod_name -index-store-path %/t/idx_wmo_with \
//    RUN:     %/t/main.swift %/t/second.swift
//
//    Check the unit output is based on "index-unit-output-path" entry
//    RUN: c-index-test core -print-unit %/t/idx_wmo_with | %FileCheck -check-prefixes=INDEX_WITH %s
//    Check no units were produced based on the "output" values:
//    RUN: c-index-test core -print-unit %/t/idx_wmo_with | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s


// 4. Index file
//
// A) Run without -index-unit-output-path
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_without \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/main.swift \
//    RUN:     -o %/t/main_out.o
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_without \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/second.swift \
//    RUN:     -o %/t/second_out.o
//
//    Check that the unit output is based on -o
//    RUN: c-index-test core -print-unit %/t/idx_index_without | %FileCheck -check-prefixes=INDEX_WITHOUT %s
//
// B) Run with -index-unit-output-path
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_with \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/main.swift \
//    RUN:     -o %/t/main_out.o -index-unit-output-path %/t/main_custom.o
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_with \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/second.swift \
//    RUN:     -o %/t/second_out.o -index-unit-output-path %/t/second_custom.o
//
//    Run the same commands as above again but with different -o value
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_with \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/main.swift \
//    RUN:     -o %/t/main_different.o -index-unit-output-path %/t/main_custom.o
//    RUN: %target-swiftc_driver -Xfrontend -index-ignore-stdlib -module-name mod_name -index-store-path %/t/idx_index_with \
//    RUN:     %/t/main.swift %/t/second.swift -index-file -index-file-path %/t/second.swift \
//    RUN:     -o %/t/second_different.o -index-unit-output-path %/t/second_custom.o
//
//    Check that the unit output is based on -index-unit-output-path
//    RUN: c-index-test core -print-unit %/t/idx_index_with | %FileCheck -check-prefixes=INDEX_WITH %s
//
//    Check that the unit output is not based on -o
//    RUN: c-index-test core -print-unit %/t/idx_index_with | %FileCheck -check-prefixes=INDEX_WITH_NEGATIVE %s
