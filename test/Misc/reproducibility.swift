// RUN: rm -rf %t && mkdir -p %t
// RUN: echo 'print("byte-for-byte identical binaries expected")' > %t/test.swift
// RUN: %target-swiftc_driver -emit-executable -gnone -o %t/executable-1 %t/test.swift
// RUN: %target-swiftc_driver -emit-executable -gnone -o %t/executable-2 %t/test.swift
// RUN: strip %t/executable-1 %t/executable-2
// RUN: cmp %t/executable-1 %t/executable-2
