// RUN: %sourcekitd-test -req=open %S/Inputs/rdar_18677108-2-a.swift \
// RUN:                               --  %S/Inputs/rdar_18677108-2-b.swift \
// RUN:                                   %S/Inputs/rdar_18677108-2-a.swift \
// RUN:                                   -primary-file %S/Inputs/rdar_18677108-2-a.swift \
// RUN:               == -req=print-diags %S/Inputs/rdar_18677108-2-a.swift | %sed_clean > %t.response
// RUN: diff -u %s.response %t.response



