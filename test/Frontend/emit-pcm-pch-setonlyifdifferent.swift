// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-emit-pcm -module-name Header -o %t/Header.pcm %t/module.modulemap
// RUN: %{python} %S/../Inputs/getmtime.py %t/Header.pcm > %t/Header.pcm.stat.before
// RUN: %target-swift-emit-pcm -module-name Header -o %t/Header.pcm %t/module.modulemap
// RUN: %{python} %S/../Inputs/getmtime.py %t/Header.pcm > %t/Header.pcm.stat.after
// RUN: diff %t/Header.pcm.stat.before %t/Header.pcm.stat.after

// RUN: %target-swift-frontend -emit-pch %t/Header.h -o %t/Header.h.pch
// RUN: %{python} %S/../Inputs/getmtime.py %t/Header.h.pch > %t/Header.h.pch.stat.before
// RUN: %target-swift-frontend -emit-pch %t/Header.h -o %t/Header.h.pch
// RUN: %{python} %S/../Inputs/getmtime.py %t/Header.h.pch > %t/Header.h.pch.stat.after
// RUN: diff %t/Header.h.pch.stat.before %t/Header.h.pch.stat.after

//--- test.swift
import Header
func testFunc() {}

//--- module.modulemap
module Header {
  header "Header.h"
}

//--- Header.h
void header(void);
