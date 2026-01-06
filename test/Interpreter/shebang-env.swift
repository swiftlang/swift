// This file is also used by shebang-direct.swift.
// REQUIRES: SR77996

// RUN: echo '#!/usr/bin/env' 'swift ' > %t.shebang.swift
// RUN: cat %s >> %t.shebang.swift
// RUN: chmod u+x %t.shebang.swift

// RUN: env PATH=$(dirname %swift_driver_plain) %t.shebang.swift | %raw-FileCheck -check-prefix=NONE %s
// RUN: env PATH=$(dirname %swift_driver_plain) %t.shebang.swift a b c | %raw-FileCheck -check-prefix=THREE-ARGS %s

// REQUIRES: swift_interpreter

print("Begin")
for arg in CommandLine.arguments {
  print(arg)
}
print("End")

// NONE: Begin
// NONE-NEXT: {{.*}}shebang.swift
// NONE-NEXT: End

// THREE-ARGS: Begin
// THREE-ARGS-NEXT: {{.*}}shebang.swift
// THREE-ARGS-NEXT: a
// THREE-ARGS-NEXT: b
// THREE-ARGS-NEXT: c
// THREE-ARGS-NEXT: End
