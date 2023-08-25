// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name=main %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: not %target-run %t/a.out 2>&1 | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=windows-msvc

// Check that a fatal error terminates the process and it appears only once.
// CHECK: Fatal error
// CHECK-NOT: Fatal error

import WinSDK

private var timerID: UINT_PTR = 1
let doWork: TIMERPROC = { (_: HWND?, _: UINT, _: UINT_PTR, _: DWORD) in
    fatalError("oops")
    timerID = SetTimer(nil, timerID, UInt32(0), doWork)
}
var msg: MSG = .init()
timerID = SetTimer(nil, timerID, UInt32(0), doWork)
while (GetMessageA(&msg, nil, 0, 0)) {
  TranslateMessage(&msg);
  DispatchMessageA(&msg);
}
