// REQUIRES: OS=windows-msvc

// Test building the CRT module with C++ interop enabled on Windows

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -scan-dependencies -module-name Test \
// RUN:   -module-cache-path %t/clang-module-cache -disable-objc-interop \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -parse-stdlib -module-load-mode prefer-serialized \
// RUN:   %t/Test.swift -o %t/deps.json -I %t 2>&1

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SAL > %t/SAL.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:vcruntime > %t/vcruntime.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/SwiftShims.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Swift > %t/Swift.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_Builtin_stddef > %t/_Builtin_stddef.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:corecrt > %t/corecrt.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std_config > %t/std_config.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_float > %t/_float.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_fenv > %t/_fenv.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_stdlib > %t/_stdlib.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_malloc > %t/_malloc.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_Builtin_intrinsics > %t/_Builtin_intrinsics.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:ucrt > %t/ucrt.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:std > %t/std.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:_complex > %t/_complex.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftOverlayShims > %t/SwiftOverlayShims.cmd
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json CRT > %t/CRT.cmd

// Remove the candidate module file flag to force building from the swiftinterface file.
// RUN: cat %t/CRT.cmd | sed '/candidate-module-file/d; /CRT.swiftmodule.*swiftmodule/d' > %t/CRT.mod.cmd

// RUN: %swift_frontend_plain @%t/SAL.cmd 2>&1
// RUN: %swift_frontend_plain @%t/vcruntime.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_Builtin_stddef.cmd 2>&1
// RUN: %swift_frontend_plain @%t/corecrt.cmd 2>&1
// RUN: %swift_frontend_plain @%t/std_config.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_float.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_fenv.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_stdlib.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_malloc.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_Builtin_intrinsics.cmd 2>&1
// RUN: %swift_frontend_plain @%t/SwiftShims.cmd 2>&1
// RUN: %swift_frontend_plain @%t/Swift.cmd 2>&1
// RUN: %swift_frontend_plain @%t/ucrt.cmd 2>&1
// RUN: %swift_frontend_plain @%t/std.cmd 2>&1
// RUN: %swift_frontend_plain @%t/_complex.cmd 2>&1
// RUN: %swift_frontend_plain @%t/SwiftOverlayShims.cmd 2>&1
// RUN: %swift_frontend_plain @%t/CRT.mod.cmd 2>&1

//--- Test.swift
import CRT

