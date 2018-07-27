// RUN: not %swift -emit-sil -target %target-triple %s -emit-fixits-path %t.remap -I %S/Inputs -diagnostics-editor-mode
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

import swift
