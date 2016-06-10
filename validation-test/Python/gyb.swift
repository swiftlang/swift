// RUN: %{python} -m doctest %S/../../utils/gyb.py
// RUN: echo 'Hello ${ME}' | %S/../../utils/gyb --test -DME=Swift | FileCheck %s
// RUN: echo 'Hello ${ME}' | %S/../../utils/gyb --verbose-test -DME=Swift | FileCheck %s
// CHECK: Hello Swift
