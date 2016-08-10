// RUN: %{python} -m doctest %utils/gyb.py
// RUN: echo 'Hello ${ME}' | %gyb --test -DME=Swift | %FileCheck %s
// RUN: echo 'Hello ${ME}' | %gyb --verbose-test -DME=Swift | %FileCheck %s
// CHECK: Hello Swift
