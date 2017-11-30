// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: cd %t

// RUN: %swift  -interpret %s &&  test ! -r %t/immediate-mode-no-output
