simple.paramNoLabel(0)
simple.paramsNoLabels(0, 1)
simple.paramWithLabel(label: 0)
simple.paramsWithLabel(label1: 0, label2: 1)

subscriptNoLabel[b].paramNoLabel(0)
subscriptWithLabel[label: b].paramWithLabel(label: 0)

// RUN: %sourcekitd-test -req=structure %s > %t.response
// RUN: %diff -u %s.response %t.response
