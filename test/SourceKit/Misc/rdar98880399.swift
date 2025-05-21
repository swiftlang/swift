// Make sure we don't create any temporary files.
// RUN: %empty-directory(%t)
// RUN: env TMP=%t TMPDIR=%t %sourcekitd-test -req=open %s -- %s -driver-force-response-files
// RUN: not ls %t/* >/dev/null 2>&1
