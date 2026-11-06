// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// The subcommand can be anywhere among the arguments.
// RUN: %cache-tool print-base-key -cas-path %t/cas -- \
// RUN:   %target-swift-frontend-plain -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd > %t/key1.casid
// RUN: %cache-tool -cas-path %t/cas print-base-key -- \
// RUN:   %target-swift-frontend-plain -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd > %t/key2.casid
// RUN: diff %t/key1.casid %t/key2.casid

/// The legacy -cache-tool-action spelling of the subcommand is still accepted.
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-base-key -- \
// RUN:   %target-swift-frontend-plain -c -cache-compile-job -cas-path %t/cas %s -o %t/test.o @%t/MyApp.cmd > %t/key3.casid
// RUN: diff %t/key1.casid %t/key3.casid

/// Only the first subcommand name is the subcommand, later ones are inputs.
/// With the legacy spelling, all positional arguments are inputs.
// RUN: not %cache-tool print-include-tree-list -cas-path %t/cas print-base-key 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=INPUT
// RUN: not %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list print-base-key 2>&1 \
// RUN:   | %FileCheck %s --check-prefix=INPUT
// INPUT: invalid cas-id 'print-base-key'

// RUN: not %cache-tool -cas-path %t/cas 2>&1 | %FileCheck %s --check-prefix=NO-SUBCOMMAND
// RUN: not %cache-tool -cas-path %t/cas bogus 2>&1 | %FileCheck %s --check-prefix=NO-SUBCOMMAND
// NO-SUBCOMMAND: error: missing subcommand; use '-help' to list the subcommands

// RUN: not %cache-tool -cas-path %t/cas -cache-tool-action bogus 2>&1 | %FileCheck %s --check-prefix=INVALID-ACTION
// INVALID-ACTION: error: invalid value 'bogus' in '-cache-tool-action'

// RUN: %cache-tool --help | %FileCheck %s --check-prefix=HELP
// RUN: %cache-tool -cache-tool-action bogus --help | %FileCheck %s --check-prefix=HELP
// HELP:      SUBCOMMANDS:
// HELP:      print-base-key - Print the base cache key
// HELP:      OPTIONS:
// HELP-NOT:  -cas-path
// HELP-NOT:  -cache-tool-action

// RUN: %cache-tool print-base-key --help | %FileCheck %s --check-prefix=HELP-BASE-KEY
// RUN: %cache-tool -cache-tool-action print-base-key --help | %FileCheck %s --check-prefix=HELP-BASE-KEY
// HELP-BASE-KEY:     Print the base cache key
// HELP-BASE-KEY:     OPTIONS:
// HELP-BASE-KEY:     -cas-path <path>

func test() {}
