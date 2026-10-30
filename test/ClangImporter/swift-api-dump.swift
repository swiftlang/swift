// RUN: %empty-directory(%t)
// RUN: not %{python} %utils/swift-api-dump.py -s macosx -m ThisModuleDoesNotExist -i %swift-ide-test_plain -o %t -j 1 -q

// REQUIRES: OS=macosx
