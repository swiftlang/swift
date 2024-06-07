// RUN: %target-swift-frontend -typecheck %s -swift-version 4 %api_diff_data_dir
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t.result -swift-version 4 %api_diff_data_dir
// RUN: diff -u %s.expected %t.result

// REQUIRES: VENDOR=apple
// UNSUPPORTED: OS=xros

func migrate<T, U: Sequence, V: Collection>(lseq: LazySequence<T>, lcol: LazyCollection<T>, seq: U, col: V, test: Bool) {
  _ = lseq.flatMap { test ? nil : $0 }
  _ = lcol.flatMap { test ? nil : $0 }
  _ = seq.flatMap { test ? nil : $0 }
  _ = col.flatMap { test ? nil : "\($0)" }

  _ = lseq.flatMap { [$0, $0] }
  _ = lcol.flatMap { [$0, $0] }
  _ = seq.flatMap { [$0, $0] }
  _ = col.flatMap { "\($0)" }
}
