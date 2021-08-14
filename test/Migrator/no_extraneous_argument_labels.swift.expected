// RUN: %target-swift-frontend -typecheck %s -swift-version 4 %api_diff_data_dir
// RUN: %empty-directory(%t) && %target-swift-frontend -c -primary-file %s -emit-migrated-file-path %t/no_extraneous_argument_labels.result -swift-version 4 -o /dev/null %api_diff_data_dir
// RUN: %diff -u %s.expected %t/no_extraneous_argument_labels.result
// RUN: %target-swift-frontend -typecheck %s.expected -swift-version 5 %api_diff_data_dir

func foo(_ oc: [String]) {
  var args: [String] = []
  let dictionary: [String: String] = [:]
  args.append(contentsOf: oc.map { orderedColumn in
    dictionary.first { (column, value) in true }!.value
  })
}
