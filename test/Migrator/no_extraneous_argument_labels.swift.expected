// RUN: %target-swift-frontend -typecheck %s -swift-version 3
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -primary-file %s -emit-migrated-file-path %t/no_extraneous_argument_labels.result -swift-version 3 -o /dev/null
// RUN: diff -u %s.expected %t/no_extraneous_argument_labels.result
// RUN: %target-swift-frontend -typecheck %s.expected -swift-version 4

func foo(_ oc: [String]) {
  var args: [String] = []
  let dictionary: [String: String] = [:]
  args.append(contentsOf: oc.map { orderedColumn in
    dictionary.first { (column, value) in true }!.value
  })
}
