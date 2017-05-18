// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -emit-migrated-file-path %t/swap_fixit.swift.result -enforce-exclusivity=checked -swift-version 3 -primary-file %s -o /dev/null
// RUN: diff -u %S/swap_fixit.swift.expected %t/swap_fixit.swift.result

import Swift

struct StructWithFixits {
  var arrayProp: [Int] = [1, 2, 3]

  mutating
  func shouldHaveFixIts<T>(_ i: Int, _ j: Int, _ param: T, _ paramIndex: T.Index) where T : MutableCollection {
    var array1 = [1, 2, 3]
    swap(&array1[i + 5], &array1[j - 2])
    swap(&self.arrayProp[i], &self.arrayProp[j])

    var localOfGenericType = param
    swap(&localOfGenericType[paramIndex], &localOfGenericType[paramIndex])
  }

  mutating
  func shouldHaveNoFixIts(_ i: Int, _ j: Int) {
    var array1 = [1, 2, 3]
    var array2 = [1, 2, 3]

    // Swapping between different arrays should cannot have the
    // Fix-It.
    swap(&array1[i], &array2[j]) // no-warning no-fixit
    swap(&array1[i], &self.arrayProp[j]) // no-warning no-fixit
  }
}

