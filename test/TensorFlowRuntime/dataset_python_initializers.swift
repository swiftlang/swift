// RUN: %target-run-simple-swift --csv-file %S/Inputs/data.csv
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DatasetPythonInitializersTest = TestSuite("DatasetAPI")

let csvFilenameIndex = CommandLine.arguments.firstIndex(of: "--csv-file")! + 1
let csvFilename = CommandLine.arguments[csvFilenameIndex]

DatasetPythonInitializersTest.test("SingleValueDataset") {
  let dataset = SingleValueDataset<Float>(contentsOfCSVFile: csvFilename,
                                          hasHeader: false,
                                          selectingColumns: [0, 1])
  let expected: [[Float]] = [[1, 2], [3, 4]]
  var i = 0
  for element in dataset {
    expectEqual(expected[i], element.scalars)
    i += 1
  }
}

DatasetPythonInitializersTest.test("DoubleValueDataset") {
  let dataset = DoubleValueDataset<Float>(contentsOfCSVFile: csvFilename,
                                          hasHeader: false,
                                          selectingColumns: ([0], [1]))
  let expected1: [[Float]] = [[1], [3]]
  let expected2: [[Float]] = [[2], [4]]
  var i = 0
  for (element1, element2) in dataset {
    expectEqual(expected1[i], element1.scalars)
    expectEqual(expected2[i], element2.scalars)
    i += 1
  }
}

runAllTests()
