//===-- PythonLibrary+Symbols.swift ---------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Python symbols required for the interoperability layer.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Required Python typealias and constants.
//===----------------------------------------------------------------------===//

typealias PyObjectPointer = UnsafeMutableRawPointer
typealias PyCCharPointer = UnsafePointer<Int8>
typealias PyBinaryOperation =
  (@convention(c) (PyObjectPointer?, PyObjectPointer?) -> PyObjectPointer?)

let Py_LT: Int32 = 0
let Py_LE: Int32 = 1
let Py_EQ: Int32 = 2
let Py_NE: Int32 = 3
let Py_GT: Int32 = 4
let Py_GE: Int32 = 5

//===----------------------------------------------------------------------===//
// Python library symbols lazily loaded at runtime.
//===----------------------------------------------------------------------===//

let Py_Initialize = PythonLibrary.getSymbol(
  name: "Py_Initialize",
  signature: (@convention(c) () -> ()).self
)

let Py_IncRef = PythonLibrary.getSymbol(
  name: "Py_IncRef",
  signature: (@convention(c) (PyObjectPointer?) -> ()).self
)

let Py_DecRef = PythonLibrary.getSymbol(
  name: "Py_DecRef",
  signature: (@convention(c) (PyObjectPointer?) -> ()).self
)

let PyImport_ImportModule = PythonLibrary.getSymbol(
  name: "PyImport_ImportModule",
  signature: (@convention(c) (PyCCharPointer) -> PyObjectPointer?).self
)

let PyEval_GetBuiltins = PythonLibrary.getSymbol(
  name: "PyEval_GetBuiltins",
  signature: (@convention(c) () -> PyObjectPointer).self
)

let PyErr_Occurred = PythonLibrary.getSymbol(
  name: "PyErr_Occurred",
  signature: (@convention(c) () -> PyObjectPointer?).self
)

let PyErr_Clear = PythonLibrary.getSymbol(
  name: "PyErr_Clear",
  signature: (@convention(c) () -> ()).self
)

let PyErr_Fetch = PythonLibrary.getSymbol(
  name: "PyErr_Fetch",
  signature: (@convention(c) (
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>) -> ()).self
)

let PyDict_New = PythonLibrary.getSymbol(
  name: "PyDict_New",
  signature: (@convention(c) () -> PyObjectPointer?).self
)

let PyDict_SetItem = PythonLibrary.getSymbol(
  name: "PyDict_SetItem",
  signature: (@convention(c) (PyObjectPointer?, PyObjectPointer, PyObjectPointer) -> ()).self
)

let PyObject_GetItem = PythonLibrary.getSymbol(
  name: "PyObject_GetItem",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer) -> PyObjectPointer?).self
)

let PyObject_SetItem = PythonLibrary.getSymbol(
  name: "PyObject_SetItem",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer, PyObjectPointer) -> ()).self
)

let PyObject_DelItem = PythonLibrary.getSymbol(
  name: "PyObject_DelItem",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer) -> ()).self
)

let PyObject_Call = PythonLibrary.getSymbol(
  name: "PyObject_Call",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer, PyObjectPointer?) -> (PyObjectPointer?)).self
)

let PyObject_CallObject = PythonLibrary.getSymbol(
  name: "PyObject_CallObject",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer) -> (PyObjectPointer?)).self
)

let PyObject_GetAttrString = PythonLibrary.getSymbol(
  name: "PyObject_GetAttrString",
  signature: (@convention(c) (PyObjectPointer, PyCCharPointer) -> (PyObjectPointer?)).self
)

let PyObject_SetAttrString = PythonLibrary.getSymbol(
  name: "PyObject_SetAttrString",
  signature: (@convention(c) (PyObjectPointer, PyCCharPointer, PyObjectPointer) -> (Int)).self
)

let PySlice_New = PythonLibrary.getSymbol(
  name: "PySlice_New",
  signature: (@convention(c) (PyObjectPointer?, PyObjectPointer?, PyObjectPointer?) -> (PyObjectPointer?)).self
)

let PyTuple_New = PythonLibrary.getSymbol(
  name: "PyTuple_New",
  signature: (@convention(c) (Int) -> (PyObjectPointer?)).self
)

let PyTuple_SetItem = PythonLibrary.getSymbol(
  name: "PyTuple_SetItem",
  signature: (@convention(c) (PyObjectPointer, Int, PyObjectPointer) -> ()).self
)

let PyObject_RichCompareBool = PythonLibrary.getSymbol(
  name: "PyObject_RichCompareBool",
  signature: (@convention(c) (PyObjectPointer, PyObjectPointer, Int32) -> (Int32)).self
)

let PyDict_Next = PythonLibrary.getSymbol(
  name: "PyDict_Next",
  signature: (@convention(c) (
    PyObjectPointer, UnsafeMutablePointer<Int>,
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>) -> (Int32)).self
)

let PyList_New = PythonLibrary.getSymbol(
  name: "PyList_New",
  signature: (@convention(c) (Int) -> (PyObjectPointer?)).self
)

let PyList_SetItem = PythonLibrary.getSymbol(
  name: "PyList_SetItem",
  signature: (@convention(c) (PyObjectPointer, Int, PyObjectPointer) -> (Int32)).self
)

let PyBool_FromLong = PythonLibrary.getSymbol(
  name: "PyBool_FromLong",
  signature: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyFloat_AsDouble = PythonLibrary.getSymbol(
  name: "PyFloat_AsDouble",
  signature: (@convention(c) (PyObjectPointer) -> (Double)).self
)

let PyFloat_FromDouble = PythonLibrary.getSymbol(
  name: "PyFloat_FromDouble",
  signature: (@convention(c) (Double) -> (PyObjectPointer)).self
)

let PyInt_AsLong = PythonLibrary.getSymbol(
  name: "PyLong_AsLong",
  legacyName: "PyInt_AsLong",
  signature: (@convention(c) (PyObjectPointer) -> (Int)).self
)

let PyInt_FromLong = PythonLibrary.getSymbol(
  name: "PyLong_FromLong",
  legacyName: "PyInt_FromLong",
  signature: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyInt_AsUnsignedLongMask = PythonLibrary.getSymbol(
  name: "PyLong_AsUnsignedLongMask",
  legacyName: "PyInt_AsUnsignedLongMask",
  signature: (@convention(c) (PyObjectPointer) -> (UInt)).self
)

let PyInt_FromSize_t = PythonLibrary.getSymbol(
  name: "PyInt_FromLong",
  legacyName: "PyInt_FromSize_t",
  signature: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyString_AsString = PythonLibrary.getSymbol(
  name: "PyUnicode_AsUTF8",
  legacyName: "PyString_AsString",
  signature: (@convention(c) (PyObjectPointer) -> (PyCCharPointer?)).self
)

let PyString_FromStringAndSize = PythonLibrary.getSymbol(
  name: "PyUnicode_DecodeUTF8",
  legacyName: "PyString_FromStringAndSize",
  signature: (@convention(c) (PyCCharPointer?, Int) -> (PyObjectPointer?)).self
)

var _Py_ZeroStruct = PythonLibrary.getSymbol(
  name: "_Py_ZeroStruct",
  signature: PyObjectPointer.self
)

var _Py_TrueStruct = PythonLibrary.getSymbol(
  name: "_Py_TrueStruct",
  signature: PyObjectPointer.self
)

var _Py_TrueStructb = PythonLibrary.getSymbol(
  name: "_Py_TrueStructb",
  signature: PyObjectPointer.self
)

var PyBool_Type = PythonLibrary.getSymbol(
  name: "PyBool_Type",
  signature: PyObjectPointer.self
)

var PySlice_Type = PythonLibrary.getSymbol(
  name: "PySlice_Type",
  signature: PyObjectPointer.self
)

let PyNumber_Add = PythonLibrary.getSymbol(
  name: "PyNumber_Add",
  signature: PyBinaryOperation.self
)

let PyNumber_Subtract = PythonLibrary.getSymbol(
  name: "PyNumber_Subtract",
  signature: PyBinaryOperation.self
)

let PyNumber_Multiply = PythonLibrary.getSymbol(
  name: "PyNumber_Multiply",
  signature: PyBinaryOperation.self
)

let PyNumber_TrueDivide = PythonLibrary.getSymbol(
  name: "PyNumber_TrueDivide",
  signature: PyBinaryOperation.self
)

let PyNumber_InPlaceAdd = PythonLibrary.getSymbol(
  name: "PyNumber_InPlaceAdd",
  signature: PyBinaryOperation.self
)

let PyNumber_InPlaceSubtract = PythonLibrary.getSymbol(
  name: "PyNumber_InPlaceSubtract",
  signature: PyBinaryOperation.self
)

let PyNumber_InPlaceMultiply = PythonLibrary.getSymbol(
  name: "PyNumber_InPlaceMultiply",
  signature: PyBinaryOperation.self
)

let PyNumber_InPlaceTrueDivide = PythonLibrary.getSymbol(
  name: "PyNumber_InPlaceTrueDivide",
  signature: PyBinaryOperation.self
)
