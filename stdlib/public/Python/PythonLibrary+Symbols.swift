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
  @convention(c) (PyObjectPointer?, PyObjectPointer?) -> PyObjectPointer?

let Py_LT: Int32 = 0
let Py_LE: Int32 = 1
let Py_EQ: Int32 = 2
let Py_NE: Int32 = 3
let Py_GT: Int32 = 4
let Py_GE: Int32 = 5

//===----------------------------------------------------------------------===//
// Python library symbols lazily loaded at runtime.
//===----------------------------------------------------------------------===//

let Py_Initialize = PythonLibrary.loadSymbol(
  name: "Py_Initialize",
  type: (@convention(c) () -> ()).self
)

let Py_IncRef = PythonLibrary.loadSymbol(
  name: "Py_IncRef",
  type: (@convention(c) (PyObjectPointer?) -> ()).self
)

let Py_DecRef = PythonLibrary.loadSymbol(
  name: "Py_DecRef",
  type: (@convention(c) (PyObjectPointer?) -> ()).self
)

let PyImport_ImportModule = PythonLibrary.loadSymbol(
  name: "PyImport_ImportModule",
  type: (@convention(c) (PyCCharPointer) -> PyObjectPointer?).self
)

let PyEval_GetBuiltins = PythonLibrary.loadSymbol(
  name: "PyEval_GetBuiltins",
  type: (@convention(c) () -> PyObjectPointer).self
)

let PyErr_Occurred = PythonLibrary.loadSymbol(
  name: "PyErr_Occurred",
  type: (@convention(c) () -> PyObjectPointer?).self
)

let PyErr_Clear = PythonLibrary.loadSymbol(
  name: "PyErr_Clear",
  type: (@convention(c) () -> ()).self
)

let PyErr_Fetch = PythonLibrary.loadSymbol(
  name: "PyErr_Fetch",
  type: (@convention(c) (
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>) -> ()).self
)

let PyDict_New = PythonLibrary.loadSymbol(
  name: "PyDict_New",
  type: (@convention(c) () -> PyObjectPointer?).self
)

let PyDict_SetItem = PythonLibrary.loadSymbol(
  name: "PyDict_SetItem",
  type: (@convention(c) (
    PyObjectPointer?,
    PyObjectPointer,
    PyObjectPointer) -> ()).self
)

let PyObject_GetItem = PythonLibrary.loadSymbol(
  name: "PyObject_GetItem",
  type: (@convention(c) (
    PyObjectPointer,
    PyObjectPointer) -> PyObjectPointer?).self
)

let PyObject_SetItem = PythonLibrary.loadSymbol(
  name: "PyObject_SetItem",
  type: (@convention(c) (
    PyObjectPointer,
    PyObjectPointer,
    PyObjectPointer) -> ()).self
)

let PyObject_DelItem = PythonLibrary.loadSymbol(
  name: "PyObject_DelItem",
  type: (@convention(c) (
    PyObjectPointer,
    PyObjectPointer) -> ()).self
)

let PyObject_Call = PythonLibrary.loadSymbol(
  name: "PyObject_Call",
  type: (@convention(c) (
    PyObjectPointer,
    PyObjectPointer,
    PyObjectPointer?) -> (PyObjectPointer?)).self
)

let PyObject_CallObject = PythonLibrary.loadSymbol(
  name: "PyObject_CallObject",
  type: (@convention(c) (
    PyObjectPointer,
    PyObjectPointer) -> (PyObjectPointer?)).self
)

let PyObject_GetAttrString = PythonLibrary.loadSymbol(
  name: "PyObject_GetAttrString",
  type: (@convention(c) (
    PyObjectPointer,
    PyCCharPointer) -> (PyObjectPointer?)).self
)

let PyObject_SetAttrString = PythonLibrary.loadSymbol(
  name: "PyObject_SetAttrString",
  type: (@convention(c) (
    PyObjectPointer,
    PyCCharPointer,
    PyObjectPointer) -> (Int)).self
)

let PySlice_New = PythonLibrary.loadSymbol(
  name: "PySlice_New",
  type: (@convention(c) (
    PyObjectPointer?,
    PyObjectPointer?,
    PyObjectPointer?) -> (PyObjectPointer?)).self
)

let PyTuple_New = PythonLibrary.loadSymbol(
  name: "PyTuple_New",
  type: (@convention(c) (Int) -> (PyObjectPointer?)).self
)

let PyTuple_SetItem = PythonLibrary.loadSymbol(
  name: "PyTuple_SetItem",
  type: (@convention(c) (
    PyObjectPointer, Int, PyObjectPointer) -> ()).self
)

let PyObject_RichCompareBool = PythonLibrary.loadSymbol(
  name: "PyObject_RichCompareBool",
  type: (@convention(c) (
    PyObjectPointer, PyObjectPointer, Int32) -> (Int32)).self
)

let PyDict_Next = PythonLibrary.loadSymbol(
  name: "PyDict_Next",
  type: (@convention(c) (
    PyObjectPointer, UnsafeMutablePointer<Int>,
    UnsafeMutablePointer<PyObjectPointer?>,
    UnsafeMutablePointer<PyObjectPointer?>) -> (Int32)).self
)

let PyList_New = PythonLibrary.loadSymbol(
  name: "PyList_New",
  type: (@convention(c) (Int) -> (PyObjectPointer?)).self
)

let PyList_SetItem = PythonLibrary.loadSymbol(
  name: "PyList_SetItem",
  type: (@convention(c) (
    PyObjectPointer, Int, PyObjectPointer) -> (Int32)).self
)

let PyBool_FromLong = PythonLibrary.loadSymbol(
  name: "PyBool_FromLong",
  type: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyFloat_AsDouble = PythonLibrary.loadSymbol(
  name: "PyFloat_AsDouble",
  type: (@convention(c) (PyObjectPointer) -> (Double)).self
)

let PyFloat_FromDouble = PythonLibrary.loadSymbol(
  name: "PyFloat_FromDouble",
  type: (@convention(c) (Double) -> (PyObjectPointer)).self
)

let PyInt_AsLong = PythonLibrary.loadSymbol(
  name: "PyLong_AsLong",
  legacyName: "PyInt_AsLong",
  type: (@convention(c) (PyObjectPointer) -> (Int)).self
)

let PyInt_FromLong = PythonLibrary.loadSymbol(
  name: "PyLong_FromLong",
  legacyName: "PyInt_FromLong",
  type: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyInt_AsUnsignedLongMask = PythonLibrary.loadSymbol(
  name: "PyLong_AsUnsignedLongMask",
  legacyName: "PyInt_AsUnsignedLongMask",
  type: (@convention(c) (PyObjectPointer) -> (UInt)).self
)

let PyInt_FromSize_t = PythonLibrary.loadSymbol(
  name: "PyInt_FromLong",
  legacyName: "PyInt_FromSize_t",
  type: (@convention(c) (Int) -> (PyObjectPointer)).self
)

let PyString_AsString = PythonLibrary.loadSymbol(
  name: "PyUnicode_AsUTF8",
  legacyName: "PyString_AsString",
  type: (@convention(c) (
    PyObjectPointer) -> (PyCCharPointer?)).self
)

let PyString_FromStringAndSize = PythonLibrary.loadSymbol(
  name: "PyUnicode_DecodeUTF8",
  legacyName: "PyString_FromStringAndSize",
  type: (@convention(c) (
    PyCCharPointer?, Int) -> (PyObjectPointer?)).self
)

var _Py_ZeroStruct = PythonLibrary.loadSymbol(
  name: "_Py_ZeroStruct",
  type: PyObjectPointer.self
)

var _Py_TrueStruct = PythonLibrary.loadSymbol(
  name: "_Py_TrueStruct",
  type: PyObjectPointer.self
)

var _Py_TrueStructb = PythonLibrary.loadSymbol(
  name: "_Py_TrueStructb",
  type: PyObjectPointer.self
)

var PyBool_Type = PythonLibrary.loadSymbol(
  name: "PyBool_Type",
  type: PyObjectPointer.self
)

var PySlice_Type = PythonLibrary.loadSymbol(
  name: "PySlice_Type",
  type: PyObjectPointer.self
)

let PyNumber_Add = PythonLibrary.loadSymbol(
  name: "PyNumber_Add",
  type: PyBinaryOperation.self
)

let PyNumber_Subtract = PythonLibrary.loadSymbol(
  name: "PyNumber_Subtract",
  type: PyBinaryOperation.self
)

let PyNumber_Multiply = PythonLibrary.loadSymbol(
  name: "PyNumber_Multiply",
  type: PyBinaryOperation.self
)

let PyNumber_TrueDivide = PythonLibrary.loadSymbol(
  name: "PyNumber_TrueDivide",
  type: PyBinaryOperation.self
)

let PyNumber_InPlaceAdd = PythonLibrary.loadSymbol(
  name: "PyNumber_InPlaceAdd",
  type: PyBinaryOperation.self
)

let PyNumber_InPlaceSubtract = PythonLibrary.loadSymbol(
  name: "PyNumber_InPlaceSubtract",
  type: PyBinaryOperation.self
)

let PyNumber_InPlaceMultiply = PythonLibrary.loadSymbol(
  name: "PyNumber_InPlaceMultiply",
  type: PyBinaryOperation.self
)

let PyNumber_InPlaceTrueDivide = PythonLibrary.loadSymbol(
  name: "PyNumber_InPlaceTrueDivide",
  type: PyBinaryOperation.self
)
