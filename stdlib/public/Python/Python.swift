//===-- Python.swift ------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines an interopability layer for talking to Python from Swift.
//
//===----------------------------------------------------------------------===//
//
// The model provided by this file is completely dynamic, it doesn't require
// invasive compiler support.  For a description of how to use this and some
// examples, please see the PythonExample page.
//
//===----------------------------------------------------------------------===//

import PythonWrapper

//===----------------------------------------------------------------------===//
// MARK: PyRef Implementation
//===----------------------------------------------------------------------===//

/// This is a typealias used when we're passing or returning a PyObject
/// pointer with ownership implied.
public typealias OwnedPyObject = UnsafeMutablePointer<PyObject>

/// Primitive reference to a Python value.  This is always non-null and always
/// owning of the underlying value.
///
/// TODO: It sure would be nice to be able to express this as a Swift struct
/// with C++ style user-defined copy ctors, move operators, etc.
final class PyRef {
  private var state : OwnedPyObject

  public init(owned: OwnedPyObject) {
    state = owned
  }
  public init(borrowed: UnsafeMutablePointer<PyObject>) {
    state = borrowed
    Py_IncRef(state)
  }
  deinit {
    Py_DecRef(state)
  }

  public var borrowedPyObject : UnsafeMutablePointer<PyObject> {
    return state
  }

  public var ownedPyObject : OwnedPyObject {
    Py_IncRef(state)
    return state
  }
}

//===----------------------------------------------------------------------===//
// MARK: PyVal type
//===----------------------------------------------------------------------===//

/// This is the currency type for Python object references.  It is passed to and
/// returned from Python calls and member references, and is overloaded to
/// support the standard operations that Python supports.
public struct PyVal {
  /// This is the actual handle to a Python value that we represent.
  fileprivate var state : PyRef

  init(_ value : PyRef) {
    state = value
  }
  public init(owned: OwnedPyObject) {
    state = PyRef(owned: owned)
  }
  public init(borrowed: UnsafeMutablePointer<PyObject>) {
    state = PyRef(borrowed: borrowed)
  }

  fileprivate var borrowedPyObject : UnsafeMutablePointer<PyObject> {
    return state.borrowedPyObject
  }
  fileprivate var ownedPyObject : OwnedPyObject {
    return state.ownedPyObject
  }
}

/// Make "print(python)" print a pretty form of the PyVal.
extension PyVal : CustomStringConvertible {
  public var description: String {
    // We describe a Python value to the REPL and Playgrounds with the str(x)
    // call, just like Python's REPL does.  'str' is designed to be readable,
    // and using 'repr' takes WAY too long for large values because it is
    // designed to faithfully represent the value.
    return String(Python.str.call(args: self))!
  }
}

// Make PyVal's show up nicely in the Xcode Playground results sidebar.
extension PyVal : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

//===----------------------------------------------------------------------===//
// MARK: PythonConvertible Protocol
//===----------------------------------------------------------------------===//

public protocol PythonConvertible {
  /// Python convertible values may be converted from Python, but these
  /// conversions can fail.
  init?(_ value : PyVal)

  /// Python convertible values may always be converted to Python.
  var pythonValue : PyVal { get }
}

// You can explicitly convert any PythonConvertible to a PyVal.
extension PyVal {
  public init<T : PythonConvertible>(_ value : T) {
    self.init(value.pythonValue)
  }
}

// For our use below, provide helpers to convert PythonConvertible values to
// owned and borrowed references.  These shouldn't be public though.
extension PythonConvertible {
  fileprivate var borrowedPyObject : UnsafeMutablePointer<PyObject> {
    return pythonValue.borrowedPyObject
  }
  fileprivate var ownedPyObject : OwnedPyObject {
    return pythonValue.ownedPyObject
  }
}

// PyRef and PyVal are trivially PythonConvertible
extension PyRef : PythonConvertible {
  public convenience init(_ value : PyVal) {
    self.init(owned: value.ownedPyObject)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonValue : PyVal { return PyVal(self) }
}

extension PyVal : PythonConvertible {
  public init(_ value : PyVal) {
    self.init(value.state)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonValue : PyVal { return self }
}

//===----------------------------------------------------------------------===//
// MARK: ThrowingPyVal Implementation
//===----------------------------------------------------------------------===//

extension PyVal {
  /// Return a version of this value that may be called.  It throws a Swift
  /// error if the underlying Python function throws a Python exception.
  public var throwing : ThrowingPyVal {
    return ThrowingPyVal(self)
  }
}

/// This represents the result of a failable operation when working with
/// python values.
public enum PythonError : Error {
  /// This represents an exception thrown out of a Python API.  This can occur
  /// on calls.
  case exception(_: PyVal)

  /// A call on the specified value failed, e.g. because it wasn't a Python
  /// callable, because the wrong number of parameters were provided, or because
  /// a keyword argument was specified multiple times.
  case invalidCall(_: PyVal)

  /// A call(member: x, ...) operation failed to look up the 'x' member.
  case invalidMember(_: String)

  /// An access to an invalid tuple member was performed.
  case invalidTupleMember(base: PyVal)
}

extension PythonError : CustomStringConvertible {
  public var description: String {
    switch self {
    case .exception(let p): return "exception: \(p)"
    case .invalidCall(let p): return "invalidCall: \(p)"
    case .invalidMember(let m): return "invalidMember: \(m)"
    case .invalidTupleMember(let p): return "invalidTupleMember: \(p)"
    }
  }
}

// Reflect a Python error (which must be active) into a Swift error if one is
// active.
private func throwPythonErrorIfPresent() throws {
  if PyErr_Occurred() == nil { return }

  var type : UnsafeMutablePointer<PyObject>?
  var value : UnsafeMutablePointer<PyObject>?
  var traceback : UnsafeMutablePointer<PyObject>?
  // This takes the exception, clearing the exception state.
  PyErr_Fetch(&type, &value, &traceback)

  // The value for the exception may not be set, but the type always should be.
  let r = PyVal(owned: value ?? type!)
  throw PythonError.exception(r)
}

/// This type is a PyVal produced when the user cares about getting an
/// exception out of a call.  We wrap this up and reflect it back as a thrown
/// Swift error.
public struct ThrowingPyVal {
  private var state : PyVal

  fileprivate init(_ value : PyVal) {
    state = value
  }

  /// Call self, which must be a Python Callable.  If the callee throws a Python
  /// exception, if the callee isn't callable, or if there is some other
  /// problem, we throw a Swift error.
  @discardableResult
  public func call<T : PythonConvertible>
    (argArray args: [T],
     kwargs: [(String, PythonConvertible)] = []) throws -> PyVal {
    // Make sure state errors are not around.
    if PyErr_Occurred() != nil {
      // FIXME: This should be an assert, but the failure mode in playgrounds
      // is just awful.
      print("Python error state must be clear")
      fatalError()
    }

    // Produce a dictionary for keyword arguments if any are present.
    var kwdictObj : OwnedPyObject? = nil
    if !kwargs.isEmpty {
      kwdictObj = PyDict_New()!
      for (key, val) in kwargs {
        // FIXME: What if there are two identical keywords provided?
        let k = PyVal(key).ownedPyObject
        let v = val.ownedPyObject
        PyDict_SetItem(kwdictObj, k, v)
        Py_DecRef(k)
        Py_DecRef(v)
      }
    }
    defer { Py_DecRef(kwdictObj) }  // Py_DecRef is nil safe.

    // Non-keyword arguments are passed as a tuple of values.
    let argTuple = PyTuple_New(args.count)!
    for (idx, elt) in args.enumerated() {
      // This 'steals' the element stored into the tuple.
      PyTuple_SetItem(argTuple, idx, elt.ownedPyObject)
    }
    // PyObject_Call doesn't take ownership of the arg tuple.
    defer { Py_DecRef(argTuple) }

    // Python calls always return a non-null value when successful.  If the
    // Python function produces the equivalent of C "void", it returns the None
    // value.  A null result of PyObjectCall happens when there is an error,
    // like 'self' not being a Python callable.
    let selfObj = state.ownedPyObject
    defer { Py_DecRef(selfObj) }

    guard let resultPtr = PyObject_Call(selfObj, argTuple, kwdictObj) else {
      // Translate a Python exception into a Swift error if one was thrown.
      try throwPythonErrorIfPresent()
      throw PythonError.invalidCall(state)
    }

    return PyVal(owned: resultPtr)
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  public func call(args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) throws -> PyVal {
    return try call(argArray: args.map { $0.pythonValue }, kwargs: kwargs)
  }

  // Call a member, as in self.foo(...)
  @discardableResult
  public func call<T: PythonConvertible>
    (member name: String, argArray args: [T],
     kwargs: [(String, PythonConvertible)] = []) throws -> PyVal {
    // If the member lookup fails, reflect it as a Swift error.
    guard let callee = state.checking.get(member: name) else {
      throw PythonError.invalidMember(name)
    }
    return try callee.throwing.call(argArray: args, kwargs: kwargs)
  }
  @discardableResult
  public func call(member name: String,
                   args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) throws -> PyVal {
    return try call(member: name, argArray: args.map { $0.pythonValue },
                    kwargs: kwargs)
  }

  public func get2Tuple() throws -> (PyVal, PyVal) {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1] else {
      throw PythonError.invalidTupleMember(base: state)
    }

    return (elt0, elt1)
  }

  public func get3Tuple() throws -> (PyVal, PyVal, PyVal) {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1], let elt2 = ct[2] else {
      throw PythonError.invalidTupleMember(base: state)
    }

    return (elt0, elt1, elt2)
  }
  public func get4Tuple() throws -> (PyVal, PyVal, PyVal, PyVal) {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1],
          let elt2 = ct[2], let elt3 = ct[3] else {
        throw PythonError.invalidTupleMember(base: state)
    }

    return (elt0, elt1, elt2, elt3)
  }
}


//===----------------------------------------------------------------------===//
// MARK: CheckingPyVal Implementation
//===----------------------------------------------------------------------===//

extension PyVal {
  /// Return a version of this value that may have member access operations
  /// performed on it.  These operations can fail and return an optional when
  /// the underlying Python operations fail.
  public var checking : CheckingPyVal {
    return CheckingPyVal(self)
  }
}

/// This type temporarily wraps a PyVal when the user cares about turning an
/// operation (like a member lookup or subscript) into a failable operation that
/// returns an optional.
public struct CheckingPyVal {
  private var state : PyVal

  fileprivate init(_ value : PyVal) {
    state = value
  }

  public func get(member: String) -> PyVal? {
    let selfObj = state.ownedPyObject
    defer { Py_DecRef(selfObj) }

    guard let result = PyObject_GetAttrString(selfObj, member) else {
      PyErr_Clear()
      return nil
    }
    return PyVal(owned: result)  // PyObject_GetAttrString returns +1 result.
  }

  /// Swift subscripts cannot throw yet, so model this as returning an optional
  /// reference.
  public subscript(array idx : [PythonConvertible]) -> PyVal? {
    get {
      let pyIndexObj = flattenSubscriptIndices(idx)
      let selfObj = state.ownedPyObject
      defer {
        Py_DecRef(pyIndexObj)
        Py_DecRef(selfObj)
      }

      // PyObject_GetItem returns +1 reference.
      if let result = PyObject_GetItem(selfObj, pyIndexObj) {
        return PyVal(owned: result)
      }

      PyErr_Clear()
      return nil
    }
    nonmutating set {
      let pyIndexObj = flattenSubscriptIndices(idx)
      let selfObj = state.ownedPyObject
      defer {
        Py_DecRef(pyIndexObj)
        Py_DecRef(selfObj)
      }

      if let newValue = newValue {
        let newValueObj = newValue.ownedPyObject
        PyObject_SetItem(selfObj, pyIndexObj, newValueObj)
        Py_DecRef(newValueObj)
      } else {
        // Assigning nil deletes the key, just like Swift dictionaries.
        PyObject_DelItem(selfObj, pyIndexObj)
      }
    }
  }

  public subscript(idx : PythonConvertible...) -> PyVal? {
    get {
      return self[array: idx]
    }
    nonmutating set {
      self[array: idx] = newValue
    }
  }


  public func get2Tuple() -> (PyVal, PyVal)? {
    guard let elt0 = self[0], let elt1 = self[1] else {
      return nil
    }

    return (elt0, elt1)
  }

  public func get3Tuple() -> (PyVal, PyVal, PyVal)? {
    guard let elt0 = self[0], let elt1 = self[1], let elt2 = self[2] else {
      return nil
    }

    return (elt0, elt1, elt2)
  }
  public func get4Tuple() -> (PyVal, PyVal, PyVal, PyVal)? {
    guard let elt0 = self[0], let elt1 = self[1],
          let elt2 = self[2], let elt3 = self[3] else {
      return nil
    }

    return (elt0, elt1, elt2, elt3)
  }
}

//===----------------------------------------------------------------------===//
// MARK: Core PyVal API
//===----------------------------------------------------------------------===//

/// Turn an array of indices into a flattened index reference as a +1 Python
/// object.
private
func flattenSubscriptIndices(_ idx : [PythonConvertible]) -> OwnedPyObject {
  if idx.count == 1 {
    return idx[0].ownedPyObject
  }
  return pyTuple(idx.map { $0.pythonValue })
}

extension PyVal {
  public func get(member: String) -> PyVal {
    return checking.get(member: member)!
  }

  // This returns true on error and false on success, gross.
  private func failableSet(member: String, _ value: PyVal) -> Bool {
    let selfObj = self.ownedPyObject, valueObj = value.ownedPyObject
    defer {
      Py_DecRef(selfObj)
      Py_DecRef(valueObj)
    }

    if PyObject_SetAttrString(selfObj, member, valueObj) == -1 {
      PyErr_Clear()
      return true
    }
    return false
  }

  public func set(member: String, _ value: PyVal) {
    let failed = failableSet(member: member, value)
    assert(!failed, "setting an invalid Python member")
  }

  // Dictionary lookups return optionals because they can always fail if the
  // key is not present.
  public func get(dictMember: PyVal) -> PyVal? {

    let selfObj = self.ownedPyObject
    let keyObj = dictMember.ownedPyObject
    defer {
      Py_DecRef(selfObj)
      Py_DecRef(keyObj)
    }

    // PyDict_GetItem returns +0 result.
    return PyVal(borrowed: PyDict_GetItem(selfObj, keyObj))
  }

  public subscript(idx : PythonConvertible...) -> PyVal {
    get {
      return self.checking[array: idx]!
    }
    nonmutating set {
      self.checking[array: idx] = newValue
    }
  }

  // Helpers for destructuring tuples
  public func get2Tuple() -> (PyVal, PyVal) {
    return (self[0], self[1])
  }
  public func get3Tuple() -> (PyVal, PyVal, PyVal) {
    return (self[0], self[1], self[2])
  }
  public func get4Tuple() -> (PyVal, PyVal, PyVal, PyVal) {
    return (self[0], self[1], self[2], self[3])
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  public func call<T : PythonConvertible>
    (argArray args: [T], kwargs: [(String, PythonConvertible)] = []) -> PyVal {
    return try! self.throwing.call(argArray: args, kwargs: kwargs)
  }
  /// Call self, which must be a Python Callable.
  @discardableResult
  public func call(args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) -> PyVal {
    return try! self.throwing.call(argArray: args.map { $0.pythonValue },
                                   kwargs: kwargs)
  }

  /// Call a member.
  @discardableResult
  public func call<T: PythonConvertible>
    (member name: String, argArray args: [T],
                   kwargs: [(String, PythonConvertible)] = []) -> PyVal {
    return try! self.throwing.call(member: name, argArray: args, kwargs: kwargs)
  }
  @discardableResult
  public func call(member name: String, args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) -> PyVal {
    return try! self.throwing.call(member: name,
                                   argArray: args.map { $0.pythonValue },
                                   kwargs: kwargs)
  }
}

//===----------------------------------------------------------------------===//
// MARK: `Python` Type Implementation
//===----------------------------------------------------------------------===//

// We want the user to iteract with Python at the top level through a
// Python.import("foo") sort of statement.  The problem with this is that we
// want to ensure that python is initialized on all uses of the Python interface
// and we want to allow this interface to eventually conform to a protocol that
// allows dynamic lookup of its members (through the Builtin map).  This will
// require an instance named "Python", which is unconventional, but does what we
// need.
public let Python = PythonInterface()

public struct PythonInterface {
  /// This is a hash table of the builtins provided by the Python language.
  public let builtins : PyVal

  init() {
    Py_Initialize()   // Initialize python
    builtins = PyVal(borrowed: PyEval_GetBuiltins())
  }

  public func `import`(_ name: String) throws -> PyVal {
    let module = PyImport_ImportModule(name)
    if module == nil {
      try throwPythonErrorIfPresent()
    }
    return PyVal(owned: module!)
  }

  public func setPath(_ path: String) {
    path.withCString {
      PySys_SetPath(UnsafeMutablePointer(mutating: $0))
    }
  }

  // TODO: Make the PythonInterface type itself `DynamicCallable`, so that
  // things like Python.open" naturally resolve to Python.get(member: "open")
  // and all the builtin functions are therefore available naturally and don't
  // have to be enumerated here.
  public var isinstance : PyVal { return builtins["isinstance"] }
  public var len : PyVal { return builtins["len"] }
  public var open : PyVal { return builtins["open"] }
  public var print : PyVal { return builtins["print"] }
  public var range : PyVal { return builtins["range"] }
  public var repr : PyVal { return builtins["repr"] }
  public var str : PyVal { return builtins["str"] }
  public var type : PyVal { return builtins["type"] }
}


//===----------------------------------------------------------------------===//
// MARK: Python List, Dictionary, Slice and Tuple Helpers
//===----------------------------------------------------------------------===//

private func pyList<T : Collection>(_ vals : T) -> OwnedPyObject
  where T.Element : PythonConvertible, T.IndexDistance == Int {
  let list = PyList_New(vals.count)!
  for (idx, elt) in vals.enumerated() {
    // This steals the reference of the value stored.
    PyList_SetItem(list, idx, elt.ownedPyObject)
  }
  return list
}

private func pyDict<T: Sequence,
                    U: PythonConvertible,
                    V: PythonConvertible>(_ elts : T) -> OwnedPyObject
  where T.Element == (U, V) {
  let dict = PyDict_New()!
  for (key, val) in elts {
    // FIXME: This can fail if the key isn't hashable.  What if there are two
    // identical keys?
    let k = key.ownedPyObject
    let v = val.ownedPyObject
    PyDict_SetItem(dict, k, v)
    Py_DecRef(k)
    Py_DecRef(v)
  }
  return dict
}

private func pySlice(_ start: PythonConvertible, _ end: PythonConvertible,
                     _ step : PythonConvertible? = nil) -> OwnedPyObject {
  let startP = start.ownedPyObject
  let endP = end.ownedPyObject
  let stepP = step?.ownedPyObject

  // PySlice_New takes each operand at +0, and returns +1.
  let result = PySlice_New(startP, endP, stepP)!

  Py_DecRef(startP)
  Py_DecRef(endP)
  Py_DecRef(stepP)  // Py_DecRef is nil safe.
  return result
}

// Create a Python tuple object with the specified elements.
private func pyTuple<T : Collection>(_ vals : T) -> OwnedPyObject
  where T.Element: PythonConvertible, T.IndexDistance == Int {

  let t = PyTuple_New(vals.count)!
  for (idx, elt) in vals.enumerated() {
    // This steals the reference of the value stored.
    PyTuple_SetItem(t, idx, elt.ownedPyObject)
  }
  return t
}


// FIXME: These creation functions should go away when we have conditional
// conformances.  Conditional conformances can't come fast enough!
//
extension PyVal {
  public init(array elements: PythonConvertible...) {
    self.init(arrayContentsOf: elements)
  }
  public init<T : Collection>(arrayContentsOf elements: T)
    where T.Element == PythonConvertible, T.IndexDistance == Int {
    self.init(arrayContentsOf: elements.map { $0.pythonValue })
  }
  public init<T : Collection>(arrayContentsOf array: T)
    where T.Element: PythonConvertible, T.IndexDistance == Int {
    self.init(owned: pyList(array))
  }

  public init(dict elts: (PythonConvertible, PythonConvertible)...) {
    self.init(dictContentsOf: elts.map { ($0.0.pythonValue, $0.1.pythonValue) })
  }
  public init<T: Sequence, U: PythonConvertible,
              V: PythonConvertible>(dictContentsOf array: T)
    where T.Element == (U, V) {
    self.init(owned: pyDict(array))
  }
  public init<T: PythonConvertible,
              U: PythonConvertible>(dictContentsOf dict: [T: U]) {
    self.init(PyVal(owned: pyDict(dict.map { ($0.key, $0.value) })))
  }
  public init<T: PythonConvertible>
      (dictContentsOf dict: [T: PythonConvertible]) {
    self.init(PyVal(owned: pyDict(dict.map { ($0.key.pythonValue,
                                              $0.value.pythonValue) })))
  }

  /// FIXME: This should be subsumed by Swift ranges + strides.  Python has a
  /// very extravagent model though, it isn't clear how best to represent this
  /// in Swift.
  ///
  /// Initial thoughts are that we should sugar the obvious cases (so you can
  /// use 0...100 in a subscript) but then provide this member for the fully
  /// general case.
  ///
  /// We also need conditional conformances to allow range if PyVal's to be a
  /// Slice.  We can probably get away with a bunch of overloads for now given
  /// that slices are typically used with concrete operands.
  public init(slice start: PythonConvertible, _ end: PythonConvertible,
              _ step : PythonConvertible? = nil) {
    self.init(owned: pySlice(start, end, step))
  }

  // Tuples will require explicit support until Tuples can conform to protocols,
  // which is probably a long time.
  public init(tuple elts: PythonConvertible...) {
    self.init(tupleContentsOf: elts)
  }
  public init<T : Collection>(tupleContentsOf elts: T)
      where T.Element == PythonConvertible, T.IndexDistance == Int {
    self.init(owned: pyTuple(elts.map { $0.pythonValue }))
  }
  public init<T : Collection>(tupleContentsOf elts: T)
      where T.Element: PythonConvertible, T.IndexDistance == Int {
    self.init(owned: pyTuple(elts))
  }
}


//===----------------------------------------------------------------------===//
// MARK: Builtin Swift Types Conformances to PyVal
//===----------------------------------------------------------------------===//

/// Return true if the specified value is an instance of the low-level Python
/// type descriptor passed in as 'type'.
private func isType(_ val : PyVal, type : UnsafeMutableRawPointer) -> Bool {
  let typePyRef = PyVal(borrowed: type.assumingMemoryBound(to: PyObject.self))
  let result = Python.isinstance.call(args: val, typePyRef)

  // We can't use the normal failable Bool initialization from PyVal here,
  // because that would cause an infinite loop, calling back into isType.
  let pyObj = result.ownedPyObject
  defer { Py_DecRef(pyObj) }

  // Anything not zero is truthy.
  return !equalPointers(pyObj, &_Py_ZeroStruct)
}

private func equalPointers(_ x : UnsafeMutablePointer<PyObject>,
                           _ y : UnsafeMutableRawPointer) -> Bool {
  return x == y.assumingMemoryBound(to: PyObject.self)
}

extension Bool : PythonConvertible {
  public init?(_ python: PyVal) {
    guard isType(python, type: &PyBool_Type) else { return nil }

    let pyObj = python.ownedPyObject
    defer { Py_DecRef(pyObj) }

    self = equalPointers(pyObj, &_Py_TrueStruct)
  }

  public var pythonValue : PyVal {
    _ = Python // ensure Python is initialized.
    return PyVal(owned: PyBool_FromLong(self ? 1 : 0))
  }
}

extension String : PythonConvertible {
  public init?(_ python: PyVal) {
    let pyObj = python.ownedPyObject
    defer { Py_DecRef(pyObj) }

    guard let cStringVal = PyString_AsString(pyObj) else {
      PyErr_Clear()
      return nil
    }

    self = String(cString: cStringVal)
  }
  public var pythonValue : PyVal {
    _ = Python // ensure Python is initialized.
    let v = self.utf8CString.withUnsafeBufferPointer {
      PyString_FromStringAndSize($0.baseAddress, $0.count-1 /*trim \0*/)!
    }
    return PyVal(owned: v)
  }
}

extension Int : PythonConvertible {
  public init?(_ python: PyVal) {
    let pyObj = python.ownedPyObject
    defer { Py_DecRef(pyObj) }

    assert(PyErr_Occurred() == nil,
           "Python error occurred somewhere but wasn't handled")

    let value = PyInt_AsLong(pyObj)

    // PyInt_AsLong return -1 and sets an error if the Python value isn't
    // integer compatible.
    if value == -1 && PyErr_Occurred() != nil {
      PyErr_Clear()
      return nil
    }

    self = value
  }
  public var pythonValue : PyVal {
    _ = Python // ensure Python is initialized.
    return PyVal(owned: PyInt_FromLong(self))
  }
}

extension UInt : PythonConvertible {
  public init?(_ python: PyVal) {
    let pyObj = python.ownedPyObject
    defer { Py_DecRef(pyObj) }

    assert(PyErr_Occurred() == nil,
           "Python error occurred somewhere but wasn't handled")

    let value = PyInt_AsUnsignedLongMask(pyObj)

    // PyInt_AsUnsignedLongMask isn't documented as such, but in fact it does
    // return -1 and sets an error if the Python value isn't
    // integer compatible.
    if value == ~0 && PyErr_Occurred() != nil {
      PyErr_Clear()
      return nil
    }

    self = value
  }

  public var pythonValue : PyVal {
    _ = Python // ensure Python is initialized.
    return PyVal(owned: PyInt_FromSize_t(Int(self)))
  }
}

extension Double : PythonConvertible {
  public init?(_ python: PyVal) {
    let pyObj = python.ownedPyObject
    defer { Py_DecRef(pyObj) }

    assert(PyErr_Occurred() == nil,
           "Python error occurred somewhere but wasn't handled")

    let value = PyFloat_AsDouble(pyObj)

    // PyFloat_AsDouble return -1 and sets an error
    // if the Python value isn't float compatible.
    if value == -1 && PyErr_Occurred() != nil {
      PyErr_Clear()
      return nil
    }

    self = value
  }

  public var pythonValue : PyVal {
    _ = Python // ensure Python is initialized.
    return PyVal(owned: PyFloat_FromDouble(self))
  }
}

//===----------------------------------------------------------------------===//
// Sized integer and Float conformances.
//===----------------------------------------------------------------------===//

// Any integer can conform to PyVal by using the Int/UInt implementation.
public protocol IntXPyVal : PythonConvertible, FixedWidthInteger {
  associatedtype ParentPythonIntType : PythonConvertible, FixedWidthInteger
}
extension IntXPyVal {
  public init?(_ python: PyVal) {
    guard let i = ParentPythonIntType(python) else { return nil }
    self = Self(i)
  }
  public var pythonValue : PyVal {
    return ParentPythonIntType(self).pythonValue
  }
}

extension Int8  : IntXPyVal { public typealias ParentPythonIntType = Int }
extension Int16 : IntXPyVal { public typealias ParentPythonIntType = Int }
extension Int32 : IntXPyVal { public typealias ParentPythonIntType = Int }
extension Int64 : IntXPyVal { public typealias ParentPythonIntType = Int }
extension UInt8  : IntXPyVal { public typealias ParentPythonIntType = UInt }
extension UInt16 : IntXPyVal { public typealias ParentPythonIntType = UInt }
extension UInt32 : IntXPyVal { public typealias ParentPythonIntType = UInt }
extension UInt64 : IntXPyVal { public typealias ParentPythonIntType = UInt }

extension Float : PythonConvertible {
  public init?(_ python: PyVal) {
    guard let v = Double(python) else { return nil }
    self = Float(v)
  }
  public var pythonValue : PyVal {
    return Double(self).pythonValue
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators and Conformances
//===----------------------------------------------------------------------===//

public func +(lhs: PyVal, rhs: PyVal) -> PyVal {
  return lhs.call(member: "__add__", args: rhs)
}
public func -(lhs: PyVal, rhs: PyVal) -> PyVal {
  return lhs.call(member: "__sub__", args: rhs)
}
public func *(lhs: PyVal, rhs: PyVal) -> PyVal {
  return lhs.call(member: "__mul__", args: rhs)
}
public func /(lhs: PyVal, rhs: PyVal) -> PyVal {
  return lhs.call(member: "__truediv__", args: rhs)
}
public func +=(lhs: inout PyVal, rhs: PyVal) {
  lhs = lhs + rhs
}
public func -=(lhs: inout PyVal, rhs: PyVal) {
  lhs = lhs - rhs
}
public func *=(lhs: inout PyVal, rhs: PyVal) {
  lhs = lhs * rhs
}
public func /=(lhs: inout PyVal, rhs: PyVal) {
  lhs = lhs / rhs
}

extension PyVal : SignedNumeric {
  public init<T : BinaryInteger>(exactly value: T) {
    self = PyVal(Int(value))
  }
  public typealias Magnitude = PyVal
  public var magnitude: PyVal {
    return self < 0 ? -self : self
  }
}


// Define conformance to Comparable and Equatable
extension PyVal : Hashable, Comparable, Equatable {
  public static func <(lhs: PyVal, rhs: PyVal) -> Bool {
    if let cmp = lhs.checking.get(member: "__cmp__") {
      guard let cmpResult = Int(cmp.call(args: rhs)) else {
        fatalError("cannot use __cmp__ on \(lhs) and \(rhs)")
      }
      return cmpResult == -1
    }
    guard let ltResult = Bool(lhs.call(member: "__lt__", args: rhs)) else {
      fatalError("cannot use __lt__ on \(lhs) and \(rhs)")
    }
    return ltResult
  }
  public static func==(lhs: PyVal, rhs: PyVal) -> Bool {
    if let cmp = lhs.checking.get(member: "__cmp__") {
      guard let cmpResult = Int(cmp.call(args: rhs)) else {
        fatalError("cannot use __cmp__ on \(lhs) and \(rhs)")
      }
      return cmpResult == 0
    }
    guard let eqResult = Bool(lhs.call(member: "__eq__", args: rhs)) else {
      fatalError("cannot use __eq__ on \(lhs) and \(rhs)")
    }
    return eqResult
  }
  public var hashValue: Int {
    guard let hash = Int(self.call(member: "__hash__")) else {
      fatalError("cannot use __hash__ on \(self)")
    }
    return hash
  }
}

extension PyVal: MutableCollection {
  public typealias Index = PyVal
  public typealias Element = PyVal

  public var startIndex: Index { return PyVal(0) }
  public var endIndex: Index {
    return Python.len.call(args: self)
  }

  public subscript(index : PyVal) -> PyVal {
    get {
      return self[index as PythonConvertible]
    }
    set {
      self[index as PythonConvertible] = newValue
    }
  }

  // Method that returns the next index when iterating
  public func index(after i: Index) -> Index {
    return i + PyVal(1)
  }
}

// Simple Literal Conformances
extension PyVal : ExpressibleByBooleanLiteral,
                  ExpressibleByIntegerLiteral,
                  ExpressibleByFloatLiteral,
                  ExpressibleByStringLiteral {
    public init(booleanLiteral value: Bool) {
      self = PyVal(value)
    }
    public init(integerLiteral value: Int) {
      self = PyVal(value)
    }
    public init(floatLiteral value: Double) {
      self = PyVal(value)
    }
    public init(stringLiteral value: String) {
      self = PyVal(value)
  }
}

// Collection Literal Conformances
extension PyVal : ExpressibleByArrayLiteral,
                  ExpressibleByDictionaryLiteral {
  public init(arrayLiteral elements: PythonConvertible...) {
    self = PyVal(arrayContentsOf: elements)
  }
  public typealias Key = PyVal
  public typealias Value = PyVal
  public init(dictionaryLiteral elements: (PyVal, PyVal)...) {
    self = PyVal(dictContentsOf: elements)
  }
}
