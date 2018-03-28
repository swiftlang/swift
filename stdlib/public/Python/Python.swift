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

import CPython

//===----------------------------------------------------------------------===//
// MARK: PyReference Implementation
//===----------------------------------------------------------------------===//

/// This is a typealias used when we're passing or returning a PyObject
/// pointer with ownership implied.
public typealias OwnedPyObject = UnsafeMutablePointer<PyObject>

/// Primitive reference to a Python value.  This is always non-null and always
/// owning of the underlying value.
///
/// - TODO: It sure would be nice to be able to express this as a Swift struct
///   with C++ style user-defined copy ctors, move operators, etc.
@_versioned @_fixed_layout
final class PyReference {
  private var state: OwnedPyObject

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

  public var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return state
  }

  public var ownedPyObject: OwnedPyObject {
    Py_IncRef(state)
    return state
  }
}

//===----------------------------------------------------------------------===//
// MARK: PyValue type
//===----------------------------------------------------------------------===//

/// This is the currency type for Python object references.  It is passed to and
/// returned from Python calls and member references, and is overloaded to
/// support the standard operations that Python supports.
@dynamicMemberLookup
@_fixed_layout
public struct PyValue {
  /// This is the actual handle to a Python value that we represent.
  fileprivate var state: PyReference

  @_versioned
  init(_ value: PyReference) {
    state = value
  }

  public init(owned: OwnedPyObject) {
    state = PyReference(owned: owned)
  }

  public init(borrowed: UnsafeMutablePointer<PyObject>) {
    state = PyReference(borrowed: borrowed)
  }

  fileprivate var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return state.borrowedPyObject
  }

  fileprivate var ownedPyObject: OwnedPyObject {
    return state.ownedPyObject
  }
}

/// Make "print(python)" print a pretty form of the PyValue.
extension PyValue : CustomStringConvertible {
  public var description: String {
    // We describe a Python value to the REPL and Playgrounds with the str(x)
    // call, just like Python's REPL does.  'str' is designed to be readable,
    // and using 'repr' takes WAY too long for large values because it is
    // designed to faithfully represent the value.
    return String(Python.str.call(with: self))!
  }
}

// Make PyValue's show up nicely in the Xcode Playground results sidebar.
extension PyValue : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

// Mirror representation, used by debugger/REPL.
extension PyValue : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: [], displayStyle: .struct)
  }
}

//===----------------------------------------------------------------------===//
// MARK: PythonConvertible Protocol
//===----------------------------------------------------------------------===//

public protocol PythonConvertible {
  /// Python convertible values may be converted from Python, but these
  /// conversions can fail.
  init?(_ value: PyValue)

  /// Python convertible values may always be converted to Python.
  var pythonValue: PyValue { get }
}

// You can explicitly convert any PythonConvertible to a PyValue.
extension PyValue {
  public init<T : PythonConvertible>(_ value: T) {
    self.init(value.pythonValue)
  }
}

// For our use below, provide helpers to convert PythonConvertible values to
// owned and borrowed references.  These shouldn't be public though.
fileprivate extension PythonConvertible {
  var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return pythonValue.borrowedPyObject
  }

  var ownedPyObject: OwnedPyObject {
    return pythonValue.ownedPyObject
  }
}

// PyReference and PyValue are trivially PythonConvertible
extension PyReference : PythonConvertible {
  public convenience init(_ value: PyValue) {
    self.init(owned: value.ownedPyObject)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonValue: PyValue {
    return PyValue(self)
  }
}

extension PyValue : PythonConvertible {
  public init(_ value: PyValue) {
    self.init(value.state)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonValue: PyValue { return self }
}

//===----------------------------------------------------------------------===//
// MARK: ThrowingPyValue Implementation
//===----------------------------------------------------------------------===//

public extension PyValue {
  /// Return a version of this value that may be called.  It throws a Swift
  /// error if the underlying Python function throws a Python exception.
  var throwing: ThrowingPyValue {
    return ThrowingPyValue(self)
  }
}

/// This represents the result of a failable operation when working with
/// Python values.
@_fixed_layout
public enum PythonError : Error {
  /// This represents an exception thrown out of a Python API.  This can occur
  /// on calls.
  case exception(PyValue)

  /// A call on the specified value failed, e.g. because it wasn't a Python
  /// callable, because the wrong number of parameters were provided, or because
  /// a keyword argument was specified multiple times.
  case invalidCall(PyValue)

  /// A callMember(x, ...) operation failed to look up the 'x' member.
  case invalidMember(String)

  /// Importing the module with the returned name failed.
  case invalidModule(String)
}

extension PythonError : Equatable {
  public static func == (lhs: PythonError, rhs: PythonError) -> Bool {
    switch (lhs, rhs) {
    case let (.invalidCall(leftVal), .invalidCall(rightVal)),
         let (.exception(leftVal), .exception(rightVal)):
      return leftVal == rightVal
    case let (.invalidModule(leftString), .invalidModule(rightString)),
         let (.invalidMember(leftString), .invalidMember(rightString)):
      return leftString == rightString
    case (.exception(_), _),
         (.invalidCall(_), _),
         (.invalidMember(_), _),
         (.invalidModule(_), _):
      return false
    }
  }
}

extension PythonError : CustomStringConvertible {
  public var description: String {
    switch self {
    case .exception(let p): return "exception: \(p)"
    case .invalidCall(let p): return "invalidCall: \(p)"
    case .invalidMember(let m): return "invalidMember: \(m)"
    case .invalidModule(let m): return "invalidModule: \(m)"
    }
  }
}

// Reflect a Python error (which must be active) into a Swift error if one is
// active.
private func throwPythonErrorIfPresent() throws {
  if PyErr_Occurred() == nil { return }

  var type: UnsafeMutablePointer<PyObject>?
  var value: UnsafeMutablePointer<PyObject>?
  var traceback: UnsafeMutablePointer<PyObject>?
  // This takes the exception, clearing the exception state.
  PyErr_Fetch(&type, &value, &traceback)

  // The value for the exception may not be set, but the type always should be.
  let r = PyValue(owned: value ?? type!)
  throw PythonError.exception(r)
}

/// This type is a PyValue produced when the user cares about getting an
/// exception out of a call.  We wrap this up and reflect it back as a thrown
/// Swift error.
@_fixed_layout
public struct ThrowingPyValue {
  private var state: PyValue

  fileprivate init(_ value: PyValue) {
    state = value
  }

  /// Call self, which must be a Python Callable.  If the callee throws a Python
  /// exception, if the callee isn't callable, or if there is some other
  /// problem, we throw a Swift error.
  @discardableResult
  public func call<T : PythonConvertible>(
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PyValue {
    // Make sure state errors are not around.
    if PyErr_Occurred() != nil {
      // FIXME: This should be an assert, but the failure mode in playgrounds
      // is just awful.
      fatalError("Python error state must be clear")
    }

    // Produce a dictionary for keyword arguments if any are present.
    var kwdictObject: OwnedPyObject? = nil
    if !kwargs.isEmpty {
      kwdictObject = PyDict_New()!
      for (key, val) in kwargs {
        // FIXME: What if there are two identical keywords provided?
        let k = PyValue(key).ownedPyObject
        let v = val.ownedPyObject
        PyDict_SetItem(kwdictObject, k, v)
        Py_DecRef(k)
        Py_DecRef(v)
      }
    }
    defer { Py_DecRef(kwdictObject) }  // Py_DecRef is nil safe.

    // Non-keyword arguments are passed as a tuple of values.
    let argTuple = pyTuple(args)
    defer { Py_DecRef(argTuple) }

    // Python calls always return a non-null value when successful.  If the
    // Python function produces the equivalent of C "void", it returns the None
    // value.  A null result of PyObjectCall happens when there is an error,
    // like 'self' not being a Python callable.
    let selfObject = state.ownedPyObject
    defer { Py_DecRef(selfObject) }

    guard let result = PyObject_Call(selfObject, argTuple, kwdictObject) else {
      // Translate a Python exception into a Swift error if one was thrown.
      try throwPythonErrorIfPresent()
      throw PythonError.invalidCall(state)
    }

    return PyValue(owned: result)
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  public func call(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PyValue {
    return try call(argArray: args.map { $0.pythonValue }, kwargs: kwargs)
  }

  // Call a member, as in self.foo(...)
  @discardableResult
  public func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PyValue {
    // If the member lookup fails, reflect it as a Swift error.
    guard let callee = state.checking[dynamicMember: name] else {
      throw PythonError.invalidMember(name)
    }
    return try callee.throwing.call(argArray: args, kwargs: kwargs)
  }

  @discardableResult
  public func callMember(
    _ name: String,
    with args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PyValue {
    return try callMember(name, argArray: args.map { $0.pythonValue },
                          kwargs: kwargs)
  }

  public var tuple2: (PyValue, PyValue)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  public var tuple3: (PyValue, PyValue, PyValue)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1], let elt2 = ct[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  public var tuple4: (PyValue, PyValue, PyValue, PyValue)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1],
          let elt2 = ct[2], let elt3 = ct[3] else {
        return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}


//===----------------------------------------------------------------------===//
// MARK: CheckingPyValue Implementation
//===----------------------------------------------------------------------===//

public extension PyValue {
  /// Return a version of this value that may have member access operations
  /// performed on it.  These operations can fail and return an optional when
  /// the underlying Python operations fail.
  var checking: CheckingPyValue {
    return CheckingPyValue(self)
  }
}

/// This type temporarily wraps a PyValue when the user cares about turning an
/// operation (like a member lookup or subscript) into a failable operation that
/// returns an optional.
@dynamicMemberLookup
@_fixed_layout
public struct CheckingPyValue {
  private var state: PyValue

  fileprivate init(_ value: PyValue) {
    state = value
  }

  public subscript(dynamicMember name: String) -> PyValue? {
    get {
      let selfObject = state.ownedPyObject
      defer { Py_DecRef(selfObject) }
      guard let result = PyObject_GetAttrString(selfObject, name) else {
        PyErr_Clear()
        return nil
      }
      return PyValue(owned: result) // PyObject_GetAttrString returns +1 result.
    }
  }

  /// Swift subscripts cannot throw yet, so model this as returning an optional
  /// reference.
  public subscript(array index: [PythonConvertible]) -> PyValue? {
    get {
      let pyIndexObject = flattenedSubscriptIndices(index)
      let selfObject = state.ownedPyObject
      defer {
        Py_DecRef(pyIndexObject)
        Py_DecRef(selfObject)
      }

      // PyObject_GetItem returns +1 reference.
      if let result = PyObject_GetItem(selfObject, pyIndexObject) {
        return PyValue(owned: result)
      }

      PyErr_Clear()
      return nil
    }
    nonmutating set {
      let pyIndexObject = flattenedSubscriptIndices(index)
      let selfObject = state.ownedPyObject
      defer {
        Py_DecRef(pyIndexObject)
        Py_DecRef(selfObject)
      }

      if let newValue = newValue {
        let newValueObject = newValue.ownedPyObject
        PyObject_SetItem(selfObject, pyIndexObject, newValueObject)
        Py_DecRef(newValueObject)
      } else {
        // Assigning nil deletes the key, just like Swift dictionaries.
        PyObject_DelItem(selfObject, pyIndexObject)
      }
    }
  }

  public subscript(i: PythonConvertible...) -> PyValue? {
    get {
      return self[array: i]
    }
    nonmutating set {
      self[array: i] = newValue
    }
  }

  public var tuple2: (PyValue, PyValue)? {
    guard let elt0 = self[0], let elt1 = self[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  public var tuple3: (PyValue, PyValue, PyValue)? {
    guard let elt0 = self[0], let elt1 = self[1], let elt2 = self[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  public var tuple4: (PyValue, PyValue, PyValue, PyValue)? {
    guard let elt0 = self[0], let elt1 = self[1],
          let elt2 = self[2], let elt3 = self[3] else {
      return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}

//===----------------------------------------------------------------------===//
// MARK: Core PyValue API
//===----------------------------------------------------------------------===//

/// Turn an array of indices into a flattened index reference as a +1 Python
/// object.
private func flattenedSubscriptIndices(
  _ index: [PythonConvertible]
) -> OwnedPyObject {
  if index.count == 1 {
    return index[0].ownedPyObject
  }
  return pyTuple(index.map { $0.pythonValue })
}

public extension PyValue {
  subscript(dynamicMember member: String) -> PyValue {
    get {
      return checking[dynamicMember: member]!
    }
    set {
      let selfObject = self.ownedPyObject
      defer { Py_DecRef(selfObject) }
      let valueObject = newValue.ownedPyObject
      defer { Py_DecRef(valueObject) }

      if PyObject_SetAttrString(selfObject, member, valueObject) == -1 {
        try! throwPythonErrorIfPresent()
        fatalError("setting an invalid Python member \(member)")
      }
    }
  }

  // Dictionary lookups return optionals because they can always fail if the
  // key is not present.
  func member(_ dictMember: PyValue) -> PyValue? {

    let selfObject = self.ownedPyObject
    let keyObject = dictMember.ownedPyObject
    defer {
      Py_DecRef(selfObject)
      Py_DecRef(keyObject)
    }

    // PyDict_GetItem returns +0 result.
    return PyValue(borrowed: PyDict_GetItem(selfObject, keyObject))
  }

  subscript(index: PythonConvertible...) -> PyValue {
    get {
      return self.checking[array: index]!
    }
    nonmutating set {
      self.checking[array: index] = newValue
    }
  }

  // Helpers for destructuring tuples
  var tuple2: (PyValue, PyValue) {
    return (self[0], self[1])
  }

  var tuple3: (PyValue, PyValue, PyValue) {
    return (self[0], self[1], self[2])
  }

  var tuple4: (PyValue, PyValue, PyValue, PyValue) {
    return (self[0], self[1], self[2], self[3])
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  func call<T : PythonConvertible>(
    with args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! self.throwing.call(argArray: args, kwargs: kwargs)
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  func call(with args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) -> PyValue {
    return try! self.throwing.call(argArray: args.map { $0.pythonValue },
                                   kwargs: kwargs)
  }

  /// Call a member.
  @discardableResult
  func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! self.throwing.callMember(name, argArray: args, kwargs: kwargs)
  }
  @discardableResult
  func callMember(
    _ name: String,
    with args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! self.throwing.callMember(name,
                                         argArray: args.map { $0.pythonValue },
                                         kwargs: kwargs)
  }

  // NOTE: These are ad-hoc functions added for the demo, in absence of
  // @dynamicCallable. Remove before project release.
  @discardableResult
  func open(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! self.throwing.callMember("open",
                                         argArray: args.map { $0.pythonValue },
                                         kwargs: kwargs)
  }

  @discardableResult
  func load(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! self.throwing.callMember("load",
                                         argArray: args.map { $0.pythonValue },
                                         kwargs: kwargs)
  }
}

//===----------------------------------------------------------------------===//
// MARK: `Python` Type Implementation
//===----------------------------------------------------------------------===//

// We want the user to iteract with Python at the top level through a
// Python.import("foo") sort of statement.  The problem with this is that we
// want to ensure that Python is initialized on all uses of the Python interface
// and we want to allow this interface to eventually conform to a protocol that
// allows dynamic lookup of its members (through the Builtin map).  This will
// require an instance named "Python", which is unconventional, but does what we
// need.
/// The Python interface.
public let Python = PythonInterface()

@_fixed_layout
@dynamicMemberLookup
public struct PythonInterface {
  /// A hash table of the builtins provided by the Python language.
  public let builtins: PyValue

  init() {
    Py_Initialize()   // Initialize Python
    builtins = PyValue(borrowed: PyEval_GetBuiltins())
  }

  public func attemptImport(_ name: String) throws -> PyValue {
    guard let module = PyImport_ImportModule(name) else {
      try throwPythonErrorIfPresent()
      throw PythonError.invalidModule(name)
    }
    return PyValue(owned: module)
  }

  public func `import`(_ name: String) -> PyValue {
    return try! attemptImport(name)
  }

  public func updatePath(to path: String) {
    var cStr = path.utf8CString
    cStr.withUnsafeMutableBufferPointer { buf in
      PySys_SetPath(buf.baseAddress)
    }
  }

  public subscript(dynamicMember name: String) -> PyValue {
    return builtins[name]
  }

  // NOTE: This is an ad-hoc function added for the demo, in absence of
  // @dynamicCallable. Remove before project release.
  @discardableResult
  public func open(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) -> PyValue {
    return try! builtins["open"].throwing.call(
      argArray: args.map { $0.pythonValue },
      kwargs: kwargs
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: Python List, Dictionary, Slice and Tuple Helpers
//===----------------------------------------------------------------------===//

private func pySlice(_ start: PythonConvertible,
                     _ end: PythonConvertible,
                     _ step: PythonConvertible? = nil) -> OwnedPyObject {
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
private func pyTuple<T : Collection>(_ vals: T) -> OwnedPyObject
  where T.Element : PythonConvertible {

  let tuple = PyTuple_New(vals.count)!
  for (index, element) in vals.enumerated() {
    // PyTuple_SetItem steals the reference of the value stored.
    PyTuple_SetItem(tuple, index, element.ownedPyObject)
  }
  return tuple
}

public extension PyValue {
  /// - FIXME: This should be subsumed by Swift ranges + strides.  Python has a
  ///   very extravagent model though, it isn't clear how best to represent this
  ///   in Swift.
  ///
  /// Initial thoughts are that we should sugar the obvious cases (so you can
  /// use 0...100 in a subscript) but then provide this member for the fully
  /// general case.
  ///
  /// We also need conditional conformances to allow range if PyValue's to be a
  /// Slice.  We can probably get away with a bunch of overloads for now given
  /// that slices are typically used with concrete operands.
  init(slice start: PythonConvertible,
       _ end: PythonConvertible,
       _ step: PythonConvertible? = nil) {
    self.init(owned: pySlice(start, end, step))
  }

  // Tuples will require explicit support until Tuples can conform to protocols,
  // which is probably a long time.
  init(tuple elts: PythonConvertible...) {
    self.init(tupleContentsOf: elts)
  }

  init<T : Collection>(tupleContentsOf elts: T)
    where T.Element == PythonConvertible {
    self.init(owned: pyTuple(elts.map { $0.pythonValue }))
  }

  init<T : Collection>(tupleContentsOf elts: T)
    where T.Element : PythonConvertible {
    self.init(owned: pyTuple(elts))
  }
}


//===----------------------------------------------------------------------===//
// MARK: Builtin Swift Types Conformances to PyValue
//===----------------------------------------------------------------------===//

/// Return true if the specified value is an instance of the low-level Python
/// type descriptor passed in as 'type'.
private func isType(_ val: PyValue, type: UnsafeMutableRawPointer) -> Bool {
  let typePyRef = PyValue(borrowed: type.assumingMemoryBound(to: PyObject.self))
  let result = Python.isinstance.call(with: val, typePyRef)

  // We can't use the normal failable Bool initialization from PyValue here,
  // because that would cause an infinite loop, calling back into isType.
  let pyObject = result.ownedPyObject
  defer { Py_DecRef(pyObject) }

  // Anything not zero is truthy.
  return !(pyObject == &_Py_ZeroStruct)
}

private func == (_ x: UnsafeMutablePointer<PyObject>,
                 _ y: UnsafeMutableRawPointer) -> Bool {
  return x == y.assumingMemoryBound(to: PyObject.self)
}

extension Bool : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    guard isType(pyValue, type: &PyBool_Type) else { return nil }

    let pyObject = pyValue.ownedPyObject
    defer { Py_DecRef(pyObject) }

    self = pyObject == &_Py_TrueStruct
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    return PyValue(owned: PyBool_FromLong(self ? 1 : 0))
  }
}

extension String : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    let pyObject = pyValue.ownedPyObject
    defer { Py_DecRef(pyObject) }

    guard let cStringVal = PyString_AsString(pyObject) else {
      PyErr_Clear()
      return nil
    }

    self = String(cString: cStringVal)
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    let v = self.utf8CString.withUnsafeBufferPointer {
      PyString_FromStringAndSize($0.baseAddress, $0.count-1 /*trim \0*/)!
    }
    return PyValue(owned: v)
  }
}

fileprivate extension PyValue {
  // This converts a PyValue to some given type by applying the appropriate
  // converter and checking against the error value.
  func converted<T : Equatable>(withError errorValue: T,
                                by converter: (OwnedPyObject) -> T) -> T? {
    let pyObject = ownedPyObject
    defer { Py_DecRef(pyObject) }

    assert(PyErr_Occurred() == nil,
           "Python error occurred somewhere but wasn't handled")

    let value = converter(pyObject)
    guard value != errorValue || PyErr_Occurred() == nil else {
      PyErr_Clear()
      return nil
    }
    return value

  }
}

extension Int : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    // PyInt_AsLong return -1 and sets an error if the Python value isn't
    // integer compatible.
    guard let value = pyValue.converted(withError: -1, by: PyInt_AsLong) else {
      return nil
    }
    self = value
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    return PyValue(owned: PyInt_FromLong(self))
  }
}

extension UInt : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    // PyInt_AsUnsignedLongMask isn't documented as such, but in fact it does
    // return -1 and sets an error if the Python value isn't
    // integer compatible.
    guard let value = pyValue.converted(withError: ~0,
                                        by: PyInt_AsUnsignedLongMask) else {
      return nil
    }
    self = value
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    return PyValue(owned: PyInt_FromSize_t(Int(self)))
  }
}

extension Double : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    // PyFloat_AsDouble return -1 and sets an error
    // if the Python value isn't float compatible.
    guard let value = pyValue.converted(withError: -1,
                                        by: PyFloat_AsDouble) else {
      return nil
    }
    self = value
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    return PyValue(owned: PyFloat_FromDouble(self))
  }
}

//===----------------------------------------------------------------------===//
// Sized integer and Float conformances.
//===----------------------------------------------------------------------===//

// Any integer can conform to PyValue by using the Int/UInt implementation.
public protocol FixedWidthIntegerPyValue : PythonConvertible, FixedWidthInteger {
  associatedtype ParentPythonIntType : PythonConvertible, FixedWidthInteger
}

public extension FixedWidthIntegerPyValue {
  init?(_ pyValue: PyValue) {
    guard let i = ParentPythonIntType(pyValue) else { return nil }
    self = Self(i)
  }

  var pythonValue: PyValue {
    return ParentPythonIntType(self).pythonValue
  }
}

extension Int8  : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = Int }
extension Int16 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = Int }
extension Int32 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = Int }
extension Int64 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = Int }
extension UInt8  : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = UInt }
extension UInt16 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = UInt }
extension UInt32 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = UInt }
extension UInt64 : FixedWidthIntegerPyValue { public typealias ParentPythonIntType = UInt }

extension Float : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    guard let v = Double(pyValue) else { return nil }
    self = Float(v)
  }
  public var pythonValue: PyValue {
    return Double(self).pythonValue
  }
}

//===----------------------------------------------------------------------===//
// Collection Conformances to PythonConvertible
//===----------------------------------------------------------------------===//

// Arrays are PythonConvertible if their elements are.
extension Array : PythonConvertible where Element : PythonConvertible {
  public init?(_ pyValue: PyValue) {
    self = []

    for elt in pyValue {
      guard let eltVal = Element(elt) else { return nil }
      append(eltVal)
    }
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.
    let list = PyList_New(count)!
    for (index, element) in enumerated() {
      // PyList_SetItem steals the reference of the value stored.
      PyList_SetItem(list, index, element.ownedPyObject)
    }
    return PyValue(owned: list)
  }
}

// Dictionary is PythonConvertible if its keys and values are.
extension Dictionary : PythonConvertible
  where Key : PythonConvertible, Value : PythonConvertible {
  public init?(_ pythonDict: PyValue) {
    self = [:]

    // Iterate the Python dictionary, converting the key/value's within it to
    // the specified Swift Key/Value pairs.
    var key, value: UnsafeMutablePointer<PyObject>?
    var position: Py_ssize_t = 0

    while PyDict_Next(pythonDict.borrowedPyObject,
                      &position, &key, &value) != 0 {
      // If either the key or value are not convertible to the expected Swift
      // type then the entire dictionary fails to convert.
      if let swiftKey = Key(PyValue(borrowed: key!)),
         let swiftValue = Value(PyValue(borrowed: value!)) {
        // It is possible that there are duplicate keys after conversion.  We
        // silently allow duplicate keys and pick a nondeterministic result
        // if there is a collision.
        self[swiftKey] = swiftValue
      } else {
        return nil
      }
    }
  }

  public var pythonValue: PyValue {
    _ = Python // ensure Python is initialized.

    let dict = PyDict_New()!
    for (key, val) in self {
      let k = key.ownedPyObject
      let v = val.ownedPyObject
      PyDict_SetItem(dict, k, v)
      Py_DecRef(k)
      Py_DecRef(v)
    }

    return PyValue(owned: dict)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators and Conformances
//===----------------------------------------------------------------------===//

public extension PyValue {
  static func + (lhs: PyValue, rhs: PyValue) -> PyValue {
    return lhs.__add__.call(with: rhs)
  }

  static func - (lhs: PyValue, rhs: PyValue) -> PyValue {
    return lhs.__sub__.call(with: rhs)
  }

  static func * (lhs: PyValue, rhs: PyValue) -> PyValue {
    return lhs.__mul__.call(with: rhs)
  }

  static func / (lhs: PyValue, rhs: PyValue) -> PyValue {
    return lhs.__truediv__.call(with: rhs)
  }

  static func += (lhs: inout PyValue, rhs: PyValue) {
    lhs = lhs + rhs
  }

  static func -= (lhs: inout PyValue, rhs: PyValue) {
    lhs = lhs - rhs
  }

  static func *= (lhs: inout PyValue, rhs: PyValue) {
    lhs = lhs * rhs
  }

  static func /= (lhs: inout PyValue, rhs: PyValue) {
    lhs = lhs / rhs
  }
}

extension PyValue : SignedNumeric {
  public init<T : BinaryInteger>(exactly value: T) {
    self = PyValue(Int(value))
  }

  public typealias Magnitude = PyValue

  public var magnitude: PyValue {
    return self < 0 ? -self : self
  }
}


// Define conformance to Comparable and Equatable
extension PyValue : Hashable, Comparable, Equatable {
  // Comparable/Equatable are implemented using rich comparison. This tries at
  // first the dedicated function, e.g. __eq__ for equality and if this does not
  // work 3 way comparison is used.
  // This is coherent with how comparison is handled in the Python interpreter.
  private func compared(to other: PyValue, byOp: Int32) -> Bool {
    let lhsObject = self.ownedPyObject
    let rhsObject = other.ownedPyObject
    defer {
      Py_DecRef(lhsObject)
      Py_DecRef(rhsObject)
    }
    assert(PyErr_Occurred() == nil,
           "Python error occurred somewhere but wasn't handled")
    switch PyObject_RichCompareBool(lhsObject, rhsObject, byOp) {
    case 0: return false
    case 1: return true
    default:
      try! throwPythonErrorIfPresent()
      fatalError(
        "No result or error returned when comparing \(self) to \(other)")
    }
  }

  public static func == (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_EQ)
  }

  public static func != (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_NE)
  }

  public static func < (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_LT)
  }

  public static func <= (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_LE)
  }

  public static func > (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_GT)
  }

  public static func >= (lhs: PyValue, rhs: PyValue) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_GE)
  }

  public var hashValue: Int {
    guard let hash = Int(self.__hash__.call()) else {
      fatalError("cannot use __hash__ on \(self)")
    }
    return hash
  }
}

extension PyValue : MutableCollection {
  public typealias Index = PyValue
  public typealias Element = PyValue

  public var startIndex: Index {
    return 0
  }

  public var endIndex: Index {
    return Python.len.call(with: self)
  }

  public subscript(index: PyValue) -> PyValue {
    get {
      return self[index as PythonConvertible]
    }
    set {
      self[index as PythonConvertible] = newValue
    }
  }

  // Method that returns the next index when iterating
  public func index(after i: Index) -> Index {
    return i + PyValue(1)
  }
}

// Simple Literal Conformances
extension PyValue : ExpressibleByBooleanLiteral,
                  ExpressibleByIntegerLiteral,
                  ExpressibleByFloatLiteral,
                  ExpressibleByStringLiteral {
  public init(booleanLiteral value: Bool) {
    self = PyValue(value)
  }
  public init(integerLiteral value: Int) {
    self = PyValue(value)
  }
  public init(floatLiteral value: Double) {
    self = PyValue(value)
  }
  public init(stringLiteral value: String) {
    self = PyValue(value)
  }
}

// Collection Literal Conformances
extension PyValue : ExpressibleByArrayLiteral,
                  ExpressibleByDictionaryLiteral {
  public init(arrayLiteral elements: PyValue...) {
    self = elements.pythonValue
  }
  public typealias Key = PyValue
  public typealias Value = PyValue
  public init(dictionaryLiteral elements: (PyValue, PyValue)...) {
    self = Dictionary(elements, uniquingKeysWith: { lhs, _ in lhs }).pythonValue
  }
}
