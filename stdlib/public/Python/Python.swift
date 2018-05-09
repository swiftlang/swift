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
// This file defines an interoperability layer for talking to Python from Swift.
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

/// Primitive reference to a Python object.  This is always non-null and always
/// owning of the underlying object.
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
// MARK: PythonObject type
//===----------------------------------------------------------------------===//

/// This is the currency type for Python object references.  It is passed to and
/// returned from Python calls and member references, and is overloaded to
/// support the standard operations that Python supports.
@dynamicMemberLookup
@_fixed_layout
public struct PythonObject {
  /// This is the actual handle to a Python object that we represent.
  fileprivate var state: PyReference

  @_versioned
  init(_ state: PyReference) {
    self.state = state
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

/// Make "print(python)" print a pretty form of the PythonObject.
extension PythonObject : CustomStringConvertible {
  public var description: String {
    // We describe a Python object to the REPL and Playgrounds with the str(x)
    // call, just like Python's REPL does.  'str' is designed to be readable,
    // and using 'repr' takes WAY too long for large objects because it is
    // designed to faithfully represent the object.
    return String(Python.str.call(with: self))!
  }
}

// Make PythonObject's show up nicely in the Xcode Playground results sidebar.
extension PythonObject : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

// Mirror representation, used by debugger/REPL.
extension PythonObject : CustomReflectable {
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
  init?(_ object: PythonObject)

  /// Python convertible values may always be converted to Python.
  var pythonObject: PythonObject { get }
}

// You can explicitly convert any PythonConvertible to a PythonObject.
extension PythonObject {
  public init<T : PythonConvertible>(_ value: T) {
    self.init(value.pythonObject)
  }
}

// For our use below, provide helpers to convert PythonConvertible values to
// owned and borrowed references.  These shouldn't be public though.
fileprivate extension PythonConvertible {
  var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return pythonObject.borrowedPyObject
  }

  var ownedPyObject: OwnedPyObject {
    return pythonObject.ownedPyObject
  }
}

// PyReference and PythonObject are trivially PythonConvertible
extension PyReference : PythonConvertible {
  public convenience init(_ object: PythonObject) {
    self.init(owned: object.ownedPyObject)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonObject: PythonObject {
    return PythonObject(self)
  }
}

extension PythonObject : PythonConvertible {
  public init(_ object: PythonObject) {
    self.init(object.state)
  }

  /// Python convertible values may always be converted to Python.
  public var pythonObject: PythonObject { return self }
}

//===----------------------------------------------------------------------===//
// MARK: ThrowingPythonObject Implementation
//===----------------------------------------------------------------------===//

public extension PythonObject {
  /// Return a version of this object that may be called.  It throws a Swift
  /// error if the underlying Python function throws a Python exception.
  var throwing: ThrowingPythonObject {
    return ThrowingPythonObject(self)
  }
}

/// This represents the result of a failable operation when working with
/// Python objects.
@_fixed_layout
public enum PythonError : Error {
  /// This represents an exception thrown out of a Python API.  This can occur
  /// on calls.
  case exception(PythonObject)

  /// A call on the specified object failed, e.g. because it wasn't a Python
  /// callable, because the wrong number of parameters were provided, or because
  /// a keyword argument was specified multiple times.
  case invalidCall(PythonObject)

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
  let r = PythonObject(owned: value ?? type!)
  throw PythonError.exception(r)
}

/// This type is a PythonObject produced when the user cares about getting an
/// exception out of a call.  We wrap this up and reflect it back as a thrown
/// Swift error.
@_fixed_layout
public struct ThrowingPythonObject {
  private var state: PythonObject

  fileprivate init(_ object: PythonObject) {
    state = object
  }

  /// Call self, which must be a Python Callable.  If the callee throws a Python
  /// exception, if the callee isn't callable, or if there is some other
  /// problem, we throw a Swift error.
  @discardableResult
  public func call<T : PythonConvertible>(
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
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
        let k = PythonObject(key).ownedPyObject
        let v = val.ownedPyObject
        PyDict_SetItem(kwdictObject, k, v)
        Py_DecRef(k)
        Py_DecRef(v)
      }
    }
    defer { Py_DecRef(kwdictObject) }  // Py_DecRef is nil safe.

    // Non-keyword arguments are passed as a tuple of objects.
    let argTuple = pyTuple(args)
    defer { Py_DecRef(argTuple) }

    // Python calls always return a non-null object when successful.  If the
    // Python function produces the equivalent of C "void", it returns the None
    // object.  A null result of PyObjectCall happens when there is an error,
    // like 'self' not being a Python callable.
    let selfObject = state.ownedPyObject
    defer { Py_DecRef(selfObject) }

    guard let result = PyObject_Call(selfObject, argTuple, kwdictObject) else {
      // Translate a Python exception into a Swift error if one was thrown.
      try throwPythonErrorIfPresent()
      throw PythonError.invalidCall(state)
    }

    return PythonObject(owned: result)
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  public func call(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
    return try call(argArray: args.map { $0.pythonObject }, kwargs: kwargs)
  }

  // Call a member, as in self.foo(...)
  @discardableResult
  public func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
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
  ) throws -> PythonObject {
    return try callMember(name, argArray: args.map { $0.pythonObject },
                          kwargs: kwargs)
  }

  public var tuple2: (PythonObject, PythonObject)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  public var tuple3: (PythonObject, PythonObject, PythonObject)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1], let elt2 = ct[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  public var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject)? {
    let ct = state.checking
    guard let elt0 = ct[0], let elt1 = ct[1],
          let elt2 = ct[2], let elt3 = ct[3] else {
        return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}


//===----------------------------------------------------------------------===//
// MARK: CheckingPythonObject Implementation
//===----------------------------------------------------------------------===//

public extension PythonObject {
  /// Return a version of this object that may have member access operations
  /// performed on it.  These operations can fail and return an optional when
  /// the underlying Python operations fail.
  var checking: CheckingPythonObject {
    return CheckingPythonObject(self)
  }
}

/// This type temporarily wraps a PythonObject when the user cares about turning
/// an operation (like a member lookup or subscript) into a failable operation
/// that returns an optional.
@dynamicMemberLookup
@_fixed_layout
public struct CheckingPythonObject {
  private var state: PythonObject

  fileprivate init(_ object: PythonObject) {
    state = object
  }

  public subscript(dynamicMember name: String) -> PythonObject? {
    get {
      let selfObject = state.ownedPyObject
      defer { Py_DecRef(selfObject) }
      guard let result = PyObject_GetAttrString(selfObject, name) else {
        PyErr_Clear()
        return nil
      }
      // PyObject_GetAttrString returns +1 result.
      return PythonObject(owned: result)
    }
  }

  /// Swift subscripts cannot throw yet, so model this as returning an optional
  /// reference.
  public subscript(array index: [PythonConvertible]) -> PythonObject? {
    get {
      let pyIndexObject = flattenedSubscriptIndices(index)
      let selfObject = state.ownedPyObject
      defer {
        Py_DecRef(pyIndexObject)
        Py_DecRef(selfObject)
      }

      // PyObject_GetItem returns +1 reference.
      if let result = PyObject_GetItem(selfObject, pyIndexObject) {
        return PythonObject(owned: result)
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

  public subscript(i: PythonConvertible...) -> PythonObject? {
    get {
      return self[array: i]
    }
    nonmutating set {
      self[array: i] = newValue
    }
  }

  public var tuple2: (PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  public var tuple3: (PythonObject, PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1], let elt2 = self[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  public var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1],
          let elt2 = self[2], let elt3 = self[3] else {
      return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}

//===----------------------------------------------------------------------===//
// MARK: Core PythonObject API
//===----------------------------------------------------------------------===//

/// Turn an array of indices into a flattened index reference as a +1 Python
/// object.
private func flattenedSubscriptIndices(
  _ index: [PythonConvertible]
) -> OwnedPyObject {
  if index.count == 1 {
    return index[0].ownedPyObject
  }
  return pyTuple(index.map { $0.pythonObject })
}

public extension PythonObject {
  subscript(dynamicMember member: String) -> PythonObject {
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
  func member(_ dictMember: PythonObject) -> PythonObject? {

    let selfObject = self.ownedPyObject
    let keyObject = dictMember.ownedPyObject
    defer {
      Py_DecRef(selfObject)
      Py_DecRef(keyObject)
    }

    // PyDict_GetItem returns +0 result.
    return PythonObject(borrowed: PyDict_GetItem(selfObject, keyObject))
  }

  subscript(index: PythonConvertible...) -> PythonObject {
    get {
      return self.checking[array: index]!
    }
    nonmutating set {
      self.checking[array: index] = newValue
    }
  }

  // Helpers for destructuring tuples
  var tuple2: (PythonObject, PythonObject) {
    return (self[0], self[1])
  }

  var tuple3: (PythonObject, PythonObject, PythonObject) {
    return (self[0], self[1], self[2])
  }

  var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject) {
    return (self[0], self[1], self[2], self[3])
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  func call<T : PythonConvertible>(
    with args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PythonObject {
    return try! self.throwing.call(argArray: args, kwargs: kwargs)
  }

  /// Call self, which must be a Python Callable.
  @discardableResult
  func call(with args: PythonConvertible...,
                   kwargs: [(String, PythonConvertible)] = []) -> PythonObject {
    return try! self.throwing.call(argArray: args.map { $0.pythonObject },
                                   kwargs: kwargs)
  }

  /// Call a member.
  @discardableResult
  func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PythonObject {
    return try! self.throwing.callMember(name, argArray: args, kwargs: kwargs)
  }
  @discardableResult
  func callMember(
    _ name: String,
    with args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) -> PythonObject {
    return try! self.throwing.callMember(name,
                                         argArray: args.map { $0.pythonObject },
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
@_fixed_layout
public let Python = PythonInterface()

@_fixed_layout
@dynamicMemberLookup
public struct PythonInterface {
  /// A hash table of the builtins provided by the Python language.
  public let builtins: PythonObject

  init() {
    Py_Initialize()   // Initialize Python
    builtins = PythonObject(borrowed: PyEval_GetBuiltins())
  }

  public func attemptImport(_ name: String) throws -> PythonObject {
    guard let module = PyImport_ImportModule(name) else {
      try throwPythonErrorIfPresent()
      throw PythonError.invalidModule(name)
    }
    return PythonObject(owned: module)
  }

  public func `import`(_ name: String) -> PythonObject {
    return try! attemptImport(name)
  }

  public func updatePath(to path: String) {
    var cStr = path.utf8CString
    cStr.withUnsafeMutableBufferPointer { buf in
      PySys_SetPath(buf.baseAddress)
    }
  }

  public subscript(dynamicMember name: String) -> PythonObject {
    return builtins[name]
  }
}

//===----------------------------------------------------------------------===//
// MARK: Python List, Dictionary, Slice and Tuple Helpers
//===----------------------------------------------------------------------===//

private func pySlice(_ start: PythonConvertible?,
                     _ stop: PythonConvertible?,
                     _ step: PythonConvertible? = nil) -> OwnedPyObject {
  let startP = start?.ownedPyObject
  let stopP = stop?.ownedPyObject
  let stepP = step?.ownedPyObject

  // PySlice_New takes each operand at +0, and returns +1.
  let result = PySlice_New(startP, stopP, stepP)!

  Py_DecRef(startP)
  Py_DecRef(stopP)
  Py_DecRef(stepP)  // Py_DecRef is nil safe.
  return result
}

// Create a Python tuple object with the specified elements.
private func pyTuple<T : Collection>(_ vals: T) -> OwnedPyObject
  where T.Element : PythonConvertible {

  let tuple = PyTuple_New(vals.count)!
  for (index, element) in vals.enumerated() {
    // PyTuple_SetItem steals the reference of the object stored.
    PyTuple_SetItem(tuple, index, element.ownedPyObject)
  }
  return tuple
}

public extension PythonObject {
  /// - FIXME: This should be subsumed by Swift ranges + strides.  Python has a
  ///   very extravagant model though, it isn't clear how best to represent this
  ///   in Swift.
  ///
  /// Initial thoughts are that we should sugar the obvious cases (so you can
  /// use 0...100 in a subscript) but then provide this member for the fully
  /// general case.
  ///
  /// We also need conditional conformances to allow range if PythonObject's to
  /// be a Slice.  We can probably get away with a bunch of overloads for now
  /// given that slices are typically used with concrete operands.
  init(sliceStart start: PythonConvertible?,
       stop: PythonConvertible?,
       step: PythonConvertible? = nil) {
    self.init(owned: pySlice(start, stop, step))
  }

  // Tuples will require explicit support until Tuples can conform to protocols,
  // which is probably a long time.
  init(tuple elts: PythonConvertible...) {
    self.init(tupleContentsOf: elts)
  }

  init<T : Collection>(tupleContentsOf elts: T)
    where T.Element == PythonConvertible {
    self.init(owned: pyTuple(elts.map { $0.pythonObject }))
  }

  init<T : Collection>(tupleContentsOf elts: T)
    where T.Element : PythonConvertible {
    self.init(owned: pyTuple(elts))
  }
}


//===----------------------------------------------------------------------===//
// MARK: Builtin Swift Types Conformances to PythonObject
//===----------------------------------------------------------------------===//

/// Return true if the specified object is an instance of the low-level Python
/// type descriptor passed in as 'type'.
private func isType(_ object: PythonObject,
                    type: UnsafeMutableRawPointer) -> Bool {
  let typePyRef = PythonObject(
    borrowed: type.assumingMemoryBound(to: PyObject.self)
  )
  let result = Python.isinstance.call(with: object, typePyRef)

  // We can't use the normal failable Bool initializer from PythonObject here,
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
  public init?(_ pythonObject: PythonObject) {
    guard isType(pythonObject, type: &PyBool_Type) else { return nil }

    let pyObject = pythonObject.ownedPyObject
    defer { Py_DecRef(pyObject) }

    self = pyObject == &_Py_TrueStruct
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(owned: PyBool_FromLong(self ? 1 : 0))
  }
}

extension String : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    let pyObject = pythonObject.ownedPyObject
    defer { Py_DecRef(pyObject) }

    guard let cStringVal = PyString_AsString(pyObject) else {
      PyErr_Clear()
      return nil
    }

    self = String(cString: cStringVal)
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    let v = self.utf8CString.withUnsafeBufferPointer {
      PyString_FromStringAndSize($0.baseAddress, $0.count-1 /*trim \0*/)!
    }
    return PythonObject(owned: v)
  }
}

fileprivate extension PythonObject {
  // This converts a PythonObject to some given type by applying the appropriate
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
  public init?(_ pythonObject: PythonObject) {
    // PyInt_AsLong return -1 and sets an error if the Python object isn't
    // integer compatible.
    guard let value = pythonObject.converted(withError: -1,
                                             by: PyInt_AsLong) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(owned: PyInt_FromLong(self))
  }
}

extension UInt : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    // PyInt_AsUnsignedLongMask isn't documented as such, but in fact it does
    // return -1 and sets an error if the Python object isn't
    // integer compatible.
    guard let value = pythonObject.converted(withError: ~0,
                                        by: PyInt_AsUnsignedLongMask) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(owned: PyInt_FromSize_t(Int(self)))
  }
}

extension Double : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    // PyFloat_AsDouble return -1 and sets an error
    // if the Python object isn't float compatible.
    guard let value = pythonObject.converted(withError: -1,
                                        by: PyFloat_AsDouble) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(owned: PyFloat_FromDouble(self))
  }
}

//===----------------------------------------------------------------------===//
// Sized integer and Float conformances.
//===----------------------------------------------------------------------===//

// Any integer can conform to PythonObject by using the Int/UInt implementation.
public protocol FixedWidthIntegerPythonObject
  : PythonConvertible, FixedWidthInteger {
  associatedtype ParentPythonIntType : PythonConvertible, FixedWidthInteger
}

public extension FixedWidthIntegerPythonObject {
  init?(_ pythonObject: PythonObject) {
    guard let i = ParentPythonIntType(pythonObject) else { return nil }
    self = Self(i)
  }

  var pythonObject: PythonObject {
    return ParentPythonIntType(self).pythonObject
  }
}

extension Int8  : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = Int }
extension Int16 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = Int }
extension Int32 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = Int }
extension Int64 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = Int }
extension UInt8  : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = UInt }
extension UInt16 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = UInt }
extension UInt32 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = UInt }
extension UInt64 : FixedWidthIntegerPythonObject { public typealias ParentPythonIntType = UInt }

extension Float : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    guard let v = Double(pythonObject) else { return nil }
    self = Float(v)
  }
  public var pythonObject: PythonObject {
    return Double(self).pythonObject
  }
}

//===----------------------------------------------------------------------===//
// Collection Conformances to PythonConvertible
//===----------------------------------------------------------------------===//

// Arrays are PythonConvertible if their elements are.
extension Array : PythonConvertible where Element : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    self = []

    for elt in pythonObject {
      guard let eltVal = Element(elt) else { return nil }
      append(eltVal)
    }
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    let list = PyList_New(count)!
    for (index, element) in enumerated() {
      // PyList_SetItem steals the reference of the object stored.
      PyList_SetItem(list, index, element.ownedPyObject)
    }
    return PythonObject(owned: list)
  }
}

// Dictionary is PythonConvertible if its keys and values are.
extension Dictionary : PythonConvertible
  where Key : PythonConvertible, Value : PythonConvertible {
  public init?(_ pythonDict: PythonObject) {
    self = [:]

    // Iterate the Python dictionary, converting the key/value's within it to
    // the specified Swift Key/Value pairs.
    var key, value: UnsafeMutablePointer<PyObject>?
    var position: Py_ssize_t = 0

    while PyDict_Next(pythonDict.borrowedPyObject,
                      &position, &key, &value) != 0 {
      // If either the key or value are not convertible to the expected Swift
      // type then the entire dictionary fails to convert.
      if let swiftKey = Key(PythonObject(borrowed: key!)),
         let swiftValue = Value(PythonObject(borrowed: value!)) {
        // It is possible that there are duplicate keys after conversion.  We
        // silently allow duplicate keys and pick a nondeterministic result
        // if there is a collision.
        self[swiftKey] = swiftValue
      } else {
        return nil
      }
    }
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.

    let dict = PyDict_New()!
    for (key, val) in self {
      let k = key.ownedPyObject
      let v = val.ownedPyObject
      PyDict_SetItem(dict, k, v)
      Py_DecRef(k)
      Py_DecRef(v)
    }

    return PythonObject(owned: dict)
  }
}

//===----------------------------------------------------------------------===//
// Range Conformances to PythonConvertible
//===----------------------------------------------------------------------===//

extension Range : PythonConvertible where Bound : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    guard isType(pythonObject, type: &PySlice_Type) else { return nil }
    guard let lowerBound = Bound(pythonObject.start),
          let upperBound = Bound(pythonObject.stop) else {
       return nil
    }
    guard pythonObject.step == Python.None else { return nil }
    self.init(uncheckedBounds: (lowerBound, upperBound))
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(sliceStart: self.lowerBound,
                   stop: self.upperBound,
                   step: nil)
  }
}

extension PartialRangeFrom : PythonConvertible where Bound : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    guard isType(pythonObject, type: &PySlice_Type) else { return nil }
    guard let lowerBound = Bound(pythonObject.start) else { return nil }
    guard pythonObject.stop == Python.None,
          pythonObject.step == Python.None else {
       return nil
    }
    self.init(lowerBound)
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(sliceStart: self.lowerBound, stop: nil, step: nil)
  }
}

extension PartialRangeUpTo : PythonConvertible where Bound : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    guard isType(pythonObject, type: &PySlice_Type) else { return nil }
    guard let upperBound = Bound(pythonObject.stop) else { return nil }
    guard pythonObject.start == Python.None,
          pythonObject.step == Python.None else {
       return nil
    }
    self.init(upperBound)
  }

  public var pythonObject: PythonObject {
    _ = Python // ensure Python is initialized.
    return PythonObject(sliceStart: nil, stop: self.upperBound, step: nil)
  }
}

//===----------------------------------------------------------------------===//
// Standard Operators and Conformances
//===----------------------------------------------------------------------===//

public extension PythonObject {
  static func + (lhs: PythonObject, rhs: PythonObject) -> PythonObject {
    return lhs.__add__.call(with: rhs)
  }

  static func - (lhs: PythonObject, rhs: PythonObject) -> PythonObject {
    return lhs.__sub__.call(with: rhs)
  }

  static func * (lhs: PythonObject, rhs: PythonObject) -> PythonObject {
    return lhs.__mul__.call(with: rhs)
  }

  static func / (lhs: PythonObject, rhs: PythonObject) -> PythonObject {
    return lhs.__truediv__.call(with: rhs)
  }

  static func += (lhs: inout PythonObject, rhs: PythonObject) {
    lhs = lhs + rhs
  }

  static func -= (lhs: inout PythonObject, rhs: PythonObject) {
    lhs = lhs - rhs
  }

  static func *= (lhs: inout PythonObject, rhs: PythonObject) {
    lhs = lhs * rhs
  }

  static func /= (lhs: inout PythonObject, rhs: PythonObject) {
    lhs = lhs / rhs
  }
}

extension PythonObject : SignedNumeric {
  public init<T : BinaryInteger>(exactly value: T) {
    self = PythonObject(Int(value))
  }

  public typealias Magnitude = PythonObject

  public var magnitude: PythonObject {
    return self < 0 ? -self : self
  }
}


// Define conformance to Comparable and Equatable
extension PythonObject : Hashable, Comparable, Equatable {
  // Comparable/Equatable are implemented using rich comparison. This tries at
  // first the dedicated function, e.g. __eq__ for equality and if this does not
  // work 3 way comparison is used.
  // This is coherent with how comparison is handled in the Python interpreter.
  private func compared(to other: PythonObject, byOp: Int32) -> Bool {
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

  public static func == (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_EQ)
  }

  public static func != (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_NE)
  }

  public static func < (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_LT)
  }

  public static func <= (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_LE)
  }

  public static func > (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_GT)
  }

  public static func >= (lhs: PythonObject, rhs: PythonObject) -> Bool {
    return lhs.compared(to: rhs, byOp: Py_GE)
  }

  public var hashValue: Int {
    guard let hash = Int(self.__hash__.call()) else {
      fatalError("cannot use __hash__ on \(self)")
    }
    return hash
  }
}

extension PythonObject : MutableCollection {
  public typealias Index = PythonObject
  public typealias Element = PythonObject

  public var startIndex: Index {
    return 0
  }

  public var endIndex: Index {
    return Python.len.call(with: self)
  }

  public subscript(index: PythonObject) -> PythonObject {
    get {
      return self[index as PythonConvertible]
    }
    set {
      self[index as PythonConvertible] = newValue
    }
  }

  // Method that returns the next index when iterating
  public func index(after i: Index) -> Index {
    return i + PythonObject(1)
  }
}

// Simple Literal Conformances
extension PythonObject : ExpressibleByBooleanLiteral,
                         ExpressibleByIntegerLiteral,
                         ExpressibleByFloatLiteral,
                         ExpressibleByStringLiteral {
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
  public init(integerLiteral value: Int) {
    self.init(value)
  }
  public init(floatLiteral value: Double) {
    self.init(value)
  }
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

// Collection Literal Conformances
extension PythonObject : ExpressibleByArrayLiteral,
                         ExpressibleByDictionaryLiteral {
  public init(arrayLiteral elements: PythonObject...) {
    self.init(elements)
  }
  public typealias Key = PythonObject
  public typealias Value = PythonObject
  public init(dictionaryLiteral elements: (PythonObject, PythonObject)...) {
    self.init(Dictionary(elements, uniquingKeysWith: { lhs, _ in lhs }))
  }
}
