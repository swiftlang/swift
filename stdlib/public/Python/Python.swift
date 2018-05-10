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
// The model provided by this file is completely dynamic and does not require
// invasive compiler support.
//
//===----------------------------------------------------------------------===//

import CPython

//===----------------------------------------------------------------------===//
// `PyReference` definition
//===----------------------------------------------------------------------===//

/// Typealias used when passing or returning a `PyObject` pointer with
/// implied ownership.
public typealias OwnedPyObjectPointer = UnsafeMutablePointer<PyObject>

/// A primitive reference to a Python C API `PyObject`.
///
/// A `PyReference` instance has ownership of its underlying `PyObject`, which
/// must be non-null.
///
/// - Note: When Swift has ownership, `PyReference` should be removed.
///   `PythonObject` will be a Swift struct with copy constructors, move
///   operators, etc.
@_versioned @_fixed_layout
final class PyReference {
  private var pointer: OwnedPyObjectPointer

  init(owning pointer: OwnedPyObjectPointer) {
    self.pointer = pointer
  }

  init(borrowing pointer: UnsafeMutablePointer<PyObject>) {
    self.pointer = pointer
    Py_IncRef(pointer)
  }

  deinit {
    Py_DecRef(pointer)
  }

  var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return pointer
  }

  var ownedPyObject: OwnedPyObjectPointer {
    Py_IncRef(pointer)
    return pointer
  }
}

//===----------------------------------------------------------------------===//
// `PythonObject` definition
//===----------------------------------------------------------------------===//

/// A Python object.
///
/// `PythonObject` is a wrapper around a `PyReference`.
///
/// `PythonObject` is passed to and returned from all Python function calls and
/// member references, and is overloaded to support the standard Python
/// operations.
///
/// - Note: `PyReference` is an implementation detail for `PythonObject`.
///   When Swift has ownership, `PyReference` will be removed and `PythonObject`
///   will directly wrap a `PyObject` pointer.
@dynamicMemberLookup
@_fixed_layout
public struct PythonObject {
  /// The underlying `PyReference`.
  fileprivate var reference: PyReference

  @_versioned
  init(_ pointer: PyReference) {
    reference = pointer
  }

  /// Creates a new instance, taking ownership of the specified `PyObject` pointer.
  public init(owning pointer: OwnedPyObjectPointer) {
    reference = PyReference(owning: pointer)
  }

  /// Creates a new instance from the specified `PyObject` pointer.
  public init(borrowing pointer: UnsafeMutablePointer<PyObject>) {
    reference = PyReference(borrowing: pointer)
  }

  fileprivate var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return reference.borrowedPyObject
  }

  fileprivate var ownedPyObject: OwnedPyObjectPointer {
    return reference.ownedPyObject
  }
}

/// Make `print(python)` print a pretty form of the `PythonObject`.
extension PythonObject : CustomStringConvertible {
  /// A textual description of this Python object, produced by `Python.str`.
  public var description: String {
    // The `str` function is used here because it is designed to return
    // human-readable descriptions of Python objects. The Python REPL also uses
    // it for printing descriptions.
    // `repr` is not used because it is not designed to be readable and takes
    // too long for large objects.
    return String(Python.str.call(with: self))!
  }
}

// Make `PythonObject` show up nicely in the Xcode Playground results sidebar.
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
// `PythonConvertible` protocol
//===----------------------------------------------------------------------===//

public protocol PythonConvertible {
  /// Creates a new instance from the given `PythonObject`, if possible.
  /// - Note: Conversion may fail if the given `PythonObject` instance is
  ///   incompatible (e.g. a Python `string` object cannot be converted into an
  ///   `Int`).
  init?(_ object: PythonObject)

  /// A `PythonObject` instance representing this value.
  var pythonObject: PythonObject { get }
}

public extension PythonObject {
  /// Creates a new instance from a `PythonConvertible` value.
  init<T : PythonConvertible>(_ object: T) {
    self.init(object.pythonObject)
  }
}

/// Internal helpers to convert `PythonConvertible` values to owned and borrowed
/// `PyObject` instances. These should not be made public.
fileprivate extension PythonConvertible {
  var borrowedPyObject: UnsafeMutablePointer<PyObject> {
    return pythonObject.borrowedPyObject
  }

  var ownedPyObject: OwnedPyObjectPointer {
    return pythonObject.ownedPyObject
  }
}

/// `PythonObject` is trivially `PythonConvertible`.
extension PythonObject : PythonConvertible {
  public init(_ object: PythonObject) {
    self.init(owning: object.ownedPyObject)
  }

  public var pythonObject: PythonObject { return self }
}

//===----------------------------------------------------------------------===//
// `PythonObject` callable implementation
//===----------------------------------------------------------------------===//

public extension PythonObject {
  /// Returns a callable version of this `PythonObject`. When called, the result
  /// throws a Swift error if the underlying Python function throws a Python
  /// exception.
  var throwing: ThrowingPythonObject {
    return ThrowingPythonObject(self)
  }
}

/// An error produced by a failable Python operation.
@_fixed_layout
public enum PythonError : Error, Equatable {
  /// A Python runtime exception, produced by calling a Python function.
  case exception(PythonObject)

  /// A failed call on a `PythonObject`.
  /// Reasons for failure include:
  /// - A non-callable Python object was called.
  /// - An incorrect number of arguments were provided to the callable Python
  ///   object.
  /// - An invalid keyword argument was specified.
  case invalidCall(PythonObject)

  /// A member lookup error.
  case invalidMember(String)

  /// A module import error.
  case invalidModule(String)
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

  // Fetch the exception and clear the exception state.
  PyErr_Fetch(&type, &value, &traceback)

  // The value for the exception may not be set but the type always should be.
  let r = PythonObject(owning: value ?? type!)
  throw PythonError.exception(r)
}

/// A `PythonObject` wrapper that enables throwing method calls.
/// Exceptions produced by Python functions are reflected as Swift errors and
/// thrown.
@_fixed_layout
public struct ThrowingPythonObject {
  private var base: PythonObject

  fileprivate init(_ base: PythonObject) {
    self.base = base
  }

  /// Call `self` with the specified arguments.
  /// If the call fails for some reason, `PythonError.invalidCall` is thrown.
  /// - Precondition: `self` must be a Python callable.
  /// - Parameters:
  ///   - argArray: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  public func call<T : PythonConvertible>(
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
    // Make sure there are no state errors.
    if PyErr_Occurred() != nil {
      // FIXME: This should be an assert, but the failure mode in Playgrounds
      // is just awful.
      fatalError("Python error state must be clear")
    }

    // Produce a dictionary for keyword arguments if any are present.
    var kwdictObject: OwnedPyObjectPointer? = nil
    if !kwargs.isEmpty {
      kwdictObject = PyDict_New()!
      for (key, value) in kwargs {
        // FIXME: What if two identical keywords are provided?
        let k = PythonObject(key).ownedPyObject
        let v = value.ownedPyObject
        PyDict_SetItem(kwdictObject, k, v)
        Py_DecRef(k)
        Py_DecRef(v)
      }
    }
    defer { Py_DecRef(kwdictObject) } // Py_DecRef is `nil` safe.

    // Non-keyword arguments are passed as a tuple of objects.
    let argTuple = pyTuple(args)
    defer { Py_DecRef(argTuple) }

    // Python calls always return a non-null object when successful. If the
    // Python function produces the equivalent of C `void`, it returns the
    // `None` object. A `null` result of `PyObjectCall` happens when there is an
    // error, like `self` not being a Python callable.
    let selfObject = base.ownedPyObject
    defer { Py_DecRef(selfObject) }

    guard let result = PyObject_Call(selfObject, argTuple, kwdictObject) else {
      // If a Python exception was thrown, throw a corresponding Swift error.
      try throwPythonErrorIfPresent()
      throw PythonError.invalidCall(base)
    }
    return PythonObject(owning: result)
  }

  // Call `self` with the specified arguments.
  /// - Precondition: `self` must be a Python callable.
  /// - Parameters:
  ///   - args: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  public func call(
    _ args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
    return try call(argArray: args.map { $0.pythonObject }, kwargs: kwargs)
  }

  // Call a member with the specified arguments.
  /// - Precondition: The member must be a Python callable.
  /// - Parameters:
  ///   - name: The name of the member.
  ///   - argArray: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  public func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
    // If the member lookup fails, reflect it as a Swift error.
    guard let callee = base.checking[dynamicMember: name] else {
      throw PythonError.invalidMember(name)
    }
    return try callee.throwing.call(argArray: args, kwargs: kwargs)
  }

  // Call a member with the specified arguments.
  /// - Precondition: `self` must be a Python callable.
  /// - Parameters:
  ///   - name: The name of the member.
  ///   - args: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  public func callMember(
    _ name: String,
    with args: PythonConvertible...,
    kwargs: [(String, PythonConvertible)] = []
  ) throws -> PythonObject {
    return try callMember(name, argArray: args.map { $0.pythonObject },
                          kwargs: kwargs)
  }

  /// Converts to a 2-tuple, if possible.
  public var tuple2: (PythonObject, PythonObject)? {
    let ct = base.checking
    guard let elt0 = ct[0], let elt1 = ct[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  /// Converts to a 2-tuple, if possible.
  public var tuple3: (PythonObject, PythonObject, PythonObject)? {
    let ct = base.checking
    guard let elt0 = ct[0], let elt1 = ct[1], let elt2 = ct[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  /// Converts to a 2-tuple, if possible.
  public var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject)? {
    let ct = base.checking
    guard let elt0 = ct[0], let elt1 = ct[1],
          let elt2 = ct[2], let elt3 = ct[3] else {
        return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}


//===----------------------------------------------------------------------===//
// `PythonObject` member access implementation
//===----------------------------------------------------------------------===//

public extension PythonObject {
  /// Returns a `PythonObject` wrapper capable of member accesses.
  var checking: CheckingPythonObject {
    return CheckingPythonObject(self)
  }
}

/// A `PythonObject` wrapper that enables member accesses.
/// Member access operations return an `Optional` result. When member access
/// fails, `nil` is returned.
@dynamicMemberLookup
@_fixed_layout
public struct CheckingPythonObject {
  /// The underlying `PythonObject`.
  private var base: PythonObject

  fileprivate init(_ base: PythonObject) {
    self.base = base
  }

  public subscript(dynamicMember name: String) -> PythonObject? {
    get {
      let selfObject = base.ownedPyObject
      defer { Py_DecRef(selfObject) }
      guard let result = PyObject_GetAttrString(selfObject, name) else {
        PyErr_Clear()
        return nil
      }
      // `PyObject_GetAttrString` returns +1 result.
      return PythonObject(owning: result)
    }
  }

  /// Access the element corresponding to the specified `PythonConvertible`
  /// values representing a key.
  /// - Note: This is equivalent to `object[key]` in Python.
  public subscript(key: [PythonConvertible]) -> PythonObject? {
    get {
      let keyObject = flattenedSubscriptIndices(key)
      let selfObject = base.ownedPyObject
      defer {
        Py_DecRef(keyObject)
        Py_DecRef(selfObject)
      }

      // `PyObject_GetItem` returns +1 reference.
      if let result = PyObject_GetItem(selfObject, keyObject) {
        return PythonObject(owning: result)
      }
      PyErr_Clear()
      return nil
    }
    nonmutating set {
      let keyObject = flattenedSubscriptIndices(key)
      let selfObject = base.ownedPyObject
      defer {
        Py_DecRef(keyObject)
        Py_DecRef(selfObject)
      }

      if let newValue = newValue {
        let newValueObject = newValue.ownedPyObject
        PyObject_SetItem(selfObject, keyObject, newValueObject)
        Py_DecRef(newValueObject)
      } else {
        // Assigning `nil` deletes the key, just like Swift dictionaries.
        PyObject_DelItem(selfObject, keyObject)
      }
    }
  }

  /// Access the element corresponding to the specified `PythonConvertible`
  /// values representing a key.
  /// - Note: This is equivalent to `object[key]` in Python.
  public subscript(key: PythonConvertible...) -> PythonObject? {
    get {
      return self[key]
    }
    nonmutating set {
      self[key] = newValue
    }
  }

  /// Converts to a 2-tuple, if possible.
  public var tuple2: (PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1] else {
      return nil
    }
    return (elt0, elt1)
  }

  /// Converts to a 3-tuple, if possible.
  public var tuple3: (PythonObject, PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1], let elt2 = self[2] else {
      return nil
    }
    return (elt0, elt1, elt2)
  }

  /// Converts to a 4-tuple, if possible.
  public var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject)? {
    guard let elt0 = self[0], let elt1 = self[1],
          let elt2 = self[2], let elt3 = self[3] else {
      return nil
    }
    return (elt0, elt1, elt2, elt3)
  }
}

//===----------------------------------------------------------------------===//
// Core `PythonObject` API
//===----------------------------------------------------------------------===//

/// Converts an array of indices into a `PythonObject` representing a flattened
/// index.
private func flattenedSubscriptIndices(
  _ indices: [PythonConvertible]
) -> OwnedPyObjectPointer {
  if indices.count == 1 {
    return indices[0].ownedPyObject
  }
  return pyTuple(indices.map { $0.pythonObject })
}

public extension PythonObject {
  subscript(dynamicMember member: String) -> PythonObject {
    get {
      return checking[dynamicMember: member]!
    }
    nonmutating set {
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

  /// Access the element corresponding to the specified `PythonConvertible`
  /// values representing a key.
  /// - Note: This is equivalent to `object[key]` in Python.
  subscript(key: PythonConvertible...) -> PythonObject {
    get {
      return self.checking[key]!
    }
    nonmutating set {
      self.checking[key] = newValue
    }
  }

  /// Converts to a 2-tuple.
  var tuple2: (PythonObject, PythonObject) {
    return (self[0], self[1])
  }

  /// Converts to a 3-tuple.
  var tuple3: (PythonObject, PythonObject, PythonObject) {
    return (self[0], self[1], self[2])
  }

  /// Converts to a 4-tuple.
  var tuple4: (PythonObject, PythonObject, PythonObject, PythonObject) {
    return (self[0], self[1], self[2], self[3])
  }

  /// Call `self` with the specified arguments.
  /// - Precondition: `self` must be a Python callable.
  /// - Parameters:
  ///   - args: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  func call<T : PythonConvertible>(
    with args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PythonObject {
    return try! self.throwing.call(argArray: args, kwargs: kwargs)
  }

  /// Call `self` with the specified arguments.
  /// - Precondition: `self` must be a Python callable.
  /// - Parameters:
  ///   - args: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  func call(with args: PythonConvertible...,
            kwargs: [(String, PythonConvertible)] = []) -> PythonObject {
    return try! self.throwing.call(argArray: args.map { $0.pythonObject },
                                   kwargs: kwargs)
  }

  // Call a member with the specified arguments.
  /// - Precondition: The member must be a Python callable.
  /// - Parameters:
  ///   - name: The name of the member.
  ///   - argArray: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
  @discardableResult
  func callMember<T : PythonConvertible>(
    _ name: String,
    argArray args: [T],
    kwargs: [(String, PythonConvertible)] = []
  ) -> PythonObject {
    return try! self.throwing.callMember(name, argArray: args, kwargs: kwargs)
  }

  // Call a member with the specified arguments.
  /// - Precondition: The member must be a Python callable.
  /// - Parameters:
  ///   - name: The name of the member.
  ///   - args: Positional arguments for the Python callable.
  ///   - kwargs: Keyword arguments for the Python callable. Analoguous to
  ///     `kwargs` in Python.
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
// Python interface implementation
//===----------------------------------------------------------------------===//

/// The global Python interface.
///
/// You can import Python modules and access Python builtin types and functions
/// via the `Python` global variable.
///
///     import Python
///     // Import modules.
///     let os = Python.import("os")
///     let np = Python.import("numpy")
///
///     // Use builtin types and functions.
///     let list: PythonObject = [1, 2, 3]
///     print(Python.len.call(with: list)) // Prints 3.
///     print(Python.type.call(with: list) == Python.list) // Prints true.
@_fixed_layout
public let Python = PythonInterface()

/// An interface for Python.
///
/// `PythonInterface` allows interaction with Python. It can be used to import
/// modules and dynamically access Python builtin types and functions.
/// - Note: It is not intended for `PythonInterface` to be initialized
///   directly. Instead, please use the global instance of `PythonInterface`
///   called `Python`.
@_fixed_layout
@dynamicMemberLookup
public struct PythonInterface {
  /// A dictionary of the Python builtins.
  public let builtins: PythonObject

  init() {
    Py_Initialize()   // Initialize Python
    builtins = PythonObject(borrowing: PyEval_GetBuiltins())
  }

  public func attemptImport(_ name: String) throws -> PythonObject {
    guard let module = PyImport_ImportModule(name) else {
      try throwPythonErrorIfPresent()
      throw PythonError.invalidModule(name)
    }
    return PythonObject(owning: module)
  }

  public func `import`(_ name: String) -> PythonObject {
    return try! attemptImport(name)
  }

  public func updatePath(to path: String) {
    var cStr = path.utf8CString
    cStr.withUnsafeMutableBufferPointer { buffPtr in
      PySys_SetPath(buffPtr.baseAddress)
    }
  }

  public subscript(dynamicMember name: String) -> PythonObject {
    return builtins[name]
  }
}

//===----------------------------------------------------------------------===//
// Helpers for Python slice and tuple types
//===----------------------------------------------------------------------===//

private func pySlice(_ start: PythonConvertible?,
                     _ stop: PythonConvertible?,
                     _ step: PythonConvertible? = nil) -> OwnedPyObjectPointer {
  let startP = start?.ownedPyObject
  let stopP = stop?.ownedPyObject
  let stepP = step?.ownedPyObject

  // `PySlice_New` takes each operand at +0, and returns +1.
  let result = PySlice_New(startP, stopP, stepP)!

  Py_DecRef(startP)
  Py_DecRef(stopP)
  Py_DecRef(stepP) // `Py_DecRef` is `nil` safe.
  return result
}

// Create a Python tuple object with the specified elements.
private func pyTuple<T : Collection>(_ vals: T) -> OwnedPyObjectPointer
  where T.Element : PythonConvertible {

  let tuple = PyTuple_New(vals.count)!
  for (index, element) in vals.enumerated() {
    // `PyTuple_SetItem` steals the reference of the object stored.
    PyTuple_SetItem(tuple, index, element.ownedPyObject)
  }
  return tuple
}

public extension PythonObject {
  /// FIXME: This should be subsumed by Swift ranges and strides. Python has a
  /// very extravagant model though, it isn't clear how best to represent this
  /// in Swift.
  ///
  /// Initial thoughts are that we should sugar the obvious cases (so you can
  /// use 0...100 in a subscript) but then provide this method for the fully
  /// general case.
  ///
  /// We also need conditional conformances to allow range if PythonObject is to
  /// be a Slice. We can probably get away with a bunch of overloads for now
  /// given that slices are typically used with concrete operands.
  init(sliceStart start: PythonConvertible?,
                  stop: PythonConvertible?,
                  step: PythonConvertible? = nil) {
    self.init(owning: pySlice(start, stop, step))
  }

  // Tuples require explicit support because tuple types cannot conform to
  // protocols.
  init(tupleOf elements: PythonConvertible...) {
    self.init(tupleContentsOf: elements)
  }

  init<T : Collection>(tupleContentsOf elements: T)
    where T.Element == PythonConvertible {
    self.init(owning: pyTuple(elements.map { $0.pythonObject }))
  }

  init<T : Collection>(tupleContentsOf elements: T)
    where T.Element : PythonConvertible {
    self.init(owning: pyTuple(elements))
  }
}


//===----------------------------------------------------------------------===//
// `PythonConvertible` conformance for basic Swift types
//===----------------------------------------------------------------------===//

/// Return true if the specified objects an instance of the low-level Python
/// type descriptor passed in as 'type'.
private func isType(_ object: PythonObject,
                    type: UnsafeMutableRawPointer) -> Bool {
  let typePyRef = PythonObject(
    borrowing: type.assumingMemoryBound(to: PyObject.self)
  )
  let result = Python.isinstance.call(with: object, typePyRef)

  // We cannot use the normal failable Bool initializer from `PythonObject`
  // here because would cause an infinite loop.
  let pyObject = result.ownedPyObject
  defer { Py_DecRef(pyObject) }

  // Anything not equal to `Py_ZeroStruct` is truthy.
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
    _ = Python // Ensure Python is initialized.
    return PythonObject(owning: PyBool_FromLong(self ? 1 : 0))
  }
}

extension String : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    let pyObject = pythonObject.ownedPyObject
    defer { Py_DecRef(pyObject) }

    guard let cString = PyString_AsString(pyObject) else {
      PyErr_Clear()
      return nil
    }
    self.init(cString: cString)
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    let v = self.utf8CString.withUnsafeBufferPointer {
      // 1 is subtracted from the C string length to trim the trailing null
      // character (`\0`).
      PyString_FromStringAndSize($0.baseAddress, $0.count - 1)!
    }
    return PythonObject(owning: v)
  }
}

fileprivate extension PythonObject {
  // Converts a `PythonObject` to the given type by applying the appropriate
  // converter function and checking the error value.
  func converted<T : Equatable>(
    withError errorValue: T, by converter: (OwnedPyObjectPointer) -> T
  ) -> T? {
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
    // `PyInt_AsLong` return -1 and sets an error if the Python object is not
    // integer compatible.
    guard let value = pythonObject.converted(withError: -1,
                                             by: PyInt_AsLong) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    return PythonObject(owning: PyInt_FromLong(self))
  }
}

extension UInt : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    // `PyInt_AsUnsignedLongMask` isn't documented as such, but in fact it does
    // return -1 and set an error if the Python object is not integer compatible.
    guard let value = pythonObject.converted(
      withError: ~0, by: PyInt_AsUnsignedLongMask) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    return PythonObject(owning: PyInt_FromSize_t(Int(self)))
  }
}

extension Double : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    // `PyFloat_AsDouble` return -1 and sets an error if the Python object is not
    // float compatible.
    guard let value = pythonObject.converted(withError: -1,
                                             by: PyFloat_AsDouble) else {
      return nil
    }
    self = value
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    return PythonObject(owning: PyFloat_FromDouble(self))
  }
}

//===----------------------------------------------------------------------===//
// `PythonConvertible` conformances for `FixedWidthInteger` and `Float`
//===----------------------------------------------------------------------===//

/// Any `FixedWidthInteger` type is `PythonConvertible` via the `Int`/`UInt`
/// implementation.
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
// `PythonConvertible` conformance for `Array` and `Dictionary`
//===----------------------------------------------------------------------===//

// `Array` conditionally conforms to `PythonConvertible` if the `Element`
// associated type does.
extension Array : PythonConvertible where Element : PythonConvertible {
  public init?(_ pythonObject: PythonObject) {
    self = []
    for elementObject in pythonObject {
      guard let element = Element(elementObject) else { return nil }
      append(element)
    }
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    let list = PyList_New(count)!
    for (index, element) in enumerated() {
      // `PyList_SetItem` steals the reference of the object stored.
      PyList_SetItem(list, index, element.ownedPyObject)
    }
    return PythonObject(owning: list)
  }
}

// `Dictionary` conditionally conforms to `PythonConvertible` if the `Key` and
// `Value` associated types do.
extension Dictionary : PythonConvertible
  where Key : PythonConvertible, Value : PythonConvertible {
  public init?(_ pythonDict: PythonObject) {
    self = [:]

    // Iterate over the Python dictionary, converting its keys and values to
    // Swift `Key` and `Value` pairs.
    var key, value: UnsafeMutablePointer<PyObject>?
    var position: Py_ssize_t = 0

    while PyDict_Next(pythonDict.borrowedPyObject,
                      &position, &key, &value) != 0 {
      // If any key or value is not convertible to the corresponding Swift
      // type, then the entire dictionary is not convertible.
      if let swiftKey = Key(PythonObject(borrowing: key!)),
         let swiftValue = Value(PythonObject(borrowing: value!)) {
        // It is possible that there are duplicate keys after conversion. We
        // silently allow duplicate keys and pick a nondeterministic result if
        // there is a collision.
        self[swiftKey] = swiftValue
      } else {
        return nil
      }
    }
  }

  public var pythonObject: PythonObject {
    _ = Python // Ensure Python is initialized.
    let dict = PyDict_New()!
    for (key, value) in self {
      let k = key.ownedPyObject
      let v = value.ownedPyObject
      PyDict_SetItem(dict, k, v)
      Py_DecRef(k)
      Py_DecRef(v)
    }
    return PythonObject(owning: dict)
  }
}

//===----------------------------------------------------------------------===//
// `PythonConvertible` conformance for `Range` types
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
    _ = Python // Ensure Python is initialized.
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
    _ = Python // Ensure Python is initialized.
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
    _ = Python // Ensure Python is initialized.
    return PythonObject(sliceStart: nil, stop: self.upperBound, step: nil)
  }
}

//===----------------------------------------------------------------------===//
// Standard operators and conformances
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
    self.init(Int(value))
  }

  public typealias Magnitude = PythonObject

  public var magnitude: PythonObject {
    return self < 0 ? -self : self
  }
}

extension PythonObject : Equatable, Comparable, Hashable {
  // `Equatable` and `Comparable` are implemented using rich comparison.
  // This is consistent with how Python handles comparisons.
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
      fatalError("Cannot use '__hash__' on \(self)")
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

  public func index(after i: Index) -> Index {
    return i + PythonObject(1)
  }

  public subscript(index: PythonObject) -> PythonObject {
    get {
      return self[index as PythonConvertible]
    }
    set {
      self[index as PythonConvertible] = newValue
    }
  }
}


//===----------------------------------------------------------------------===//
// `ExpressibleByLiteral` conformances
//===----------------------------------------------------------------------===//

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
