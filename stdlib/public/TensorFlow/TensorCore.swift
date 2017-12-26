//===-- TensorCore.swift --------------------------------------*- swift -*-===//
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
// This file defines the TensorHandle type and the primitive Tensor "op"
// functions, in a simple and predictable style that can be mapped onto
// TensorFlow ops.
//
//===----------------------------------------------------------------------===//

import Swift

public class TensorHandle<T: TensorElementProtocol> {
  // FIXME: Implement in terms of a TensorFlow TensorHandle, using the C API.
}

// For "print", REPL, and Playgrounds integeration, we'll eventually want to
// implement this, probably in terms of fetching a summary.  For now, this is
// disabled.
#if false
/// Make "print(someTensor)" print a pretty form of the tensor.
extension TensorHandle : CustomStringConvertible {
  public var description: String {
    fatalError("unimplemented")
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension TensorHandle : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}
#endif

//===----------------------------------------------------------------------===//
// Primitive "Ops"
//===----------------------------------------------------------------------===//
//
// Ops should eventually have a new attribute like @tf.op(...) that encapsulates
// their magic and weird behavior, including specifying the TensorFlow op they
// correspond to and associating a derivative function.  That said, for now we
// are doing hacky things to get the prototype off the ground, as described
// below.
//
// The implementation of these functions are all marked @inline(never) as an
// implementation hack that allows the tf-compiler partitioning pass to see
// the ops instead of having them get inlined away.
//
// These each get a @_versioned attribute, which allows the symbol to be
// accessible outside of the module even though it doesn't have public access
// control.  This is required because the Tensor type and higher level
// operations are all @_inlineable.  Trivia: @_versioned and @_inlineable are
// going to get renamed in mainline in the next couple months.
//
// The body of these ops just abort, since they are never actually run: these
// are just markers in the code that are seen by the tf-compiler partitioning
// pass and then removed, so the bodies are never used.
//
// These ops are also marked with @_silgen_name, which gives them simple and
// predictable names that are easy to match in the partitioner and then lower
// into graphs.  Op functions that correspond to TensorFlow ops are expected to
// be of the form:
//   @inline(never) @_silgen_name("__tfop_<OPNAME>__<OPERANDINFO>__")
//
// The <OPNAME> corresponds to the TensorFlow op name, e.g. Add or Const.  The
// operand info is an encoding of the operands to the function with instructions
// on how to transform them into in the graph node.  Here are the codes that are
// recognized so far:
//
//    t: the next operand is a TensorHandle, and is an "input" to the TF node.
//    d: the dtype attribute must be added, corresponding to the last tensor
//       operand we saw, or the result type of the function if first.
//    c: the next operand is a standard library integer or FP type.  We should
//       pass the value(s) as the 'value' attribute.
//
// The tf-compiler expects that all operands passed to the op function will be
// described by the codes above.  The node created will have results equal to
// the results of the function, currently they must all be TensorHandle results.
//
// Here are some example encodings:
//   1) 'Add' is @_silgen_name("__tfop_Add__tt__").
//   2) Literal0d form of 'Const' is @_silgen_name("__tfop_Const__dc__").
//

// A simple wrapper to make the bodies of the op functions more concise.
func opBody() -> Never {
  Builtin.unreachable()
}

/// tfop_send marks the specified Tensor source as intentionally being on the
/// host, and that the result Tensor is intended to be on the accelerator.  This
/// enables warnings in the compiler partitioning algorithm about cases of
/// unanticipated copies back and forth.
@_versioned @inline(never)
@_silgen_name("__tfop_send") // Special name, not a TF op!
func tfop_send<T>(_ c : TensorHandle<T>) -> TensorHandle<T> {
  return c
}

/// tfop_receive marks the specified Tensor source as intentionally being on the
/// accelerator, and that the result Tensor is intended to be on the host.  This
/// enables warnings in the compiler partitioning algorithm about cases of
/// unanticipated copies back and forth.
@_versioned @inline(never)
@_silgen_name("__tfop_receive") // Special name, not a TF op!
func tfop_receive<T>(_ c : TensorHandle<T>) -> TensorHandle<T> {
  return c
}

@_versioned @inline(never) @_silgen_name("_tfop_rank")
func tfop_rank<T>(_ c : TensorHandle<T>) -> Int { opBody() }


@_versioned @inline(never) @_silgen_name("_tfop_shape")
func tfop_shape<T>(_ c : TensorHandle<T>) -> [Int] { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Const__dc__")
func tfop_literal_0d<T>(_ value: T) -> TensorHandle<T> { opBody() }


// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_literal_1d_")
func tfop_literal_1d<T>(_ values: [T]) -> TensorHandle<T> {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_literal_2d_")
func tfop_literal_2d<T>(_ values: [[T]]) -> TensorHandle<T> {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_init_zeros")
func tfop_init_zeros<T>(_ dimensions: UnsafeBufferPointer<Int>)
  -> TensorHandle<T> {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_init_ones")
func tfop_init_ones<T>(_ dimensions: UnsafeBufferPointer<Int>)
  -> TensorHandle<T> {
  fatalError("this should become an op")
}


// def tf.random_normal(shape, mean=0.0, stddev=1.0, dtype=dtypes.float32,
//                      seed=None, name=None):
// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_init_random_normal")
func tfop_init_random_normal<T>(_ dimensions: [Int], _ mean: Double,
                                _ stddev: Double) -> TensorHandle<T> {
  fatalError("this should become an op")
}

// def tf.eye(shape)
// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_init_eye")
func tfop_init_eye<T>(_ dimensions: [Int]) -> TensorHandle<T> {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_subscript_tensor")
func tfop_subscript_tensor<T>(_ c : TensorHandle<T>,
                              _ indices : [Int]) -> TensorHandle<T> {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_subscript_scalar_")
func tfop_subscript_scalar<T>(_ c : TensorHandle<T>, _ indices : [Int]) -> T {
  fatalError("this should become an op")
}

// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_slice")
func tfop_slice<T>(_ c : TensorHandle<T>, start: Int, end: Int)
  -> TensorHandle<T> {
  fatalError("this should become an op")
}

/// Broadcast the specified Tensor to a rank >= its current size, filling in the
/// new dimensions with rank = 1.
/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_broadcast")
func tfop_broadcast<T>(_ c : TensorHandle<T>, _ rank: Int) -> TensorHandle<T> {
  fatalError("this should become an op")
}

/// Broadcast tensor A to the same shape as B
/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_broadcast_to")
func tfop_broadcast_to<T>(_ a : TensorHandle<T>,
                          _ b: TensorHandle<T>) -> TensorHandle<T> {
  fatalError("this should become an op")
}


/// Broadcast the specified scalar value to be a tensor with the same rank as
/// the specified other tensor, but with dimension=1 for each rank.
/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_broadcast_scalar_")
func tfop_broadcast_scalar<T>(_ value : T, _ rank: Int) -> TensorHandle<T> {
  fatalError("this should become an op")
}

/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_transpose")
func tfop_transpose<T>(_ value : TensorHandle<T>) -> TensorHandle<T> {
  fatalError("this should become an op")
}

@_versioned @inline(never) @_silgen_name("__tfop_Tanh__t__")
func tfop_tanh<T>(_ value : TensorHandle<T>) -> TensorHandle<T> { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Log__t__")
func tfop_log<T>(_ value : TensorHandle<T>) -> TensorHandle<T> { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Exp__t__")
func tfop_exp<T>(_ value : TensorHandle<T>) -> TensorHandle<T> { opBody() }

/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_sum_")
func tfop_sum<T>(_ c : TensorHandle<T>) -> T {
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
// TODO: Push the indices array out of this.  Use UnsafeBufferPointer instead.
@_versioned @inline(never) @_silgen_name("_tfop_sum_axis")
func tfop_sum_axis<T>(_ c : TensorHandle<T>, _ axis : [Int]) -> TensorHandle<T>{
  fatalError("this should become an op")
}

// FIXME: Teach the partitioner about this.  For now, this executes on the host.
// FIXME: The issue is that we can't reason about array parameters.
@_versioned @inline(never) @_silgen_name("_tfop_max_axis")
func tfop_max_axis<T>(_ c : TensorHandle<T>, _ axis : [Int]) -> TensorHandle<T>{
  fatalError("this should become an op")
}


/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_mean_")
func tfop_mean<T>(_ c : TensorHandle<T>) -> T {
  fatalError("this should become an op")
}

/// FIXME: Bind this to a proper TensorFlow op.
/// TODO: Should this return a Tensor0D to keep it in the guaranteed tensor
/// domain?
@_versioned @inline(never) @_silgen_name("_tfop_max_")
func tfop_max<T>(_ c : TensorHandle<T>) -> T {
  fatalError("this should become an op")
}

/// FIXME: Bind this to a proper TensorFlow op.
/// TODO: Should this return a Tensor0D to keep it in the guaranteed tensor
/// domain?
@_versioned @inline(never) @_silgen_name("_tfop_argmax_")
func tfop_argmax<T>(_ c : TensorHandle<T>) -> Int64 {
  fatalError("this should become an op")
}

/// FIXME: Bind this to a proper TensorFlow op.
/// TODO: Should this return a Tensor0D to keep it in the guaranteed tensor
/// domain?
@_versioned @inline(never) @_silgen_name("_tfop_min_")
func tfop_min<T>(_ c : TensorHandle<T>) -> T {
  fatalError("this should become an op")
}

/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_elt_convert")
func tfop_elt_convert<From, To>(_ c : TensorHandle<From>) -> TensorHandle<To> {
  fatalError("this should become an op")
}

@_versioned @inline(never) @_silgen_name("__tfop_Add__tt__")
func tfop_elt_add<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<T> { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Sub__tt__")
func tfop_elt_subtract<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<T> { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Mul__tt__")
func tfop_elt_multiply<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<T> { opBody() }

@_versioned @inline(never) @_silgen_name("__tfop_Div__tt__")
func tfop_elt_divide<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<T> { opBody() }

/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_elt_matmul")
func tfop_elt_matmul<T>(_ lhs : TensorHandle<T>,
                        _ rhs : TensorHandle<T>) -> TensorHandle<T> {
  fatalError("this should become an op")
}

@_versioned @inline(never) @_silgen_name("__tfop_Less__tt__")
func tfop_elt_less<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<Bool> { opBody() }

/// FIXME: Bind this to a proper TensorFlow op.
@_versioned @inline(never) @_silgen_name("_tfop_concat")
func tfop_concat<T>(_ lhs : TensorHandle<T>, _ rhs : TensorHandle<T>)
  -> TensorHandle<T> {
  fatalError("this should become an op")
}

