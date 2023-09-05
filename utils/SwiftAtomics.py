#===-----------------------------------------------------------------------===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2020 - 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===-----------------------------------------------------------------------===//

atomicTypes = [
  # Swift    64 bit   32 bit
  ("Int8",   "Int8",  "Int8"),
  ("Int16",  "Int16", "Int16"),
  ("Int32",  "Int32", "Int32"),
  ("Int64",  "Int64", "Int64"),
  ("Int",    "Int64", "Int32"),
  ("UInt8",  "Int8",  "Int8"),
  ("UInt16", "Int16", "Int16"),
  ("UInt32", "Int32", "Int32"),
  ("UInt64", "Int64", "Int64"),
  ("UInt",   "Int64", "Int32")
]

loadOrderings = [
  # Swift                    API name                  doc name                   LLVM name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic"),
  ("acquiring",              "Acquiring",              "acquiring",               "acquire"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst")
]

storeOrderings = [
  # Swift enum case,         API name,                 doc name,                  LLVM name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic"),
  ("releasing",              "Releasing",              "releasing",               "release"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst")
]

updateOrderings = [
  # Swift enum case,         API name,                 doc name,                  LLVM name,   failure name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic", "monotonic"),
  ("acquiring",              "Acquiring",              "acquiring",               "acquire",   "acquire"),
  ("releasing",              "Releasing",              "releasing",               "release",   "monotonic"),
  ("acquiringAndReleasing",  "AcquiringAndReleasing",  "acquiring-and-releasing", "acqrel",    "acquire"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst",    "seqcst"),
]

def shim_name(llvmOrdering):
  if llvmOrdering == 'monotonic':
    return "relaxed"
  if llvmOrdering == 'acquire':
    return "acquire"
  if llvmOrdering == 'release':
    return "release"
  if llvmOrdering == 'acqrel':
    return "acq_rel"
  if llvmOrdering == 'seqcst':
    return "seq_cst"
  raise ValueError("Unknown ordering " + llvmOrdering)

def isStrongerThan(rmw, load): # See llvm/Support/AtomicOrdering.h
  if rmw == "sequentiallyConsistent":
    return load == "relaxed" or load == "acquiring"
  if rmw == "acquiringAndReleasing":
    return load == "relaxed"
  if rmw == "releasing":
    return False
  if rmw == "acquiring":
    return load == "relaxed"
  if rmw == "relaxed":
    return False

def failureOrderingOf(update):
  if update == "release":
    return "monotonic"
  if update == "acqrel":
    return "acquire"
  return update

# LLVM doesn't support arbitrary ordering combinations yet, so for the
# two-ordering cmpxchg variants we need to upgrade the success
# ordering when necessary so that it is at least as "strong" as the
# failure case. This function implements that mapping.
#
# See llvm/Support/AtomicOrdering.h
def actualOrders(success, failure):
  def max(success, failure):
    if failure == "acquire":
      if success == "monotonic":
        return "acquire"
      if success == "release":
        return "acqrel"
    if failure == "seqcst":
      return "seqcst"
    return success
  actualSuccess = max(success, failure)
  return actualSuccess + "_" + failure

def actualShimOrders(success, failure):
  r=actualOrders(success, failure).split("_")
  return shim_name(r[0]) + "_" + shim_name(r[1])

integerOperations = [
  # Swift name,         llvm name,  operator, label,  doc
  ("WrappingIncrement", "add",      "&+",     "by",   "wrapping add"),
  ("WrappingDecrement", "sub",      "&-",     "by",   "wrapping subtract"),
  ("BitwiseAnd",        "and",      "&",      "with", "bitwise AND"),
  ("BitwiseOr",         "or",       "|",      "with", "bitwise OR"),
  ("BitwiseXor",        "xor",      "^",      "with", "bitwise XOR")
]

boolOperations = [
  # Swift name,         Int8 name,    operator, label,  doc
  ("LogicalAnd",        "BitwiseAnd", "&&",     "with", "logical AND"),
  ("LogicalOr",         "BitwiseOr",  "||",     "with", "logical OR"),
  ("LogicalXor",        "BitwiseXor", "!=",     "with", "logical XOR")
]

def lowerFirst(str):
  return str[:1].lower() + str[1:] if str else ""

def argLabel(label):
  return label + ": " if label != "_" else ""

def bitwidth_variants(value64, value32):
  if value64 == value32:
    return [("shared", value64),]
  return [
    ("32", value32),
    ("64", value64),
  ]

ptrBitWidth32 = "_pointerBitWidth(_32)"

def archConditionStart(variant):
  if variant == "shared":
    return ""
  if variant == "32":
    return "#if " + ptrBitWidth32
  return "#else /* " + ptrBitWidth32 + " */"

def archConditionEnd(variant):
  if variant == "64":
    return "#endif /* " + ptrBitWidth32 + " */"
  return ""
