DWARF debug information for Swift
=================================

.. contents::

Abstract
--------

This document describes the DWARF debug information that is emitted by the Swift compiler.

Deviations from the DWARF standard
----------------------------------

Swift adds a ``DW_AT_linkage_name`` attribute to types to uniquely
identify a type. This allows the debugger to import the type from the
module it is defined in rather than having to recreate it from the
DWARF representation.

Variable Locations
------------------

* Structs, Tuples
  A location for a struct or tuple describes the actual data.

* Classes
  A location for a class describes the pointer to the object.

