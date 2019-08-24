Pass Pipeline
-------------

Purpose
=======

``Pass Pipeline`` is a python library for generating pipeline json descriptors
for Swift. The goal is to enable a user to generate various pass pipelines
without needing to recompile the compiler. The main targets for this work are:

1. Correctness issues exposed by adding and removing parts of pass pipelines.
2. Exploration of various pipelines from a performance perspective.

Why not just use sil-opt and generate IR from sil for this purpose?
===================================================================

This is necessary since currently we do not have the infrastructure to always be
able to use sil-opt and friends to generate IR from a .sil file. This is a
shortcut around such issues.

The JSON Descriptor Format
==========================

The general format is as follows:

  [
    [
      "PASS_MANAGER_ID",
      "run_n_times"|"run_to_fixed_point",
      count,
      "PASS1", "PASS2", ...
    ],
    ...
  ]

Where "id" is printed out when we process the action, "action" can be one of
"run_n_times", "run_to_fixed_point" and "passes" is a list of passes to
run. The names to use are the stringified versions of pass kinds.

Structure
=========

There are two parts to this library. In ``src``, we have the library building
blocks. This is where we define the passes and the general pipeline
infrastructure. In ``scripts``, is where we implement various pipeline
generators using code from ``src``.
