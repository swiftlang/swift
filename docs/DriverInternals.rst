=========================
Driver Design & Internals
=========================

.. contents::
   :local:

Introduction
============

This document serves to describe the high-level design of the Swift 1.0 compiler
driver (which includes what the driver is intended to do, and the approach it
takes to do that), as well as the internals of the driver (which is meant to
provide a brief overview of and rationale for how the high-level design is
implemented).

The Swift 1.0 driver is not intended to be GCC/Clang compatible, as it does not
need to serve as a drop-in replacement for either driver. However, the design
of the driver is inspired by Clang's design (though it is "smarter" than Clang,
since it performs some limited dependency analysis), and will only diverge if it
produces a better design, or if it needs to implement something which Clang
does not.

High-Level Driver Design
========================

The compiler driver for Swift will roughly follow the same design as Clang's
compiler driver: it will parse command-line arguments into Arg objects; use
those to generate a pipeline of Actions; bind Tools to each Action, which will
then translate an Action into a Job; execute Jobs; and return a result code.
However, since Swift source files must be compiled on a per-module basis,
external build systems cannot easily detect while files in a module must be
recompiled to accurately produce an executable. As a result, the Swift compiler
driver must perform dependency tracking on its own, which means that all Jobs
will not be executed. Instead, Jobs will be logically grouped, and each
Job will have the opportunity to influence whether other Jobs in the same
logical group will be executed.

.. contents::
   :local:

Overview
--------

The diagram below, taken from Clang's "Driver Design & Internals" document,
shows the high-level design of the Swift compiler driver.

.. admonition:: TODO

  Update diagram to show conditional Job execution

.. image:: DriverDesign.png
   :align: center
   :alt: Driver Design Diagram

1. Parse input strings into an ArgList of Args.

2. Establish a pipeline of Action groups, such as the following:

	- A0: Input, "a.swift"

	- A1: Input, "b.swift"

	- B0: Compile, {A0}, "a.o"

	- B1: Compile, {A1}, "b.o"

	- C0: Link, {B0, B1}, "a.out"

4. Bind the appropriate Tool to each Action.

5. Using the bound Tools, translate each Action into a Job, creating a graph.

6. Execute each top-level Job by performing the following:

	1. Ask each Job which is an input to the current Job if it needs to
	   execute. This will have the side-effect of loading dependency
	   information from the last build (if present). If a Job already
	   knows that it needs to execute, schedule it for execution.
	
	2. Execute each Job which is scheduled for execution. This will have the
	   side-effect of creating new dependency information for that Job.
	
	3. After each Job finishes execution, load the new dependency information
	   and reevaluate every Job which is a peer to that Job.

7. After all top-level Jobs been processed, the build artifacts should be
   present on disk, either from a new execution or from a previous execution.
   Return a result code.

Driver Stages
-------------

The Swift compiler driver is conceptually broken into five stages: Parse
(transforming input strings to ArgList/Args), Pipeline (transforming Args into
groups of Actions), Bind (assigning Tools and other build information to
Actions), Translate (using Tools to translate Actions into Jobs), and Execute.
From a high level, these look like Clang's driver stages, and functionally
they're similar. However, unlike Clang, Translate and Execute will, optimally,
only be performed on a subset of Actions, and the execution of one Action will
influence whether or not another Action is executed.

Parse: Option parsing
^^^^^^^^^^^^^^^^^^^^^

This is a fairly straightforward port of the Clang driver's Parse stage. The
command line arguments are parsed as options and inputs into Arg instances.

Pipeline: Converting Args into Actions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is also a fairly straightforward port of the Clang driver's Pipeline stage.
At this stage, the driver will take the input Args and input files and establish
a graph of Actions.

Bind: Tool and Filename Selection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This stage, like the Clang driver's Bind stage, selects an appropriate Tool to
use for each Action in a pipeline. This is achieved by asking the Toolchain for
the right Tool for a given Action. Once every Action in the pipeline has a Tool,
this stage will determine how to pass output from one Action to the next.

For Actions which do not already have output filenames but require one, this
stage will also assign unique output filenames.

Translate: Translating Actions into Jobs using Tools
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This stage, like the Clang driver's Translate stage, uses the Tool bound to each
Action to translate the Args in ArgList into tool-specific arguments. Unlike
Clang's Translate stage, though, the Swift driver will translate the graph of
Actions into a graph of Jobs, instead of putting it into a serial queue.

This stage must result in a graph of Jobs instead of a queue of Jobs so that
Jobs can remain logically grouped. All Jobs which are an input to a particular
Job will be given the opportunity to impact whether other Jobs in that logical
group need to be executed; this permits us to perform partial rebuilds when
safe.

Execute
^^^^^^^

This stage, like the Clang driver's Execute stage, executes the Jobs which are
created by Translate. Unlike Clang's Execute stage, Swift's will support
concurrency: at the most basic level, this will be something like ``make -jn``,
where the compiler executes up to n Jobs concurrently. This could be enhanced to
include things like intelligently scaling back the number of Jobs if the system
is under pressure, but that may not be necessary for Swift 1.0. (Another
possible enhancement would be to let an external build system update the value
of n as the build continues, but that will definitely not be necessary for 1.0.)

Jobs will be scheduled onto a single work queue. Multiple Jobs may execute
simultaneously, but Job termination will be handled on a single thread. When a
Job terminates, the driver will evaluate the other Jobs in that Job's group
to determine if any additional Jobs need to be scheduled. Once all outstanding
Jobs in the same group have terminated, any unprocessed Jobs will be evaluated
before executing the downstream Job for which all of the Jobs in that group are
an input.

Driver Internals
================

TBD
