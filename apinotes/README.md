# API Notes README

API notes provide a mechanism by which Objective-C APIs can be
annotated with additional semantic information not present within the
original Objective-C headers. This semantic information can then be
used by the Swift compiler when importing the corresponding Objective-C
module to provide a better mapping of Objective-C APIs into Swift.

API notes are organized into a set of `.apinotes` files. Each `.apinotes` file
contains annotations for a single Objective-C module, written in YAML (FIXME:
to be) described in the Clang repository. These YAML sources are lazily loaded
by the Swift compiler when it imports the corresponding framework, also
described below.

To add API notes for a system module `$MODULE` that does not have them yet,
create a new source file `$MODULE.apinotes` and update CMakeLists.txt.
Updated API notes will be found by the build system during the next build.
