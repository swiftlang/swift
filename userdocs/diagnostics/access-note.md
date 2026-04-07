# Invalid or successful access notes (AccessNote)

Remarks to verify application of access notes, and errors diagnosing access note failures.

## Overview

By providing an access notes file, you can annotate Swift declarations with `@objc` without modifying the source code. To verify that they are applied correctly you can enable remark output during compilation.

Available options:
 - -Raccess-note=none
   - don't emit access note diagnostics
 - -Raccess-note=failure
   - emit remark for access notes that were not applied because of a failure
 - -Raccess-note=all
   - in addition to failures, also emit remarks for successfully applied access notes
 - -Raccess-note=all-validate
   - emit remarks for successfully applied access notes, and emit *errors* for failures

## Using access notes

Access notes use the YAML file format and can be supplied using `-access-notes-path foo.accessnotes`. Example file:

```yaml
---
Reason: this text is included in diagnostics
Notes:
- Name: 'TypeName.methodName(_:)'
  ObjC: true
  ObjCName: 'customObjCSelector:'
  Dynamic: true
- Name: 'getter:Foo.someComputedProp()'
  ObjCName: 'getSomeComputedProp'
- Name: 'setter:Foo.someComputedProp()'
  ObjCName: 'setSomeComputedProp:'
```

`-Raccess-note=<arg>` is a frontend-only option intended for testing and debugging. It is not exposed in the compiler driver.
