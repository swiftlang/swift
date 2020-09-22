# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            dependsonmain.swiftdeps
    fingerprint:     after
    sequenceNumber:  0
    defsIDependUpon: [ 2 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            dependsonmain.swiftdeps
    fingerprint:     after
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            main
    sequenceNumber:  2
    defsIDependUpon: [ ]
    isProvides:      false
...
