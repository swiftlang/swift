# Fine-grained v0
---
allNodes:
  - key:
      kind:            sourceFileProvide
      aspect:          interface
      context:         ''
      name:            main.swiftdeps
    fingerprint:     f216f45027a3fa6bf3d16c1b05dd8feb
    sequenceNumber:  0
    defsIDependUpon: [ 6, 5, 2, 4 ]
    isProvides:      true
  - key:
      kind:            sourceFileProvide
      aspect:          implementation
      context:         ''
      name:            main.swiftdeps
    fingerprint:     f216f45027a3fa6bf3d16c1b05dd8feb
    sequenceNumber:  1
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            V
    sequenceNumber:  2
    defsIDependUpon: [ 0 ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          implementation
      context:         ''
      name:            V
    sequenceNumber:  3
    defsIDependUpon: [  ]
    isProvides:      true
  - key:
      kind:            topLevel
      aspect:          interface
      context:         ''
      name:            a
    sequenceNumber:  4
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            externalDepend
      aspect:          interface
      context:         ''
      name:            './main1-external'
    sequenceNumber:  5
    defsIDependUpon: [  ]
    isProvides:      false
  - key:
      kind:            externalDepend
      aspect:          interface
      context:         ''
      name:            './main2-external'
    sequenceNumber:  6
    defsIDependUpon: [  ]
    isProvides:      false
...
