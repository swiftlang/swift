// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

extension Integer { 
#^A^#
  extension {
        var : Self   
