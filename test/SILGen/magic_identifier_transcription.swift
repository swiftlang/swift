// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func f(_ expression: Bool, transcription: String = #transcription(of: expression)) { }

func g() {
    f(1 + 1 == 2)
    
    // CHECK: string_literal utf8 "1 + 1 == 2"
}
