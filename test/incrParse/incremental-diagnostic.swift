// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case REPLACE

let myFont: Font = {
    let myFont = Font.body
    return myFont.weight(.<<REPLACE<bold|||light>>>)
}()
