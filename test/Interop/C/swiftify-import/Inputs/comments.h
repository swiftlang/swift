#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

void begin();

// line comment
void lineComment(int len, int * __counted_by(len) p);

/// line doc comment
/// 
/// Here's a more complete description.
///
/// @param len the buffer length
/// @param p the buffer
void lineDocComment(int len, int * __counted_by(len) p);

/*
 block comment
 */
void blockComment(int len, int * __counted_by(len) p);

/**
 * block doc comment
 * 
 * NB: it's very important to pass the correct length to this function
 * @param len don't mess this one up
 * @param p   some integers to play with
 */
void blockDocComment(int len, int * __counted_by(len) p);
