/**
  * This is a work towards extracting mathematics from the literature (i.e. from TEX files), 
  * which could be used in theorem prover's learning.
  * 
  *  The implementation of the parser is split into two steps:
  *   - [[DeTeX]] : removing .tex commands and classifying contents into blocks of 
  *       paragraph, list, image, definition, theorems, proofs, equations, etc.
  *   - [[MathParser]] : tokenizing math obejcts from theorems, proofs, equation, etc; 
  *       following the styles of First Order Logic and Set Theory.
  * 
  *  These are contained in ``shared`` package. 
  */ 
  package object partex {}
