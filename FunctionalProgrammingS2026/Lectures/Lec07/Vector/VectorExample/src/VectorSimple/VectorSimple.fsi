// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.2: Interface for simple vector module

module VectorSimple                      // Vector signature
type Vector
val ( ~-. ) : Vector -> Vector           // Vector sign change
val ( +. )  : Vector -> Vector -> Vector // Vector sum
val ( -. )  : Vector -> Vector -> Vector // Vector difference
val ( *. )  : float  -> Vector -> Vector // Product wth number
val ( &. )  : Vector -> Vector -> float  // Dot product
val norm    : Vector -> float            // Length of vector
val make    : float * float -> Vector    // Make vector
val coord   : Vector -> float * float    // Get coordinates
