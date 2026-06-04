// Prøv at skrive signaturfilen .fsi. Den skal indeholde:

// typen Complex
// en funktion make der laver et komplekst tal fra to floats
// en funktion re der giver realdelen
// en funktion im der giver imaginærdelen
// 
// Hvad tror du make sin type er?

module Complex

[<Sealed>]
type Complex =
    static member (+) : Complex * Complex -> Complex
    static member (-) : Complex * Complex -> Complex
    static member (*) : Complex * Complex -> Complex
    static member (/) : Complex * Complex -> Complex

val make : float -> float -> Complex
val real : Complex ->float
val imaginary : Complex -> float

