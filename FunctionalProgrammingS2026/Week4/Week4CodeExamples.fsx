// ============================================================
// KAPITEL 5: Collections – Lists, Maps og Sets
// F# kodeeksempler med kommentarer
// ============================================================


// ============================================================
// DEL 1: LIST HIGHER-ORDER FUNKTIONER
// ============================================================

// --- List.map ---
// Anvender en funktion på HVERT element og returnerer en ny liste.
// Den originale liste ændres ikke.

let priser = [10; 20; 30; 40]

let prisMedMoms = List.map (fun p -> p * 125 / 100) priser
// priser     = [10; 20; 30; 40]
// prisMedMoms = [12; 25; 37; 50]

// Samme med pipe-operator |> (læses "send dette videre til")
let prisMedMoms2 = priser |> List.map (fun p -> p * 125 / 100)


// --- List.filter ---
// Returnerer kun de elementer hvor predikatet er sandt.

let tal = [1; 7; 2; 9; 4; 6; 3]

let storreTal = List.filter (fun x -> x > 4) tal
// storreTal = [7; 9; 6]


// --- List.exists og List.forall ---
// exists: er der MINDST ÉT element der opfylder betingelsen?
// forall: opfylder ALLE elementer betingelsen?

let navne = ["Lars"; "Mette"; "Ole"; "Anna"]

let erDerEtKortNavn = List.exists (fun s -> s.Length <= 3) navne
// true  (Ole har 3 tegn)

let alleHarMindst3Tegn = List.forall (fun s -> s.Length >= 3) navne
// true


// --- List.tryFind ---
// Finder det FØRSTE element der matcher – returnerer Some x eller None.
// Bruger option-typen så programmet ikke crasher ved "ikke fundet".

let karakterer = [4; 7; 2; 10; 12]

let foersteBestaet = List.tryFind (fun k -> k >= 7) karakterer
// foersteBestaet = Some 7

let foersteOver12 = List.tryFind (fun k -> k > 12) karakterer
// foersteOver12 = None

// Sådan håndterer du resultatet:
let besked =
    match foersteBestaet with
    | Some k -> sprintf "Første beståede karakter: %d" k
    | None   -> "Ingen beståede karakterer"


// ============================================================
// List.fold – den vigtigste og sværeste
// ============================================================
//
// fold tager:
//   - en akkumulatorfunktion  f : akkumulator -> element -> akkumulator
//   - en startværdi           e
//   - en liste                [x0; x1; ...; xn]
//
// og beregner: f( ... f( f(e, x0), x1) ..., xn)
// dvs. den "folder" funktionen hen over listen fra venstre.

// Eksempel 1: summer alle tal i en liste
let sum = List.fold (fun acc x -> acc + x) 0 [1; 2; 3; 4; 5]
// Skridt for skridt:
//   fold starter med acc = 0
//   acc = 0 + 1 = 1
//   acc = 1 + 2 = 3
//   acc = 3 + 3 = 6
//   acc = 6 + 4 = 10
//   acc = 10 + 5 = 15
// sum = 15

// Eksempel 2: find maksimum i en liste
let maksimum lst =
    match lst with
    | []    -> failwith "Tom liste har intet maksimum"
    | x::xs -> List.fold (fun maks el -> if el > maks then el else maks) x xs

let maks = maksimum [3; 1; 7; 2; 9; 4]
// maks = 9

// Eksempel 3: tæl elementer der opfylder et predikat (uden at bruge filter)
let taelStorre4 = List.fold (fun acc x -> if x > 4 then acc + 1 else acc) 0 tal
// tal = [1; 7; 2; 9; 4; 6; 3]
// taelStorre4 = 3  (7, 9 og 6)


// --- List.foldBack ---
// Samme ide som fold, men akkumulerer fra HØJRE mod venstre.
// Parametrenes rækkefølge i akkumulatorfunktionen er byttet om:
//   f : element -> akkumulator -> akkumulator

// Eksempel: byg en liste der filtrerer og transformerer i ét hug
let bearbejdet =
    List.foldBack
        (fun x acc -> if x > 3 then (x * 2) :: acc else acc) // :: skubber elemnter ind fra starten 
        [1; 5; 2; 8; 3; 6]
        []
// Starter fra højre, tilføjer x*2 til acc hvis x > 3
// Resultat = [10; 16; 12]   (5*2, 8*2, 6*2)

// Vigtig forskel på fold vs foldBack med ikke-kommutativ operation:
let medFold     = List.fold     (-)  0 [1; 2; 3]   // ((0-1)-2)-3 = -6
let medFoldBack = List.foldBack (-) [1; 2; 3]  0   // 1-(2-(3-0)) = 2


// ============================================================
// DEL 2: SETS
// ============================================================
//
// Et set er en uordnet samling UDEN gentagelser.
// Implementeret som et balanceret binært træ → O(log n) opslag.

let mandligeKlubmedlemmer = Set.ofList ["Bob"; "Bill"; "Ben"; "Bill"]
// Duplikater fjernes automatisk, og elementerne sorteres
// mandligeKlubmedlemmer = set ["Ben"; "Bill"; "Bob"]

let bestyrelse = Set.ofList ["Alice"; "Bill"; "Ann"]

// Union: alle der er i mindst ét af de to sets
let alleInvolverede = Set.union mandligeKlubmedlemmer bestyrelse
// = set ["Alice"; "Ann"; "Ben"; "Bill"; "Bob"]

// Intersection: kun dem der er i BEGGE sets
let mandligeBestyrelsesmedlemmer = Set.intersect mandligeKlubmedlemmer bestyrelse
// = set ["Bill"]

// Difference: dem der er i første men IKKE i andet
let mandligeMedlemmerUdenBestyrelse = Set.difference mandligeKlubmedlemmer bestyrelse
// = set ["Ben"; "Bob"]

// Membership og subset-tjek
let erAliceEtMandligtMedlem = Set.contains "Alice" mandligeKlubmedlemmer
// false

let erDetteTjekKorrekt = Set.isSubset (set ["Ben"; "Bill"]) mandligeKlubmedlemmer
// true

// Set.map – virker ligesom List.map men på sets
let storeNavne = Set.map (fun s -> s.ToUpper()) mandligeKlubmedlemmer
// = set ["BEN"; "BILL"; "BOB"]

// Set.filter
let kortNavne = Set.filter (fun s -> s.Length <= 3) mandligeKlubmedlemmer
// = set ["Ben"; "Bob"]

// Set.fold – summer kardinaliteterne af en mængde af mængder
let setAfSets = set [set [1;3;5]; set [2;4]; set [7;8;9]]

let totalElementer = Set.fold (fun acc s -> acc + Set.count s) 0 setAfSets
// (0 + 3) + 2 + 3 = 8


// ============================================================
// DEL 3: MAPS
// ============================================================
//
// Et map er en endelig funktion fra nøgler til værdier.
// Tænk på det som en dictionary/hashtabel – men immutable.
// Implementeret som balanceret binært træ → O(log n) opslag.

// Byg et map fra en liste af (nøgle, værdi) par
let produktRegister =
    Map.ofList [
        ("A001", ("Rugbrød",    18))
        ("A002", ("Smør",       25))
        ("A003", ("Leverpostej", 22))
        ("A004", ("Mælk",       10))
    ]
// Map<string, string * int>
// Nøgle = artikelkode, Værdi = (navn, pris i kr)

// Slå en nøgle op – tryFind returnerer Some eller None (crasher ikke)
let opslag = Map.tryFind "A002" produktRegister
// opslag = Some ("Smør", 25)

let ukendt = Map.tryFind "X999" produktRegister
// ukendt = None

// find crasher med exception hvis nøglen ikke findes
// brug kun find når du er SIKKER på nøglen eksisterer
let (smorNavn, smorPris) = Map.find "A002" produktRegister
// smorNavn = "Smør", smorPris = 25

// Tilføj en ny vare (returnerer et NYT map – originalen ændres ikke)
let udvidetRegister = Map.add "A005" ("Ost", 45) produktRegister

// Slet en vare
let reducertRegister = Map.remove "A004" produktRegister

// Tjek om en nøgle eksisterer
let findesmælk = Map.containsKey "A004" produktRegister
// true

// Map.map – anvend en funktion på alle VÆRDIER
// Her: giv 10% rabat på alle priser
let rabatRegister =
    Map.map (fun _kode (navn, pris) -> (navn, int (float pris * 0.9))) produktRegister
// A001 -> ("Rugbrød", 16), A002 -> ("Smør", 22), osv.

// Map.filter – behold kun billige varer (pris < 20)
let billigeVarer =
    Map.filter (fun _kode (_navn, pris) -> pris < 20) produktRegister
// = map [("A001", ("Rugbrød", 18)); ("A004", ("Mælk", 10))]

// Map.fold – beregn den samlede varebeholdningsværdi
let samletPris =
    Map.fold (fun acc _kode (_navn, pris) -> acc + pris) 0 produktRegister
// 18 + 25 + 22 + 10 = 75


// ============================================================
// DEL 4: SAMLET EKSEMPEL – simpel kasse
// ============================================================
//
// En kunde køber varer. Vi slår priser op i registret og
// beregner den samlede bon.

type ArticleCode = string
type ArticleName = string
type Price       = int
type NoPieces    = int

type Register = Map<ArticleCode, ArticleName * Price>
type Purchase = (NoPieces * ArticleCode) list
type BillLine = NoPieces * ArticleName * Price
type Bill     = BillLine list * Price   // (linjer, totalsum)

let reg : Register =
    Map.ofList [
        ("A001", ("Rugbrød",    18))
        ("A002", ("Smør",       25))
        ("A003", ("Leverpostej", 22))
    ]

// makeBill bruger List.foldBack til at bygge bonen op
// foldBack fordi vi vil have linjerne i SAMME rækkefølge som købet
let makeBill (reg: Register) (pur: Purchase) : Bill =
    let f (antal, kode) (linjer, total) =
        match Map.tryFind kode reg with
        | Some (navn, pris) ->
            let linjePris = antal * pris
            ((antal, navn, linjePris) :: linjer, total + linjePris)
        | None ->
            failwith (sprintf "Ukendt artikelkode: %s" kode)
    List.foldBack f pur ([], 0)

let indkoeb : Purchase = [(2, "A001"); (1, "A002"); (3, "A003")]

let (bonLinjer, total) = makeBill reg indkoeb
// bonLinjer = [
//   (2, "Rugbrød",    36)   // 2 * 18
//   (1, "Smør",       25)   // 1 * 25
//   (3, "Leverpostej", 66)  // 3 * 22
// ]
// total = 127